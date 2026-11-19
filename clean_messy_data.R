# =============================================================================
# clean_data(): automated, configurable data-cleaning pipeline for R
# -----------------------------------------------------------------------------
# Dependencies: none (base R only). Works on data.frame / tibble / data.table
# input; always returns a plain data.frame with a "cleaning_log" attribute.
#
# Pipeline (each step can be switched on/off):
#   1. Standardize column names (snake_case, unique, syntactic)
#   2. Trim whitespace + collapse internal spaces in text columns
#   3. Convert NA-like tokens ("", "NA", "N/A", "null", "-", ...) to NA
#   4. Drop fully empty rows and columns
#   5. Drop constant (zero-variance) columns           [optional]
#   6. Auto-detect and convert types: numeric (handles $ , % ( )),
#      logical (yes/no/true/false/y/n/1/0), dates
#   7. Standardize text case                            [optional]
#   8. Remove duplicate rows (all columns or a key)
#   9. Handle missing values: none / drop rows / impute / drop sparse columns
#  10. Handle outliers: none / flag / cap (IQR or percentile) / remove
#  11. Convert low-cardinality text columns to factors  [optional]
#
# Usage:
#   source("clean_data.R")
#   clean <- clean_data(raw_df)
#   print_cleaning_log(clean)
# =============================================================================

clean_data <- function(df,
                       standardize_names   = TRUE,
                       trim_whitespace     = TRUE,
                       na_strings          = c("", "na", "n/a", "nan", "null",
                                               "none", "nil", "-", "--", "?",
                                               "missing", "#n/a", "#value!"),
                       remove_empty        = TRUE,
                       remove_constant     = FALSE,
                       convert_types       = TRUE,
                       numeric_threshold   = 0.95,
                       keep_as_text        = NULL,
                       date_formats        = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y",
                                               "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y",
                                               "%b %d %Y", "%d %b %Y", "%B %d, %Y"),
                       text_case           = c("none", "lower", "upper", "title"),
                       remove_duplicates   = TRUE,
                       duplicate_keys      = NULL,
                       missing_strategy    = c("none", "drop_rows", "impute"),
                       numeric_impute      = c("median", "mean", "zero"),
                       categorical_impute  = c("mode", "unknown"),
                       max_missing_col_pct = NULL,
                       outlier_strategy    = c("none", "flag", "cap", "remove"),
                       outlier_method      = c("iqr", "percentile"),
                       iqr_multiplier      = 1.5,
                       percentile_bounds   = c(0.01, 0.99),
                       outlier_exclude     = NULL,
                       factor_max_levels   = NULL,
                       verbose             = TRUE) {

  # ---- Argument validation ---------------------------------------------------
  if (!is.data.frame(df)) stop("`df` must be a data.frame (or tibble/data.table).")
  text_case          <- match.arg(text_case)
  missing_strategy   <- match.arg(missing_strategy)
  numeric_impute     <- match.arg(numeric_impute)
  categorical_impute <- match.arg(categorical_impute)
  outlier_strategy   <- match.arg(outlier_strategy)
  outlier_method     <- match.arg(outlier_method)
  if (numeric_threshold <= 0 || numeric_threshold > 1)
    stop("`numeric_threshold` must be in (0, 1].")
  if (!is.null(max_missing_col_pct) &&
      (max_missing_col_pct < 0 || max_missing_col_pct > 1))
    stop("`max_missing_col_pct` must be between 0 and 1.")

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # Factors -> character so text cleaning applies uniformly
  fac_cols <- vapply(df, is.factor, logical(1))
  df[fac_cols] <- lapply(df[fac_cols], as.character)

  # ---- Logging helpers -------------------------------------------------------
  log <- list(start_dim = dim(df), steps = list())
  add_log <- function(step, detail) {
    log$steps[[length(log$steps) + 1]] <<- list(step = step, detail = detail)
    if (verbose) message(sprintf("[clean_data] %-22s %s", step, detail))
  }
  is_text <- function(x) is.character(x)

  # ---- 1. Standardize column names ------------------------------------------
  if (standardize_names) {
    old <- names(df)
    new <- old
    new <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", new)       # camelCase -> camel_Case
    new <- tolower(new)
    new <- gsub("%", "_pct", new, fixed = TRUE)
    new <- gsub("#", "_num", new, fixed = TRUE)
    new <- gsub("[^a-z0-9]+", "_", new)                     # non-alnum -> _
    new <- gsub("^_+|_+$", "", new)                         # trim underscores
    new[new == ""] <- "col"
    new <- ifelse(grepl("^[0-9]", new), paste0("x", new), new)
    new <- make.unique(new, sep = "_")
    names(df) <- new
    changed <- sum(old != new)
    add_log("standardize_names", sprintf("%d column name(s) changed", changed))
    log$name_map <- data.frame(original = old, cleaned = new, stringsAsFactors = FALSE)
  }

  # ---- 2. Trim / squish whitespace ------------------------------------------
  if (trim_whitespace) {
    txt <- names(df)[vapply(df, is_text, logical(1))]
    for (col in txt) {
      x <- df[[col]]
      x <- gsub("[ \t\r\n]+", " ", x)                  # nbsp/tabs/newlines
      x <- gsub("\\s+", " ", trimws(x))
      df[[col]] <- x
    }
    add_log("trim_whitespace", sprintf("%d text column(s) trimmed", length(txt)))
  }

  # ---- 3. NA-like tokens -> NA ----------------------------------------------
  if (length(na_strings)) {
    na_lc <- tolower(na_strings)
    n_replaced <- 0L
    for (col in names(df)[vapply(df, is_text, logical(1))]) {
      hit <- !is.na(df[[col]]) & tolower(trimws(df[[col]])) %in% na_lc
      n_replaced <- n_replaced + sum(hit)
      df[[col]][hit] <- NA
    }
    add_log("standardize_na", sprintf("%d NA-like value(s) set to NA", n_replaced))
  }

  # ---- 4. Remove empty rows / columns ---------------------------------------
  if (remove_empty && ncol(df) > 0) {
    empty_cols <- names(df)[vapply(df, function(x) all(is.na(x)), logical(1))]
    if (length(empty_cols)) df <- df[, setdiff(names(df), empty_cols), drop = FALSE]
    empty_rows <- if (ncol(df)) rowSums(!is.na(df)) == 0 else logical(0)
    df <- df[!empty_rows, , drop = FALSE]
    add_log("remove_empty", sprintf("%d empty column(s), %d empty row(s) removed",
                                    length(empty_cols), sum(empty_rows)))
    log$removed_empty_cols <- empty_cols
  }

  # ---- 5. Remove constant columns -------------------------------------------
  if (remove_constant && ncol(df) > 0) {
    const_cols <- names(df)[vapply(df, function(x)
      length(unique(x[!is.na(x)])) <= 1, logical(1))]
    if (length(const_cols)) df <- df[, setdiff(names(df), const_cols), drop = FALSE]
    add_log("remove_constant", sprintf("%d constant column(s) removed: %s",
                                       length(const_cols),
                                       paste(const_cols, collapse = ", ")))
  }

  # ---- 6. Type detection & conversion ---------------------------------------
  if (convert_types) {
    conversions <- character(0)

    parse_number <- function(x) {
      neg <- grepl("^\\(.*\\)$", x)                         # (123) accounting negatives
      y <- gsub("[$£€,%()\\s]", "", x, perl = TRUE)
      y <- suppressWarnings(as.numeric(y))
      y[neg & !is.na(y)] <- -y[neg & !is.na(y)]
      y
    }
    try_dates <- function(x) {
      best <- NULL; best_ok <- 0
      for (fmt in date_formats) {
        d <- as.Date(x, format = fmt)
        ok <- mean(!is.na(d[!is.na(x)]))
        if (ok > best_ok) { best <- d; best_ok <- ok }
        if (ok == 1) break
      }
      list(value = best, rate = best_ok)
    }
    true_vals  <- c("true", "t", "yes", "y", "1")
    false_vals <- c("false", "f", "no", "n", "0")

    for (col in names(df)) {
      x <- df[[col]]
      if (!is_text(x) || col %in% keep_as_text) next
      nn <- x[!is.na(x)]
      if (!length(nn)) next
      lx <- tolower(nn)

      # Protect ID-like codes with leading zeros (zip codes, "001") from numeric
      if (any(grepl("^0[0-9]+$", nn))) {
        conversions <- c(conversions, sprintf("%s kept as text (leading zeros)", col))
        next
      }

      # Logical: every non-NA value is a recognized boolean token
      # (pure 0/1 columns are left for the numeric check)
      if (all(lx %in% c(true_vals, false_vals)) && !all(lx %in% c("0", "1"))) {
        df[[col]] <- ifelse(is.na(x), NA, tolower(x) %in% true_vals)
        conversions <- c(conversions, sprintf("%s -> logical", col)); next
      }
      # Numeric: share of parseable values >= threshold
      num <- parse_number(x)
      if (mean(!is.na(num[!is.na(x)])) >= numeric_threshold) {
        bad <- sum(!is.na(x) & is.na(num))
        df[[col]] <- num
        conversions <- c(conversions, sprintf("%s -> numeric%s", col,
                         if (bad) sprintf(" (%d unparseable -> NA)", bad) else ""))
        next
      }
      # Date
      dt <- try_dates(x)
      if (!is.null(dt$value) && dt$rate >= numeric_threshold) {
        df[[col]] <- dt$value
        conversions <- c(conversions, sprintf("%s -> Date", col)); next
      }
    }
    add_log("convert_types", if (length(conversions))
      paste(conversions, collapse = "; ") else "no conversions")
  }

  # ---- 7. Text case ----------------------------------------------------------
  if (text_case != "none") {
    to_title <- function(s) gsub("\\b([a-z])", "\\U\\1", tolower(s), perl = TRUE)
    f <- switch(text_case, lower = tolower, upper = toupper, title = to_title)
    txt <- names(df)[vapply(df, is_text, logical(1))]
    df[txt] <- lapply(df[txt], f)
    add_log("text_case", sprintf("'%s' applied to %d column(s)", text_case, length(txt)))
  }

  # ---- 8. Duplicates ---------------------------------------------------------
  if (remove_duplicates && nrow(df) > 0) {
    keys <- if (is.null(duplicate_keys)) names(df) else duplicate_keys
    missing_keys <- setdiff(keys, names(df))
    if (length(missing_keys))
      stop("duplicate_keys not found after cleaning: ",
           paste(missing_keys, collapse = ", "),
           " (names are standardized first; check log$name_map)")
    dup <- duplicated(df[, keys, drop = FALSE])
    df <- df[!dup, , drop = FALSE]
    add_log("remove_duplicates", sprintf("%d duplicate row(s) removed (keys: %s)",
            sum(dup), if (is.null(duplicate_keys)) "all columns"
                      else paste(keys, collapse = ", ")))
  }

  # ---- 9. Missing values -----------------------------------------------------
  log$missing_before <- colSums(is.na(df))

  if (!is.null(max_missing_col_pct) && ncol(df) > 0) {
    pct <- colMeans(is.na(df))
    sparse <- names(pct)[pct > max_missing_col_pct]
    if (length(sparse)) df <- df[, setdiff(names(df), sparse), drop = FALSE]
    add_log("drop_sparse_cols", sprintf("%d column(s) > %.0f%% missing removed%s",
            length(sparse), max_missing_col_pct * 100,
            if (length(sparse)) paste0(": ", paste(sparse, collapse = ", ")) else ""))
  }

  if (missing_strategy == "drop_rows") {
    keep <- complete.cases(df)
    add_log("missing_values", sprintf("%d row(s) with NA dropped", sum(!keep)))
    df <- df[keep, , drop = FALSE]
  } else if (missing_strategy == "impute") {
    mode_of <- function(x) {
      ux <- x[!is.na(x)]
      if (!length(ux)) return(NA)
      tab <- table(ux); names(tab)[which.max(tab)]
    }
    imputed <- character(0)
    for (col in names(df)) {
      x <- df[[col]]; n_na <- sum(is.na(x))
      if (!n_na || all(is.na(x))) next
      if (is.numeric(x)) {
        fill <- switch(numeric_impute,
                       median = stats::median(x, na.rm = TRUE),
                       mean   = mean(x, na.rm = TRUE),
                       zero   = 0)
        x[is.na(x)] <- fill
      } else if (is_text(x)) {
        x[is.na(x)] <- if (categorical_impute == "mode") mode_of(x) else "Unknown"
      } else if (is.logical(x)) {
        x[is.na(x)] <- as.logical(mode_of(x))
      } else next                                            # leave Dates as NA
      df[[col]] <- x
      imputed <- c(imputed, sprintf("%s(%d)", col, n_na))
    }
    add_log("missing_values", if (length(imputed))
      paste("imputed:", paste(imputed, collapse = ", ")) else "nothing to impute")
  }

  # ---- 10. Outliers ----------------------------------------------------------
  if (outlier_strategy != "none") {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    num_cols <- setdiff(num_cols, outlier_exclude)
    any_outlier <- rep(FALSE, nrow(df))
    summary_out <- character(0)

    for (col in num_cols) {
      x <- df[[col]]
      if (sum(!is.na(x)) < 4) next
      if (outlier_method == "iqr") {
        q <- stats::quantile(x, c(.25, .75), na.rm = TRUE)
        lo <- q[1] - iqr_multiplier * diff(q); hi <- q[2] + iqr_multiplier * diff(q)
      } else {
        b <- stats::quantile(x, percentile_bounds, na.rm = TRUE)
        lo <- b[1]; hi <- b[2]
      }
      is_out <- !is.na(x) & (x < lo | x > hi)
      if (!any(is_out)) next
      summary_out <- c(summary_out, sprintf("%s(%d)", col, sum(is_out)))

      if (outlier_strategy == "cap") {
        x[!is.na(x) & x < lo] <- lo; x[!is.na(x) & x > hi] <- hi
        df[[col]] <- unname(x)
      } else if (outlier_strategy == "flag") {
        df[[paste0(col, "_outlier")]] <- is_out
      }
      any_outlier <- any_outlier | is_out
    }
    if (outlier_strategy == "remove") df <- df[!any_outlier, , drop = FALSE]
    add_log("outliers", sprintf("%s via %s: %s", outlier_strategy, outlier_method,
            if (length(summary_out)) paste(summary_out, collapse = ", ") else "none found"))
  }

  # ---- 11. Factors -----------------------------------------------------------
  if (!is.null(factor_max_levels)) {
    txt <- names(df)[vapply(df, is_text, logical(1))]
    to_fac <- txt[vapply(df[txt], function(x) {
      nn <- x[!is.na(x)]; k <- length(unique(nn))
      k <= factor_max_levels && k < length(nn)              # skip all-unique IDs
    }, logical(1))]
    df[to_fac] <- lapply(df[to_fac], factor)
    add_log("factors", sprintf("%d column(s) -> factor%s", length(to_fac),
            if (length(to_fac)) paste0(": ", paste(to_fac, collapse = ", ")) else ""))
  }

  # ---- Finish ----------------------------------------------------------------
  rownames(df) <- NULL
  log$end_dim       <- dim(df)
  log$missing_after <- colSums(is.na(df))
  log$col_types     <- vapply(df, function(x) class(x)[1], character(1))
  attr(df, "cleaning_log") <- log
  if (verbose) message(sprintf("[clean_data] done: %d x %d -> %d x %d",
                               log$start_dim[1], log$start_dim[2],
                               log$end_dim[1], log$end_dim[2]))
  df
}

# -----------------------------------------------------------------------------
# print_cleaning_log(): readable summary of what clean_data() did
# -----------------------------------------------------------------------------
print_cleaning_log <- function(df) {
  log <- attr(df, "cleaning_log")
  if (is.null(log)) stop("No cleaning_log attribute; run clean_data() first.")
  cat("== Data cleaning report ==\n")
  cat(sprintf("Rows: %d -> %d | Columns: %d -> %d\n\n",
              log$start_dim[1], log$end_dim[1], log$start_dim[2], log$end_dim[2]))
  for (s in log$steps) cat(sprintf("- %-20s %s\n", s$step, s$detail))
  cat("\nColumn types:\n")
  print(log$col_types)
  cat("\nRemaining NAs per column:\n")
  print(log$missing_after[log$missing_after > 0])
  invisible(log)
}

# -----------------------------------------------------------------------------
# Example (runs only when this file is executed directly with Rscript)
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  raw <- data.frame(
    `Customer ID`   = c("001", "002", "002", "003", "004", "005", " ", "006"),
    `First Name`    = c("  alice ", "BOB", "BOB", "carol", "N/A", "dave", "", "erin"),
    signupDate      = c("2024-01-05", "2024-02-11", "2024-02-11", "2024-03-15",
                        "null", "2024-05-20", "", "2024-06-01"),
    `Revenue ($)`   = c("$1,200.50", "850", "850", "(300)", "-", "999999", "", "1,100"),
    Active          = c("Yes", "no", "no", "Y", "N", "yes", "", "n"),
    `Discount %`    = c("10%", "5%", "5%", "", "15%", "20%", "", "10%"),
    Region          = c("East", "west", "west", "EAST", "North", NA, "", "east"),
    EmptyCol        = NA,
    check.names = FALSE, stringsAsFactors = FALSE
  )

  clean <- clean_data(
    raw,
    text_case         = "title",
    missing_strategy  = "impute",
    outlier_strategy  = "cap",
    factor_max_levels = 10,
    duplicate_keys    = "customer_id"
  )
  print(clean)
  str(clean)
  print_cleaning_log(clean)
}
