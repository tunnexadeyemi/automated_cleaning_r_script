# =============================================================================
# clean_data_tidy(): automated, configurable data-cleaning pipeline (tidyverse)
# -----------------------------------------------------------------------------
# Packages: dplyr, tidyr, stringr, readr, lubridate, janitor, purrr, tibble
#   install.packages(c("dplyr","tidyr","stringr","readr","lubridate",
#                      "janitor","purrr","tibble"))
#
# Pipeline (each step can be switched on/off):
#   1. Standardize column names               janitor::clean_names()
#   2. Trim / squish whitespace               stringr::str_squish()
#   3. NA-like tokens -> NA                   dplyr::na_if-style matching
#   4. Drop empty rows/columns                janitor::remove_empty()
#   5. Drop constant columns  [optional]      janitor::remove_constant()
#   6. Auto type conversion                   readr::parse_number(), lubridate
#   7. Text case              [optional]      stringr::str_to_*()
#   8. Remove duplicates (all cols or key)    dplyr::distinct()
#   9. Missing values: drop rows / impute / drop sparse columns
#  10. Outliers: flag / cap / remove (IQR or percentile)
#  11. Low-cardinality text -> factor  [optional]
#
# Returns a tibble with a "cleaning_log" attribute (see print_cleaning_log()).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(janitor)
  library(purrr)
  library(tibble)
})

clean_data_tidy <- function(df,
                            standardize_names   = TRUE,
                            name_case           = "snake",
                            trim_whitespace     = TRUE,
                            na_strings          = c("", "na", "n/a", "nan", "null",
                                                    "none", "nil", "-", "--", "?",
                                                    "missing", "#n/a", "#value!"),
                            remove_empty        = TRUE,
                            remove_constant     = FALSE,
                            convert_types       = TRUE,
                            parse_threshold     = 0.95,
                            date_orders         = c("Ymd", "mdY", "dmY", "bdY",
                                                    "dbY", "BdY"),
                            keep_as_text        = NULL,
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

  # ---- Validation ------------------------------------------------------------
  stopifnot(is.data.frame(df))
  text_case          <- match.arg(text_case)
  missing_strategy   <- match.arg(missing_strategy)
  numeric_impute     <- match.arg(numeric_impute)
  categorical_impute <- match.arg(categorical_impute)
  outlier_strategy   <- match.arg(outlier_strategy)
  outlier_method     <- match.arg(outlier_method)
  if (parse_threshold <= 0 || parse_threshold > 1)
    stop("`parse_threshold` must be in (0, 1].")

  df <- as_tibble(df, .name_repair = "minimal") |>
    mutate(across(where(is.factor), as.character))

  # ---- Logging ---------------------------------------------------------------
  log <- list(start_dim = dim(df), steps = tibble(step = character(), detail = character()))
  add_log <- function(step, detail) {
    log$steps <<- add_row(log$steps, step = step, detail = detail)
    if (verbose) message(sprintf("[clean] %-20s %s", step, detail))
  }
  collapse <- function(x) if (length(x)) str_c(x, collapse = ", ") else "none"

  # ---- 1. Column names -------------------------------------------------------
  if (standardize_names) {
    old <- names(df)
    df  <- clean_names(df, case = name_case)
    log$name_map <- tibble(original = old, cleaned = names(df))
    add_log("standardize_names", sprintf("%d name(s) changed", sum(old != names(df))))
  }

  # ---- 2. Whitespace ---------------------------------------------------------
  if (trim_whitespace) {
    df <- df |> mutate(across(where(is.character),
                              ~ str_squish(str_replace_all(.x, " ", " "))))
    add_log("trim_whitespace", sprintf("%d text column(s)",
                                       sum(map_lgl(df, is.character))))
  }

  # ---- 3. NA-like tokens -----------------------------------------------------
  if (length(na_strings)) {
    na_lc <- str_to_lower(na_strings)
    n_hit <- df |> summarise(across(where(is.character),
                                    ~ sum(str_to_lower(str_trim(.x)) %in% na_lc))) |>
      unlist() |> sum()
    df <- df |> mutate(across(where(is.character),
                              ~ if_else(str_to_lower(str_trim(.x)) %in% na_lc,
                                        NA_character_, .x)))
    add_log("standardize_na", sprintf("%d value(s) set to NA", n_hit))
  }

  # ---- 4/5. Empty & constant -------------------------------------------------
  if (remove_empty) {
    before_cols <- names(df); before_rows <- nrow(df)
    df <- remove_empty(df, which = c("rows", "cols"), quiet = TRUE)
    dropped <- setdiff(before_cols, names(df))
    log$removed_empty_cols <- dropped
    add_log("remove_empty", sprintf("%d col(s) [%s], %d row(s) removed",
            length(dropped), collapse(dropped), before_rows - nrow(df)))
  }
  if (remove_constant) {
    before_cols <- names(df)
    df <- janitor::remove_constant(df, na.rm = TRUE, quiet = TRUE)
    add_log("remove_constant", collapse(setdiff(before_cols, names(df))))
  }

  # ---- 6. Type conversion ----------------------------------------------------
  if (convert_types) {
    true_vals  <- c("true", "t", "yes", "y", "1")
    false_vals <- c("false", "f", "no", "n", "0")

    parse_num_safe <- function(x) {
      neg <- str_detect(x, "^\\(.*\\)$") %in% TRUE          # (300) -> -300
      out <- suppressWarnings(parse_number(x, na = character()))
      out[neg & !is.na(out)] <- -abs(out[neg & !is.na(out)])
      attr(out, "problems") <- NULL
      as.numeric(out)
    }

    convert_col <- function(x, col) {
      if (!is.character(x) || col %in% keep_as_text) return(list(x, NULL))
      nn <- x[!is.na(x)]
      if (!length(nn)) return(list(x, NULL))
      lx <- str_to_lower(nn)

      if (any(str_detect(nn, "^0\\d+$")))                    # IDs / ZIPs
        return(list(x, sprintf("%s kept as text (leading zeros)", col)))

      if (all(lx %in% c(true_vals, false_vals)) && !all(lx %in% c("0", "1")))
        return(list(case_when(is.na(x) ~ NA,
                              str_to_lower(x) %in% true_vals ~ TRUE,
                              TRUE ~ FALSE),
                    sprintf("%s -> logical", col)))

      # Only try numbers when values look numeric (avoids "Order 12" -> 12)
      looks_num <- str_detect(nn, "^[\\s$£€(+-]*[0-9.,]+\\s*%?\\)?$")
      if (mean(looks_num) >= parse_threshold) {
        num <- parse_num_safe(x)
        if (mean(!is.na(num[!is.na(x)])) >= parse_threshold)
          return(list(num, sprintf("%s -> numeric", col)))
      }

      dt <- suppressWarnings(as_date(parse_date_time(x, orders = date_orders,
                                                     quiet = TRUE)))
      if (mean(!is.na(dt[!is.na(x)])) >= parse_threshold)
        return(list(dt, sprintf("%s -> Date", col)))

      list(x, NULL)
    }

    res  <- imap(df, convert_col)
    df   <- as_tibble(map(res, 1))
    msgs <- compact(map(res, 2)) |> unlist(use.names = FALSE)
    add_log("convert_types", if (length(msgs)) str_c(msgs, collapse = "; ") else "none")
  }

  # ---- 7. Text case ----------------------------------------------------------
  if (text_case != "none") {
    f <- switch(text_case, lower = str_to_lower, upper = str_to_upper,
                title = str_to_title)
    txt <- setdiff(names(df)[map_lgl(df, is.character)], keep_as_text)
    df  <- df |> mutate(across(all_of(txt), f))
    add_log("text_case", sprintf("'%s' on %d column(s)", text_case, length(txt)))
  }

  # ---- 8. Duplicates ---------------------------------------------------------
  if (remove_duplicates) {
    n0 <- nrow(df)
    if (is.null(duplicate_keys)) {
      df <- distinct(df)
    } else {
      bad <- setdiff(duplicate_keys, names(df))
      if (length(bad)) stop("duplicate_keys not found (names are cleaned first): ",
                            collapse(bad))
      df <- distinct(df, across(all_of(duplicate_keys)), .keep_all = TRUE)
    }
    add_log("remove_duplicates", sprintf("%d row(s) removed (keys: %s)", n0 - nrow(df),
            if (is.null(duplicate_keys)) "all columns" else collapse(duplicate_keys)))
  }

  # ---- 9. Missing values -----------------------------------------------------
  log$missing_before <- colSums(is.na(df))

  if (!is.null(max_missing_col_pct)) {
    sparse <- names(df)[colMeans(is.na(df)) > max_missing_col_pct]
    df <- select(df, -all_of(sparse))
    add_log("drop_sparse_cols", sprintf(">%.0f%% NA: %s",
                                        max_missing_col_pct * 100, collapse(sparse)))
  }

  if (missing_strategy == "drop_rows") {
    n0 <- nrow(df); df <- drop_na(df)
    add_log("missing_values", sprintf("%d row(s) with NA dropped", n0 - nrow(df)))
  } else if (missing_strategy == "impute") {
    mode_of <- function(x) {
      x <- x[!is.na(x)]
      if (!length(x)) return(x[NA_integer_])
      ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
    }
    na_counts <- colSums(is.na(df))
    num_fill  <- switch(numeric_impute,
                        median = \(x) median(x, na.rm = TRUE),
                        mean   = \(x) mean(x, na.rm = TRUE),
                        zero   = \(x) 0)
    df <- df |>
      mutate(
        across(where(is.numeric),   ~ coalesce(.x, num_fill(.x))),
        across(where(is.character), ~ coalesce(.x,
                 if (categorical_impute == "mode") mode_of(.x) else "Unknown")),
        across(where(is.logical),   ~ coalesce(.x, mode_of(.x)))
      )                                               # Dates intentionally left NA
    filled <- na_counts - colSums(is.na(df))
    filled <- filled[filled > 0]
    add_log("missing_values", sprintf("imputed: %s",
            collapse(sprintf("%s(%d)", names(filled), filled))))
  }

  # ---- 10. Outliers ----------------------------------------------------------
  if (outlier_strategy != "none") {
    num_cols <- setdiff(names(df)[map_lgl(df, is.numeric)], outlier_exclude)

    bounds <- function(x) {
      if (outlier_method == "iqr") {
        q <- quantile(x, c(.25, .75), na.rm = TRUE); r <- diff(q) * iqr_multiplier
        unname(c(q[1] - r, q[2] + r))
      } else unname(quantile(x, percentile_bounds, na.rm = TRUE))
    }
    is_out <- function(x) {
      if (sum(!is.na(x)) < 4) return(rep(FALSE, length(x)))
      b <- bounds(x); !is.na(x) & (x < b[1] | x > b[2])
    }

    flags  <- map(df[num_cols], is_out)
    counts <- map_int(flags, sum); counts <- counts[counts > 0]

    df <- switch(outlier_strategy,
      cap = df |> mutate(across(all_of(num_cols), \(x) {
              if (sum(!is.na(x)) < 4) return(x)
              b <- bounds(x); pmin(pmax(x, b[1]), b[2])
            })),
      flag = df |> mutate(across(all_of(names(counts)), is_out,
                                 .names = "{.col}_outlier")),
      remove = df |> filter(!reduce(flags, `|`, .init = rep(FALSE, nrow(df))))
    )
    add_log("outliers", sprintf("%s via %s: %s", outlier_strategy, outlier_method,
            collapse(sprintf("%s(%d)", names(counts), counts))))
  }

  # ---- 11. Factors -----------------------------------------------------------
  if (!is.null(factor_max_levels)) {
    to_fac <- df |>
      select(where(is.character)) |>
      select(-any_of(keep_as_text)) |>
      keep(\(x) { nn <- na.omit(x); k <- n_distinct(nn)
                  k <= factor_max_levels && k < length(nn) }) |>   # skip unique IDs
      names()
    df <- df |> mutate(across(all_of(to_fac), as.factor))
    add_log("factors", collapse(to_fac))
  }

  # ---- Finish ----------------------------------------------------------------
  log$end_dim       <- dim(df)
  log$missing_after <- colSums(is.na(df))
  log$col_types     <- map_chr(df, \(x) class(x)[1])
  attr(df, "cleaning_log") <- log
  if (verbose) message(sprintf("[clean] done: %d x %d -> %d x %d",
                               log$start_dim[1], log$start_dim[2],
                               nrow(df), ncol(df)))
  df
}

# -----------------------------------------------------------------------------
# print_cleaning_log(): readable report of what clean_data_tidy() did
# -----------------------------------------------------------------------------
print_cleaning_log <- function(df) {
  log <- attr(df, "cleaning_log")
  if (is.null(log)) stop("No cleaning_log attribute; run clean_data_tidy() first.")
  cat(sprintf("== Cleaning report ==\nRows: %d -> %d | Columns: %d -> %d\n\n",
              log$start_dim[1], log$end_dim[1], log$start_dim[2], log$end_dim[2]))
  cat(sprintf("- %-18s %s\n", log$steps$step, log$steps$detail), sep = "")
  cat("\nColumn types:\n"); print(log$col_types)
  na_left <- log$missing_after[log$missing_after > 0]
  cat("\nRemaining NAs:\n"); if (length(na_left)) print(na_left) else cat("none\n")
  invisible(log)
}

# -----------------------------------------------------------------------------
# Example (runs only with: Rscript clean_data_tidy.R)
# -----------------------------------------------------------------------------
if (sys.nframe() == 0) {
  raw <- tibble(
    `Customer ID` = c("001", "002", "002", "003", "004", "005", " ", "006"),
    `First Name`  = c("  alice ", "BOB", "BOB", "carol", "N/A", "dave", "", "erin"),
    signupDate    = c("2024-01-05", "02/11/2024", "02/11/2024", "2024-03-15",
                      "null", "May 20 2024", "", "2024-06-01"),
    `Revenue ($)` = c("$1,200.50", "850", "850", "(300)", "-", "999999", "", "1,100"),
    Active        = c("Yes", "no", "no", "Y", "N", "yes", "", "n"),
    `Discount %`  = c("10%", "5%", "5%", "", "15%", "20%", "", "10%"),
    Region        = c("East", "west", "west", "EAST", "North", NA, "", "east"),
    EmptyCol      = NA
  )

  clean <- clean_data_tidy(
    raw,
    text_case          = "title",
    duplicate_keys     = "customer_id",
    missing_strategy   = "impute",
    categorical_impute = "unknown",
    outlier_strategy   = "cap",
    factor_max_levels  = 10
  )
  print(clean)
  print_cleaning_log(clean)
}
