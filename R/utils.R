#' Internal utilities (validation and helpers)
#' @keywords internal

#' Validate that required columns exist
#'
#' Checks that all specified column names are present in a data frame;
#' aborts with a helpful message if any are missing.
#'
#' @param data A data frame.
#' @param cols Character vector of required column names.
#' @keywords internal
#' @noRd
.validate_cols <- function(data, cols) {
  miss <- setdiff(cols, names(data))
  if (length(miss)) {
    cli::cli_abort(c(
      "Missing required column{?s}: {toString(miss)}.",
      "i" = "Use `set_roles()` with existing columns."
    ))
  }
}

#' Capture a role column from tidy-eval input
#'
#' Quosures and expressions are validated to ensure they are a single,
#' existing column name in `data`.
#'
#' @param data A data frame.
#' @param x A tidy-eval column reference.
#' @return A character scalar with the resolved column name.
#' @keywords internal
#' @noRd
.capture_role <- function(data, x) {
  q <- rlang::enquo(x)
  if (rlang::quo_is_missing(q) || rlang::is_null(rlang::quo_get_expr(q))) {
    cli::cli_abort("A valid, unquoted column name is required.")
  }
  nm <- rlang::as_name(q)
  if (!nzchar(nm)) {
    cli::cli_abort("A valid, unquoted column name is required.")
  }
  if (!nm %in% names(data)) {
    cli::cli_abort("Column `{nm}` not found in `data`.")
  }
  nm
}

#' Standardize two-group numeric data
#'
#' Select and rename the outcome and group columns, validate type/levels,
#' and coerce group to a factor with exactly two levels.
#'
#' @param data A data frame.
#' @param outcome,group Character names of validated columns.
#' @return A tibble with columns `outcome` (numeric) and `group` (factor).
#' @keywords internal
#' @noRd
.standardize_two_group_numeric <- function(data, outcome, group) {
  df <- tibble::as_tibble(data[, c(outcome, group)])
  names(df) <- c("outcome", "group")
  if (!is.numeric(df$outcome)) {
    cli::cli_abort("Outcome must be numeric for the current engine.")
  }
  if (!is.factor(df$group)) {
    df$group <- factor(df$group)
  }
  if (nlevels(df$group) != 2) {
    cli::cli_abort("Group must have exactly 2 levels for this engine.")
  }
  df
}

#' Standardize multi-group numeric data
#'
#' Select and rename the outcome and group columns, ensure a numeric
#' outcome, and coerce the grouping variable to a factor with at least
#' two levels.
#'
#' @param data A data frame.
#' @param outcome,group Character names of validated columns.
#' @return A tibble with columns `outcome` (numeric) and `group` (factor).
#' @keywords internal
#' @noRd
.standardize_multi_group_numeric <- function(data, outcome, group) {
  df <- tibble::as_tibble(data[, c(outcome, group)])
  names(df) <- c("outcome", "group")
  if (!is.numeric(df$outcome)) {
    cli::cli_abort("Outcome must be numeric for the current engine.")
  }
  if (!is.factor(df$group)) {
    df$group <- factor(df$group)
  }
  if (nlevels(df$group) < 2) {
    cli::cli_abort("Group must have at least 2 levels for this engine.")
  }
  df
}

#' Standardize paired numeric data with id
#'
#' Select outcome, group, and id columns, validate pairing structure, and
#' return a wide tibble with one column per group ordered by id.
#'
#' @param data A data frame.
#' @param outcome,group,id Character names of validated columns.
#' @return A tibble with two numeric columns named after group levels.
#' @keywords internal
#' @noRd
.standardize_paired_numeric <- function(data, outcome, group, id) {
  df <- tibble::as_tibble(data[, c(outcome, group, id)])
  names(df) <- c("outcome", "group", "id")
  if (!is.numeric(df$outcome)) {
    cli::cli_abort("Outcome must be numeric for the current engine.")
  }
  if (is.null(id)) {
    cli::cli_abort("Paired design requires an `id` role.")
  }
  if (!is.factor(df$group)) {
    df$group <- factor(df$group)
  }
  if (nlevels(df$group) != 2) {
    cli::cli_abort("Group must have exactly 2 levels for this engine.")
  }
  counts <- table(df$id)
  if (any(counts != 2)) {
    cli::cli_abort("Each id must appear exactly twice (once per group).")
  }
  chk <- dplyr::count(df, id, group)
  if (any(chk$n != 1)) {
    cli::cli_abort("Each id must have one observation for each group.")
  }
  wide <- tidyr::pivot_wider(
    df,
    id_cols = "id",
    names_from = "group",
    values_from = "outcome"
  )
  if (any(!stats::complete.cases(wide))) {
    cli::cli_abort("Missing outcomes for at least one id.")
  }
  tibble::as_tibble(wide[, setdiff(names(wide), "id"), drop = FALSE])
}

#' Standardize repeated-measures numeric data
#'
#' Select outcome, group, and id columns and validate that each id has
#' one observation per group. Returns the data in long format; engines may
#' reshape as needed.
#'
#' @param data A data frame.
#' @param outcome,group,id Character names of validated columns.
#' @return A tibble with columns `outcome`, `group`, and `id`.
#' @keywords internal
#' @noRd
.standardize_repeated_numeric <- function(data, outcome, group, id) {
  df <- tibble::as_tibble(data[, c(outcome, group, id)])
  names(df) <- c("outcome", "group", "id")
  if (!is.numeric(df$outcome)) {
    cli::cli_abort("Outcome must be numeric for the current engine.")
  }
  if (is.null(id)) {
    cli::cli_abort("Repeated measures design requires an `id` role.")
  }
  if (!is.factor(df$group)) {
    df$group <- factor(df$group)
  }
  if (nlevels(df$group) < 2) {
    cli::cli_abort("Group must have at least 2 levels for this engine.")
  }
  chk <- dplyr::count(df, id, group)
  if (any(chk$n != 1)) {
    cli::cli_abort("Each id must have one observation for each group.")
  }
  df
}

#' Flag outliers using common rules
#'
#' Compute lower/upper fences for finite values of `x` based on one of
#' three rules: interquartile range (IQR), median absolute deviation (MAD),
#' or standard deviation (SD). The default `k = 3` aligns with the
#' recommendations in the [*r4np* book](https://r4np.com/09_sources_of_bias.html#sec-dealing-with-outliers).
#'
#' @param x Numeric vector.
#' @param method Outlier rule; one of `"iqr"`, `"mad"`, or `"sd"`.
#' @param k Multiplier for the chosen scale (default 3).
#' @return A list with elements `lo` and `hi`.
#' @keywords internal
#' @noRd
.flag_outliers <- function(x, method = c("iqr", "mad", "sd"), k = 3) {
  method <- match.arg(method)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(list(lo = NA_real_, hi = NA_real_))
  }
  if (method == "iqr") {
    q <- stats::quantile(x, probs = c(.25, .75), na.rm = TRUE, names = FALSE)
    scale <- diff(q)
    lo <- q[1] - k * scale
    hi <- q[2] + k * scale
  } else if (method == "mad") {
    center <- stats::median(x, na.rm = TRUE)
    scale <- stats::median(abs(x - center), na.rm = TRUE)
    lo <- center - k * scale
    hi <- center + k * scale
  } else {
    center <- mean(x, na.rm = TRUE)
    scale <- stats::sd(x, na.rm = TRUE)
    lo <- center - k * scale
    hi <- center + k * scale
  }
  list(lo = lo, hi = hi)
}

#' Brown–Forsythe test (median-based Levene)
#'
#' Median-based Levene test for equality of variance across two or more
#' groups. Returns a p-value from a one-way ANOVA on absolute deviations
#' from group medians.
#'
#' @param y Numeric outcome.
#' @param g Grouping variable (coerced to factor).
#' @return Numeric p-value, or `NA` if fewer than two groups are present.
#' @keywords internal
#' @noRd
.brown_forsythe <- function(y, g) {
  g <- factor(g)
  if (nlevels(g) < 2) {
    return(NA_real_)
  }
  med <- tapply(y, g, stats::median, na.rm = TRUE)
  z <- abs(y - med[match(g, levels(g))])
  fit <- stats::aov(z ~ g)
  an <- summary(fit)[[1]]
  an["g", "Pr(>F)"]
}


#' Check if the `effectsize` package is installed
#'
#' This internal helper is used to determine whether the \pkg{effectsize}
#' package is available in the current R session. It wraps
#' [base::requireNamespace()] with `quietly = TRUE` and returns `TRUE`
#' if the package can be loaded, `FALSE` otherwise.
#'
#' This wrapper is useful for testing purposes as well.
#'
#' @return A logical scalar:
#'   * `TRUE` if the \pkg{effectsize} package is installed and loadable.
#'   * `FALSE` if it is not installed.
#'
#' @examples
#' tidycomp:::.has_effectsize()
#'
#' @keywords internal
.has_effectsize <- function() {
  requireNamespace("effectsize", quietly = TRUE)
}
