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

#' Flag outliers using the IQR rule
#'
#' Compute lower/upper fences for finite values of `x` based on the IQR rule.
#'
#' @param x Numeric vector.
#' @param k Multiplier for the IQR (default 3).
#' @return A list with elements `lo` and `hi`.
#' @keywords internal
#' @noRd
.flag_outliers_iqr <- function(x, k = 3) {
  x <- x[is.finite(x)]
  q <- stats::quantile(x, probs = c(.25, .75), na.rm = TRUE, names = FALSE)
  iqr <- diff(q)
  lo <- q[1] - k * iqr
  hi <- q[2] + k * iqr
  list(lo = lo, hi = hi)
}

#' Brown–Forsythe test (2 groups, minimal)
#'
#' Median-based Levene test for equality of variance in two groups.
#' Implemented via a classic two-sample t-test on absolute deviations
#' from group medians. Returns a p-value.
#'
#' @param y Numeric outcome.
#' @param g Grouping variable (coerced to factor).
#' @return Numeric p-value, or `NA` if not applicable.
#' @keywords internal
#' @noRd
.brown_forsythe_2g <- function(y, g) {
  g <- factor(g)
  if (nlevels(g) != 2) {
    return(NA_real_)
  }
  med <- tapply(y, g, stats::median, na.rm = TRUE)
  z <- abs(y - med[match(g, levels(g))])
  # one-way ANOVA on z ~ g
  # minimal implementation:
  z1 <- z[g == levels(g)[1]]
  z2 <- z[g == levels(g)[2]]
  # classic two-sample t on z (proxy); small sample caveat
  res <- stats::t.test(z1, z2, var.equal = TRUE)
  res$p.value
}
