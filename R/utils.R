#' Internal utilities (validation and helpers)
#' @keywords internal

#' Validate columns exist
#' @param data A data frame
#' @param cols Character vector of required column names
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

#' Capture column names from tidy-eval inputs
#' @noRd
.capture_role <- function(data, x) {
  q <- rlang::enquo(x)
  nm <- rlang::as_name(q)
  if (!nzchar(nm) || rlang::quo_is_missing(q)) {
    cli::cli_abort("A valid, unquoted column name is required.")
  }
  if (!nm %in% names(data)) {
    cli::cli_abort("Column `{nm}` not found in `data`.")
  }
  nm
}

#' Standardize two-group numeric data for engines
#' @param data data frame
#' @param outcome,group character column names (already validated)
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

#' Simple outlier flag (IQR rule)
#' @noRd
.flag_outliers_iqr <- function(x, k = 3) {
  x <- x[is.finite(x)]
  q <- stats::quantile(x, probs = c(.25, .75), na.rm = TRUE, names = FALSE)
  iqr <- diff(q)
  lo <- q[1] - k * iqr
  hi <- q[2] + k * iqr
  list(lo = lo, hi = hi)
}

#' Brown–Forsythe (median-based Levene) for 2 groups (minimal)
#' Returns p-value; NA if not applicable.
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
