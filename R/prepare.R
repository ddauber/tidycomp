#' Prepare data with explicit, logged steps
#'
#' @param spec A `comp_spec`.
#' @param steps A list of step calls (see e.g. [step_trim_outliers()]).
#' @param lock Logical, if TRUE, attaches a digest for reproducibility checks (future).
#' @export
prepare <- function(spec, steps = list(), lock = FALSE) {
  stopifnot(inherits(spec, "comp_spec"))
  df <- spec$data_raw
  prep_log <- tibble::tibble()
  if (!length(steps)) {
    cli::cli_inform("No preparation steps supplied; using raw data.")
    spec$data_prepared <- df
    spec$prep_log <- prep_log
    return(spec)
  }

  for (st in steps) {
    res <- st(df = df, spec = spec) # each step returns list(df=, log_row=)
    df <- res$df
    prep_log <- dplyr::bind_rows(prep_log, res$log)
  }

  spec$data_prepared <- tibble::as_tibble(df)
  spec$prep_log <- prep_log
  cli::cli_inform("Preparation applied: {nrow(prep_log)} step{?s}.")
  spec
}

#' Step: trim or winsorize outliers (IQR rule)
#'
#' @param var A tidy-eval column (numeric outcome).
#' @param method Currently only "iqr".
#' @param k Multiplier for IQR (default 3).
#' @param action "remove" or "winsorize".
#' @export
step_trim_outliers <- function(
  var,
  method = c("iqr"),
  k = 3,
  action = c("remove", "winsorize")
) {
  method <- rlang::arg_match(method)
  action <- rlang::arg_match(action)
  var_quo <- rlang::enquo(var)

  function(df, spec) {
    v <- rlang::as_name(var_quo)
    .validate_cols(df, v)
    if (!is.numeric(df[[v]])) {
      cli::cli_abort("`step_trim_outliers()` requires a numeric variable.")
    }

    lim <- .flag_outliers_iqr(df[[v]], k = k)
    n_before <- nrow(df)

    if (action == "remove") {
      idx <- which(df[[v]] < lim$lo | df[[v]] > lim$hi)
      df2 <- df[-idx, , drop = FALSE]
      n_after <- nrow(df2)
      changed <- length(idx)
      msg <- "removed"
    } else {
      df2 <- df
      df2[[v]] <- pmax(pmin(df2[[v]], lim$hi), lim$lo)
      n_after <- nrow(df2)
      changed <- sum(df[[v]] != df2[[v]], na.rm = TRUE)
      msg <- "winsorized"
    }

    cli::cli_inform("Outlier step on `{v}` ({msg} {changed}).")

    log_row <- tibble::tibble(
      step = "trim_outliers",
      var = v,
      method = method,
      k = k,
      action = action,
      n_before = n_before,
      n_after = n_after,
      n_changed = changed
    )
    list(df = df2, log = log_row)
  }
}
