#' Prepare data with explicit, logged steps
#'
#' Apply a sequence of preparation steps to a `comp_spec`, recording each
#' step in a preparation log. Steps are small functions that take
#' `df` (a data frame) and `spec` and return a list with elements
#' `df` (the updated data) and `log` (a one‑row tibble describing the step).
#'
#' @param spec A `comp_spec` created by [comp_spec()].
#' @param steps A list of step *calls* (e.g., [step_trim_outliers()]) to be
#'   executed in order. Each step must return `list(df = ..., log = tibble)`.
#' @param lock Logical; if `TRUE`, attaches a digest for reproducibility checks
#'   (reserved for a future release; currently informational only).
#'
#' @details
#' The function updates:
#' - `spec$data_prepared`: the resulting tibble after all steps.
#' - `spec$prep_log`: a tibble accumulating one log row per step with
#'   step‑specific metadata.
#'
#' If no steps are provided, the raw data are passed through unchanged and a
#' message is emitted.
#'
#' @return The input `spec` with `data_prepared` and `prep_log` populated.
#'
#' @examples
#' spec <- comp_spec(mtcars)
#'
#' # Remove extreme MPG outliers using the IQR rule (k = 3)
#' spec2 <- prepare(
#'   spec,
#'   steps = list(step_trim_outliers(var = mpg, k = 3, action = "remove"))
#' )
#' spec2$prep_log
#' head(spec2$data_prepared)
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

#' Step: trim or winsorize outliers
#'
#' Create a preparation step that flags outliers using common rules and
#' either removes them or winsorizes them to the nearest fence. Available
#' methods are the interquartile range (IQR), median absolute deviation (MAD),
#' and standard deviation (SD). Defaults follow the guidance in the
#' [*r4np* book](https://r4np.com/09_sources_of_bias.html#sec-dealing-with-outliers).
#'
#' @param var A tidy‑eval column identifying the **numeric** variable to trim.
#' @param method Outlier rule; one of `"iqr"`, `"mad"`, or `"sd"`.
#' @param k Multiplier for the chosen scale (IQR, MAD, or SD); defaults to `3`.
#'   For IQR, fences are `Q1 - k*IQR` and `Q3 + k*IQR`; for MAD, `median ± k*MAD`;
#'   for SD, `mean ± k*SD`.
#' @param action One of `"remove"` or `"winsorize"`.
#'
#' @details
#' This is a *step constructor*: it returns a function with signature
#' `function(df, spec)` that:
#' - Validates the target variable,
#' - Computes method-specific fences,
#' - Applies the chosen action,
#' - Returns `list(df = updated_df, log = one_row_tibble)` where the log
#'   contains `step`, `var`, `method`, `k`, `action`, `n_before`, `n_after`,
#'   and `n_changed`.
#'
#' @return A function suitable for use inside [prepare()]'s `steps` list.
#'
#' @examples
#' # Winsorize extreme values of `mpg` to IQR fences
#' step <- step_trim_outliers(mpg, method = "iqr", k = 3, action = "winsorize")
#' res <- step(df = mtcars, spec = comp_spec(mtcars))
#' res$log
#'
#' # Use as part of a preparation pipeline
#' spec <- comp_spec(mtcars)
#' spec <- prepare(spec, steps = list(step_trim_outliers(mpg, method = "sd", action = "remove")))
#' spec$prep_log
#' @export
step_trim_outliers <- function(
  var,
  method = c("iqr", "mad", "sd"),
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

    lim <- .flag_outliers(df[[v]], method = method, k = k)
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
