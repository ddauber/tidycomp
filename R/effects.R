#' Add an effect size to a fitted comparison
#'
#' Compute an effect size (with a confidence interval) for a two‑group,
#' numeric outcome based on the inferential engine stored in
#' `spec$fitted$engine`. Supported tests map to the following metrics:
#'
#' - `"student_t"`: Cohen's *d* (no Hedges correction)
#' - `"welch_t"`: Hedges' *g* (default) or Cohen's *d* when
#'   `effect = "cohens_d"`
#' - `"mann_whitney"`: Wilcoxon *r* (rank biserial)
#'
#' The function reads from `spec$fitted` and writes `es_value`,
#' `es_conf_low`, `es_conf_high`, and `es_metric` before returning `spec`.
#'
#' @param spec A `comp_spec` created by [comp_spec()] and already fitted
#'   via `test()` (i.e., `spec$fitted` must exist). Roles must include a
#'   numeric outcome and a two‑level group.
#' @param conf_level Confidence level for the interval (numeric in (0, 1),
#'   default `0.95`).
#' @param effect Optional override for some engines. Currently, only
#'   `effect = "cohens_d"` is supported for the `welch_t` engine to
#'   compute Cohen's *d* instead of Hedges' *g*.
#'
#' @details
#' - Supported design: two‑group comparison with a **numeric** outcome.
#' - Backend functions from the
#'   [`effectsize`](https://easystats.github.io/effectsize/) package
#'   (e.g., `hedges_g()`, `cohens_d()`, `rank_biserial()`).
#' - If the **effectsize** package is not installed, a warning is issued
#'   and the input `spec` is returned unchanged.
#'
#' The function selects `spec$data_prepared` when available, otherwise
#' falls back to `spec$data_raw`. It standardizes inputs internally and
#' then calls the appropriate effect size function based on
#' `spec$fitted$engine`.
#'
#' @return The input `spec`, updated in place with effect‑size fields in
#'   `spec$fitted`: `es_value`, `es_conf_low`, `es_conf_high`, `es_metric`.
#'
#' @seealso [comp_spec()], diagnose(), test()
#'
#' @examples
#' # Minimal workflow (illustrative):
#' spec <- comp_spec(mtcars)
#' spec$roles <- list(outcome = "mpg", group = "am")  # am has two levels
#' spec$outcome_type <- "numeric"
#'
#' # Pretend we've run the inferential test step that creates `spec$fitted`
#' # (your package's `test()` function). Then add effect size:
#' # spec <- test(spec)
#' # spec <- effects(spec, conf_level = 0.90)
#' #
#' # Access results:
#' # spec$fitted$es_value
#' # spec$fitted$es_conf_low
#' # spec$fitted$es_conf_high
#' # spec$fitted$es_metric
#'
#' @export
effects <- function(spec, conf_level = 0.95, effect = "default") {
  stopifnot(inherits(spec, "comp_spec"))
  if (is.null(spec$fitted)) {
    cli::cli_abort("Run `test()` before `effects()`.")
  }
  if (!requireNamespace("effectsize", quietly = TRUE)) {
    cli::cli_warn("Package {effectsize} not installed; skipping effect size.")
    return(spec)
  }

  data <- spec$data_prepared %||% spec$data_raw
  df <- .standardize_two_group_numeric(
    data,
    spec$roles$outcome,
    spec$roles$group
  )

  engine <- spec$fitted$engine %||% ""
  effect <- match.arg(effect, c("default", "cohens_d"))
  es <- switch(
    engine,
    student_t = {
      out <- effectsize::cohens_d(
        outcome ~ group,
        data = df,
        ci = conf_level,
        hedges.correction = FALSE
      )
      list(value = out$Cohens_d, low = out$CI_low, high = out$CI_high,
           metric = "Cohens_d")
    },
    welch_t = {
      if (effect == "cohens_d") {
        out <- effectsize::cohens_d(
          outcome ~ group,
          data = df,
          ci = conf_level,
          hedges.correction = FALSE
        )
        list(value = out$Cohens_d, low = out$CI_low, high = out$CI_high,
             metric = "Cohens_d")
      } else {
        out <- effectsize::hedges_g(
          outcome ~ group,
          data = df,
          ci = conf_level
        )
        list(value = out$Hedges_g, low = out$CI_low, high = out$CI_high,
             metric = "Hedges_g")
      }
    },
    mann_whitney = {
      out <- effectsize::rank_biserial(
        outcome ~ group,
        data = df,
        ci = conf_level
      )
      list(
        value = out$Rank_Biserial,
        low = out$CI_low,
        high = out$CI_high,
        metric = "r_Wilcoxon"
      )
    },
    {
      out <- effectsize::hedges_g(
        outcome ~ group,
        data = df,
        ci = conf_level
      )
      cli::cli_warn("Unknown engine; defaulting to Hedges' g.")
      list(value = out$Hedges_g, low = out$CI_low, high = out$CI_high,
           metric = "Hedges_g")
    }
  )

  spec$fitted$es_value <- es$value
  spec$fitted$es_conf_low <- es$low
  spec$fitted$es_conf_high <- es$high
  spec$fitted$es_metric <- es$metric
  cli::cli_inform("Effect size added: {es$metric}.")
  spec
}
