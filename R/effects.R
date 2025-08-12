#' Add an effect size to a fitted comparison
#'
#' Compute Hedges' *g* (with a confidence interval) for a two‑group,
#' numeric outcome and store it on the `comp_spec`. This is a
#' post‑fit step that reads from `spec$fitted` and writes the following:
#' `es_value`, `es_conf_low`, `es_conf_high`, and `es_metric` (set to
#' `"Hedges_g"`).
#'
#' @param spec A `comp_spec` created by [comp_spec()] and already fitted
#'   via `test()` (i.e., `spec$fitted` must exist). Roles must include a
#'   numeric outcome and a two‑level group.
#' @param conf_level Confidence level for the interval (numeric in (0, 1),
#'   default `0.95`).
#'
#' @details
#' - Supported design: two‑group comparison with a **numeric** outcome.
#' - Backend: [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/).
#' - If the **effectsize** package is not installed, a warning is issued
#'   and the input `spec` is returned unchanged.
#'
#' The function selects `spec$data_prepared` when available, otherwise
#' falls back to `spec$data_raw`. It standardizes inputs internally and
#' then calls `effectsize::hedges_g(outcome ~ group, ...)`.
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
effects <- function(spec, conf_level = 0.95) {
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
  es <- effectsize::hedges_g(outcome ~ group, data = df, ci = conf_level)
  es <- tibble::as_tibble(es)[1, , drop = FALSE]

  spec$fitted$es_value <- es$Hedges_g
  spec$fitted$es_conf_low <- es$CI_low
  spec$fitted$es_conf_high <- es$CI_high
  spec$fitted$es_metric <- "Hedges_g"
  cli::cli_inform("Effect size added: Hedges' g.")
  spec
}
