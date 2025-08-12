#' Add effect sizes (Hedges' g) to results
#'
#' @param spec A `comp_spec` with `fitted`.
#' @param conf_level Confidence level for ES CI.
#' @return Updated `comp_spec` with ES columns added to `fitted`.
#' @export
#' @examples
#' # After test():
#' # spec <- effects(spec)
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
