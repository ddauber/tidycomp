#' Run the comparison test (choose + fit engine)
#'
#' - If `engine` was set via `set_engine()`, that engine is run.
#' - Otherwise, engine is chosen from `strategy` and `diagnostics`:
#'   - "auto"/"pragmatic": prefer Welch t; if normality is severely flagged and very small n, suggest MW.
#'   - "parametric": Student's t.
#'   - "robust": Welch t for now (placeholder).
#'   - "permutation": not implemented in MVP.
#'
#' @export
test <- function(spec) {
  stopifnot(inherits(spec, "comp_spec"))
  if (is.null(spec$roles$outcome) || is.null(spec$roles$group)) {
    cli::cli_abort(
      "Set roles with `set_roles(outcome, group)` before `test()`."
    )
  }
  if (is.null(spec$design) || spec$design != "independent") {
    cli::cli_abort(
      "MVP `test()` currently supports `design = 'independent'` only."
    )
  }
  if (is.null(spec$outcome_type) || spec$outcome_type != "numeric") {
    cli::cli_abort("MVP `test()` supports `outcome_type = 'numeric'` only.")
  }

  data <- spec$data_prepared %||% spec$data_raw
  if (is.null(spec$diagnostics)) {
    cli::cli_warn("`diagnose()` not run; proceeding without diagnostic notes.")
  }

  # engine choice
  engine <- spec$engine
  if (is.null(engine)) {
    engine <- switch(
      spec$strategy,
      auto = "welch_t",
      pragmatic = "welch_t",
      parametric = "student_t",
      robust = "welch_t",
      permutation = "welch_t",
      "welch_t"
    )
    # gentle nudge to MW when severe normality + small n
    diag <- spec$diagnostics
    if (!is.null(diag)) {
      small_n <- any((diag$group_sizes$n) < 15)
      nonnorm <- any(diag$normality$p_shapiro < 0.01, na.rm = TRUE)
      if (small_n && nonnorm) {
        cli::cli_warn(
          "Severe non-normality with very small n detected; consider `set_engine('mann_whitney')`."
        )
      }
    }
  }

  eng_fun <- .tidycomp_engines()[[engine]]
  if (is.null(eng_fun)) {
    cli::cli_abort("Selected engine `{engine}` not available.")
  }

  res <- eng_fun(
    data = data,
    meta = list(roles = spec$roles, diagnostics = spec$diagnostics)
  )
  class(res) <- c("comp_result", class(res))
  spec$fitted <- res
  cli::cli_inform("Engine run: {engine}.")
  spec
}
