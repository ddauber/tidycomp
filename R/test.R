#' Run the comparison test (choose + fit engine)
#'
#' Fit a `comp_spec` by selecting an engine (if not already set) and running
#' the corresponding test. The chosen engine is based on `spec$strategy`
#' and, when available, `spec$diagnostics`.
#'
#' @param spec A `comp_spec` created by [comp_spec()] with roles set via
#'   `set_roles(outcome, group)`, `design = "independent"`, and
#'   `outcome_type = "numeric"`. Optionally, run [diagnose()] beforehand.
#'
#' @details
#' **Scope (MVP):**
#' - Design: `design = "independent"` only.
#' - Outcome: `outcome_type = "numeric"` only.
#'
#' **Engine selection (defaults if `spec$engine` is `NULL`):**
#' - `strategy = "parametric"` → Student's *t* (`"student_t"`).
#' - `strategy = "auto"` / `"pragmatic"` / `"robust"` / `"permutation"` → Welch *t* (`"welch_t"`) for now.
#' - If diagnostics indicate **very small n** (< 15 in any group) **and severe non‑normality**
#'   (Shapiro *p* < .01), a gentle warning suggests using `"mann_whitney"`.
#'
#' On success, results are stored in `spec$fitted` as a `comp_result`, and the engine used is reported.
#'
#' @return The input `spec`, updated with a fitted `comp_result` in `spec$fitted`.
#'
#' @seealso [set_roles()], [set_engine()], [diagnose()], [compare_engines()],
#'   [decision_tree()], [comp_spec()]
#'
#' @examples
#' # Minimal workflow
#' spec <- comp_spec(mtcars)
#' spec$roles <- list(outcome = "mpg", group = "am")
#' spec$design <- "independent"
#' spec$outcome_type <- "numeric"
#'
#' # (Optional) run diagnostics to inform engine nudges
#' spec <- diagnose(spec)
#'
#' # Fit using strategy-based engine selection (Welch t by default)
#' spec <- test(spec)
#' spec$fitted
#'
#' # Force a specific engine, then fit
#' # spec <- set_engine(spec, "student_t")
#' # spec <- test(spec)
#' # tidy(spec$fitted)
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
