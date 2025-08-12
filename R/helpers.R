#' Compare multiple engines on the same specification
#'
#' Fit the same `comp_spec` with several different engines and return
#' a combined tibble of results. This is useful for quickly seeing how
#' different inferential approaches (e.g., parametric vs. robust) perform
#' on the same dataset and roles.
#'
#' @param spec A `comp_spec` created by [comp_spec()] and ready to fit
#'   (roles and outcome type must be set).
#' @param engines Character vector of engine IDs to compare. Each must
#'   correspond to a registered engine (see `.tidycomp_engines()`).
#'
#' @details
#' For each engine in `engines`, this function:
#' 1. Clones the input spec and sets the requested engine via `set_engine()`.
#' 2. Fits it using [test()].
#' 3. Tidies the fitted result into a one-row tibble and adds a column
#'    `engine_forced` indicating the engine used.
#'
#' The resulting tibbles are row-bound into a single tibble for comparison.
#'
#' @return A tibble combining tidy results from all requested engines.
#' @export
compare_engines <- function(
  spec,
  engines = c("welch_t", "student_t", "mann_whitney")
) {
  stopifnot(inherits(spec, "comp_spec"))
  out <- purrr::map(engines, function(e) {
    s <- set_engine(spec, e)
    s <- test(s)
    tibble::add_column(tidy(s$fitted), engine_forced = e, .before = 1)
  })
  dplyr::bind_rows(out)
}

#' Show the current decision tree for engine selection
#'
#' Print the MVP decision rules for mapping strategies/diagnostics to
#' engine choices. This is primarily for developers and advanced users
#' to understand how `tidycomp` currently selects an engine when `strategy`
#' is `"auto"`, `"pragmatic"`, `"parametric"`, or `"robust"`.
#'
#' @details
#' Current rules (MVP):
#' - **strategy = `"parametric"`** → Student's *t* (equal variances)
#' - **strategy = `"auto"` / `"pragmatic"` / `"robust"`** → Welch's *t*
#' - **Severe non-normality** (Shapiro *p* < .01) **with very small n** (< 15) → suggest `"mann_whitney"` (manual override)
#'
#' This function is informational only; it does not run any analysis.
#'
#' @return `NULL` invisibly, after printing to the console.
#' @export
decision_tree <- function() {
  cli::cli_inform(
    "
Engine selection (MVP):
- strategy = 'parametric'  -> Student's t
- strategy = 'auto'/'pragmatic'/'robust' -> Welch t
- Severe non-normality (Shapiro p<.01) with very small n (<15): suggest `mann_whitney` (manual).
"
  )
  invisible(NULL)
}
