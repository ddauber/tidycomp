#' Compare multiple engines on the same spec
#' @param spec A `comp_spec`.
#' @param engines Character vector of engine names.
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

#' Show the current decision tree (text)
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
