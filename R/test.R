#' Run the comparison test (choose + fit engine)
#'
#' Fit a `comp_spec` by selecting an engine (if not already set) and running
#' the corresponding test. The chosen engine is based on `spec$strategy`
#' and, when available, `spec$diagnostics`.
#'
#' @param spec A `comp_spec` created by [comp_spec()] with roles set via
#'   `set_roles(outcome, group)`, `design = "independent"` or `"paired"`, and
#'   `outcome_type = "numeric"` or `"binary"`. Optionally, run [diagnose()] beforehand.
#'
#' @details
#' **Scope (MVP):**
#' - Design: `design = "independent"` or `"paired"`.
#' - Outcome: `outcome_type = "numeric"` or `"binary"`.
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
  if (
    is.null(spec$design) ||
      !spec$design %in% c("independent", "paired", "repeated")
  ) {
    cli::cli_abort(
      "MVP `test()` currently supports `design = 'independent'`, 'paired', or 'repeated'."
    )
  }
  if (spec$design %in% c("paired", "repeated") && is.null(spec$roles$id)) {
    cli::cli_abort(
      "Paired or repeated designs require an id role via `set_roles(id = ...)`."
    )
  }
  if (
    is.null(spec$outcome_type) || !spec$outcome_type %in% c("numeric", "binary")
  ) {
    cli::cli_abort(
      "MVP `test()` supports `outcome_type = 'numeric'` or 'binary'."
    )
  }

  data <- spec$data_prepared %||% spec$data_raw
  if (spec$outcome_type == "binary") {
    if (
      !is.factor(data[[spec$roles$outcome]]) ||
        !is.factor(data[[spec$roles$group]])
    ) {
      cli::cli_abort("Binary outcome and group columns must be factors.")
    }
    diag <- spec$diagnostics
    if (is.null(diag)) {
      cli::cli_warn("`diagnose()` not run; running contingency checks.")
      if (spec$design == "paired") {
        diag <- .diagnose_paired_contingency(
          data,
          spec$roles$outcome,
          spec$roles$group,
          spec$roles$id
        )
      } else {
        g <- data[[spec$roles$group]]
        o <- data[[spec$roles$outcome]]
        diag <- .diagnose_contingency(g, o)
      }
      spec$diagnostics <- diag
    }
  } else if (is.null(spec$diagnostics)) {
    cli::cli_warn("`diagnose()` not run; proceeding without diagnostic notes.")
  }

  # engine choice
  engine <- spec$engine
  if (is.null(engine)) {
    if (spec$outcome_type == "binary") {
      engine <- spec$diagnostics$engine
    } else {
      g_levels <- if (!is.null(spec$roles$group)) {
        nlevels(factor(data[[spec$roles$group]]))
      } else {
        0 # nocov
      }
      if (spec$design == "paired") {
        engine <- switch(
          spec$strategy,
          auto = "paired_t",
          pragmatic = "paired_t",
          parametric = "paired_t",
          robust = "paired_t",
          permutation = "paired_t",
          "paired_t"
        )
      } else if (spec$design == "repeated") {
        engine <- switch(
          spec$strategy,
          auto = "anova_repeated",
          pragmatic = "anova_repeated",
          parametric = "anova_repeated",
          robust = "anova_repeated",
          permutation = "anova_repeated",
          "anova_repeated"
        )
        diag <- spec$diagnostics
        if (!is.null(diag)) {
          p_mauchly <- .extract_sphericity_p(diag$sphericity)
          if (is.finite(p_mauchly) && !is.na(p_mauchly) && p_mauchly < 0.05) {
            cli::cli_warn(
              "Sphericity violation detected; consider `set_engine('friedman')`."
            )
          }
        }
      } else {
        if (g_levels > 2) {
          engine <- switch(
            spec$strategy,
            auto = "anova_oneway_welch",
            pragmatic = "anova_oneway_welch",
            parametric = "anova_oneway_equal",
            robust = "anova_oneway_welch",
            permutation = "anova_oneway_welch",
            "anova_oneway_welch"
          )
          diag <- spec$diagnostics
          if (!is.null(diag)) {
            small_n <- any((diag$group_sizes$n) < 15)
            nonnorm <- any(diag$normality$p_shapiro < 0.01, na.rm = TRUE)
            if (small_n && nonnorm) {
              cli::cli_warn(
                "Severe non-normality with very small n detected; consider `set_engine('kruskal_wallis')`."
              )
            }
          }
        } else {
          engine <- switch(
            spec$strategy,
            auto = "welch_t",
            pragmatic = "welch_t",
            parametric = "student_t",
            robust = "welch_t",
            permutation = "welch_t",
            "welch_t"
          )
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
      }
    }
  }

  spec$engine <- engine
  spec$effects_hint <- .engine_effect_hint(engine)
  spec$post_hoc_hint <- .engine_post_hoc_hint(engine)

  eng_fun <- .tidycomp_engines()[[engine]]
  if (is.null(eng_fun)) {
    cli::cli_abort("Selected engine `{engine}` not available.")
  }

  res <- eng_fun(
    data = data,
    meta = list(
      roles = spec$roles,
      diagnostics = spec$diagnostics,
      engine = list(args = spec$engine_args %||% list()),
      engine_args = spec$engine_args %||% list()
    )
  )

  attr(res, "data") <- data
  attr(res, "roles") <- spec$roles
  attr(res, "design") <- spec$design
  attr(res, "outcome_type") <- spec$outcome_type
  class(res) <- c("comp_result", class(res))
  attr(res, "engine_hint") <- spec$effects_hint
  attr(res, "post_hoc_hint") <- spec$post_hoc_hint
  spec$fitted <- res
  cli::cli_inform("Engine run: {engine}.")

  if (isTRUE(spec$effects_args$compute)) {
    spec <- effects(spec)
  }

  if (isTRUE(spec$post_hoc_args$compute)) {
    spec <- post_hoc(spec)
  }

  spec
}
