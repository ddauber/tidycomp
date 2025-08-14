#' Add an effect size to a fitted comparison
#'
#' Compute an effect size (with a confidence interval) for a numeric outcome
#' based on the inferential engine stored in `spec$fitted$engine`. Supported
#' tests map to the following default metrics:
#'
#' - `"student_t"`: Cohen's *d*
#' - `"welch_t"`: Hedges' *g*
#' - `"mann_whitney"`: Wilcoxon *r* (rank biserial)
#' - `"paired_t"`: Cohen's *d* for paired samples
#' - `"wilcoxon_signed_rank"`: Wilcoxon *r* (rank biserial)
#' - `"anova_oneway_equal"`: Eta squared
#' - `"anova_oneway_welch"`: Omega squared
#' - `"kruskal_wallis"`: Rank-based epsilon squared
#' - `"anova_repeated"`: Partial eta squared
#' - `"friedman"`: Kendall's *W*
#'
#' The function reads from `spec$fitted` and writes `es_value`,
#' `es_conf_low`, `es_conf_high`, and `es_metric` before returning `spec`.
#'
#' @param spec A `comp_spec` created by [comp_spec()] and already fitted
#'   via `test()` (i.e., `spec$fitted` must exist). Roles must include a
#'   numeric outcome and a grouping variable.
#' @param conf_level Confidence level for the interval (numeric in (0, 1),
#'   default `0.95`).
#' @param effect Optional name of an effect size function from the
#'   **effectsize** package. The default (`"default"`) selects a sensible
#'   metric for the chosen engine. Any exported function from **effectsize**
#'   (e.g., `"cohens_d"`, `"omega_squared"`) may be provided.
#'
#' @details
#' - Supported designs: two-group and multi-group comparisons with a
#'   **numeric** outcome.
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
  if (!.has_effectsize()) {
    cli::cli_warn(
      "Package {.pkg effectsize} not installed; skipping effect size.",
      pkg = "effectsize"
    )
    return(spec)
  }

  data <- spec$data_prepared %||% spec$data_raw
  engine <- spec$fitted$engine %||% ""

  default_map <- list(
    student_t = "cohens_d",
    welch_t = "hedges_g",
    mann_whitney = "rank_biserial",
    paired_t = "cohens_d",
    wilcoxon_signed_rank = "rank_biserial",
    anova_oneway_equal = "eta_squared",
    anova_oneway_welch = "omega_squared",
    kruskal_wallis = "rank_epsilon_squared",
    anova_repeated = "eta_squared",
    friedman = "kendalls_w"
  )

  effect <- effect %||% "default"
  if (identical(effect, "default")) {
    effect <- default_map[[engine]] %||% "hedges_g"
    if (is.null(default_map[[engine]])) {
      cli::cli_warn("Unknown engine; defaulting to Hedges' g.")
    }
  }

  if (!effect %in% getNamespaceExports("effectsize")) {
    cli::cli_abort("Unknown effect size `{effect}` from package 'effectsize'.")
  }

  if (identical(spec$design, "paired")) {
    df <- .standardize_paired_numeric(
      data,
      spec$roles$outcome,
      spec$roles$group,
      spec$roles$id
    )
    g <- names(df)
    args <- list(df[[g[2]]], df[[g[1]]], paired = TRUE, ci = conf_level)
  } else if (
    engine %in% c("anova_oneway_equal", "anova_oneway_welch", "kruskal_wallis")
  ) {
    df <- .standardize_multi_group_numeric(
      data,
      spec$roles$outcome,
      spec$roles$group
    )
    if (engine == "kruskal_wallis") {
      args <- list(outcome ~ group, data = df, ci = conf_level)
    } else {
      fit <- switch(
        engine,
        anova_oneway_equal = stats::aov(outcome ~ group, data = df),
        anova_oneway_welch = stats::oneway.test(outcome ~ group, data = df)
      )
      args <- list(fit, ci = conf_level)
    }
  } else if (engine %in% c("anova_repeated", "friedman")) {
    df <- .standardize_repeated_numeric(
      data,
      spec$roles$outcome,
      spec$roles$group,
      spec$roles$id
    )
    if (engine == "anova_repeated") {
      fit <- stats::aov(outcome ~ group + Error(id / group), data = df)
      args <- list(fit, partial = TRUE, ci = conf_level)
    } else {
      args <- list(outcome ~ group | id, data = df, ci = conf_level)
    }
  } else {
    df <- .standardize_two_group_numeric(
      data,
      spec$roles$outcome,
      spec$roles$group
    )
    args <- list(outcome ~ group, data = df, ci = conf_level)
  }

  effect_fun <- switch(
    effect,
    cohens_d = effectsize::cohens_d,
    hedges_g = effectsize::hedges_g,
    rank_biserial = effectsize::rank_biserial,
    eta_squared = effectsize::eta_squared,
    omega_squared = effectsize::omega_squared,
    rank_epsilon_squared = effectsize::rank_epsilon_squared,
    kendalls_w = effectsize::kendalls_w,
    getExportedValue("effectsize", effect)
  )

  out <- if (engine == "anova_repeated") {
    suppressWarnings(do.call(effect_fun, args))
  } else {
    do.call(effect_fun, args)
  }

  if (engine == "anova_repeated") {
    out <- out[out$Group == "Within", , drop = FALSE]
    out <- out[, c("Eta2_partial", "CI_low", "CI_high"), drop = FALSE]
  }

  metric <- names(out)[1]
  if (metric == "Parameter") {
    metric <- names(out)[2]
    out <- out[, c(metric, "CI_low", "CI_high"), drop = FALSE]
  }
  if (metric == "r_rank_biserial") {
    metric <- "r_Wilcoxon"
  }
  if (metric == "rank_epsilon_squared") {
    metric <- "Epsilon2"
  }

  spec$fitted$es_value <- unname(out[[1]])
  spec$fitted$es_conf_low <- out$CI_low %||% NA_real_
  spec$fitted$es_conf_high <- out$CI_high %||% NA_real_
  spec$fitted$es_metric <- metric
  cli::cli_inform("Effect size added: {metric}.")
  spec
}
