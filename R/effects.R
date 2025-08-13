#' Add an effect size to a fitted comparison
#'
#' Compute an effect size (with a confidence interval) for a numeric
#' outcome based on the inferential engine stored in
#' `spec$fitted$engine`. Supported tests map to the following metrics:
#'
#' - `"student_t"`: Cohen's *d* (no Hedges correction)
#' - `"welch_t"`: Hedges' *g* (default) or Cohen's *d* when
#'   `effect = "cohens_d"`
#' - `"mann_whitney"`: Wilcoxon *r* (rank biserial)
#' - `"paired_t"`: Cohen's *d* for paired samples
#' - `"wilcoxon_signed_rank"`: Wilcoxon *r* (rank biserial)
#' - `"kruskal_wallis"`: Rank *eta*² (default) or rank *epsilon*² when
#'   `effect = "epsilon_squared"`
#' - `"anova_oneway_equal"` / `"anova_oneway_welch"`: *eta*² (default) or
#'   *omega*² when `effect = "omega_squared"`
#' - `"anova_repeated"`: partial *eta*²
#' - `"friedman"`: Kendall's *W*
#'
#' The function reads from `spec$fitted` and writes `es_value`,
#' `es_conf_low`, `es_conf_high`, and `es_metric` before returning `spec`.
#'
#' @param spec A `comp_spec` created by [comp_spec()] and already fitted
#'   via `test()` (i.e., `spec$fitted` must exist). Roles must include a
#'   numeric outcome and an appropriate group (two-level for two-group
#'   engines, multi-level for ANOVA/Kruskal-Wallis, or `id` for repeated
#'   measures).
#' @param conf_level Confidence level for the interval (numeric in (0, 1),
#'   default `0.95`).
#' @param effect Optional override for some engines. Supported overrides
#'   are `effect = "cohens_d"` for the `welch_t` engine,
#'   `effect = "epsilon_squared"` for `kruskal_wallis`, and
#'   `effect = "omega_squared"` for one-way ANOVA engines.
#'
#' @details
#' - Supported designs: two-group, multi-group, or repeated measures with a
#'   **numeric** outcome.
#' - Backend functions from the
#'   [`effectsize`](https://easystats.github.io/effectsize/) package
#'   (e.g., `hedges_g()`, `cohens_d()`, `eta_squared()`,
#'   `rank_epsilon_squared()`).
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

  engine <- spec$fitted$engine %||% ""
  effect <- match.arg(effect, c("default", "cohens_d", "omega_squared", "epsilon_squared"))

  data <- spec$data_prepared %||% spec$data_raw
  df <- if (identical(spec$design, "repeated")) {
    .standardize_repeated_numeric(
      data,
      spec$roles$outcome,
      spec$roles$group,
      spec$roles$id
    )
  } else if (identical(spec$design, "paired")) {
    .standardize_paired_numeric(
      data,
      spec$roles$outcome,
      spec$roles$group,
      spec$roles$id
    )
  } else if (engine %in% c("kruskal_wallis", "anova_oneway_equal", "anova_oneway_welch")) {
    .standardize_multi_group_numeric(
      data,
      spec$roles$outcome,
      spec$roles$group
    )
  } else {
    .standardize_two_group_numeric(
      data,
      spec$roles$outcome,
      spec$roles$group
    )
  }
  es <- switch(
    engine,
    student_t = {
      out <- effectsize::cohens_d(
        outcome ~ group,
        data = df,
        ci = conf_level,
        hedges.correction = FALSE
      )
      list(
        value = out$Cohens_d,
        low = out$CI_low,
        high = out$CI_high,
        metric = "Cohens_d"
      )
    },
    welch_t = {
      if (effect == "cohens_d") {
        out <- effectsize::cohens_d(
          outcome ~ group,
          data = df,
          ci = conf_level,
          hedges.correction = FALSE
        )
        list(
          value = out$Cohens_d,
          low = out$CI_low,
          high = out$CI_high,
          metric = "Cohens_d"
        )
      } else {
        out <- effectsize::hedges_g(
          outcome ~ group,
          data = df,
          ci = conf_level
        )
        list(
          value = out$Hedges_g,
          low = out$CI_low,
          high = out$CI_high,
          metric = "Hedges_g"
        )
      }
    },
    mann_whitney = {
      out <- effectsize::rank_biserial(
        outcome ~ group,
        data = df,
        ci = conf_level
      )
      list(
        value = out$r_rank_biserial,
        low = out$CI_low,
        high = out$CI_high,
        metric = "r_Wilcoxon"
      )
    },
    kruskal_wallis = {
      if (effect == "epsilon_squared") {
        out <- effectsize::rank_epsilon_squared(
          outcome ~ group,
          data = df,
          ci = conf_level
        )
        list(
          value = out$rank_epsilon_squared,
          low = out$CI_low,
          high = out$CI_high,
          metric = "epsilon_squared"
        )
      } else {
        out <- effectsize::rank_eta_squared(
          outcome ~ group,
          data = df,
          ci = conf_level
        )
        list(
          value = out$rank_eta_squared,
          low = out$CI_low,
          high = out$CI_high,
          metric = "eta_squared"
        )
      }
    },
    anova_oneway_equal = {
      mod <- stats::aov(outcome ~ group, data = df)
      if (effect == "omega_squared") {
        out <- effectsize::omega_squared(mod, ci = conf_level)
        list(
          value = out$Omega2,
          low = out$CI_low,
          high = out$CI_high,
          metric = "omega_squared"
        )
      } else {
        out <- effectsize::eta_squared(mod, ci = conf_level)
        list(
          value = out$Eta2,
          low = out$CI_low,
          high = out$CI_high,
          metric = "eta_squared"
        )
      }
    },
    anova_oneway_welch = {
      mod <- stats::aov(outcome ~ group, data = df)
      if (effect == "omega_squared") {
        out <- effectsize::omega_squared(mod, ci = conf_level)
        list(
          value = out$Omega2,
          low = out$CI_low,
          high = out$CI_high,
          metric = "omega_squared"
        )
      } else {
        out <- effectsize::eta_squared(mod, ci = conf_level)
        list(
          value = out$Eta2,
          low = out$CI_low,
          high = out$CI_high,
          metric = "eta_squared"
        )
      }
    },
    anova_repeated = {
      mod <- stats::aov(outcome ~ group + Error(id/group), data = df)
      out <- effectsize::eta_squared(mod, partial = TRUE, ci = conf_level)
      row <- out[out$Group == "Within" & out$Parameter == "group", ]
      list(
        value = row$Eta2_partial,
        low = row$CI_low,
        high = row$CI_high,
        metric = "eta_squared_partial"
      )
    },
    friedman = {
      out <- effectsize::kendalls_w(
        outcome ~ group | id,
        data = df,
        ci = conf_level
      )
      list(
        value = out$Kendalls_W,
        low = out$CI_low,
        high = out$CI_high,
        metric = "Kendalls_W"
      )
    },
    paired_t = {
      g <- names(df)
      out <- effectsize::cohens_d(
        df[[g[2]]],
        df[[g[1]]],
        paired = TRUE,
        ci = conf_level,
        hedges.correction = FALSE
      )
      list(
        value = out$Cohens_d,
        low = out$CI_low,
        high = out$CI_high,
        metric = "Cohens_d"
      )
    },
    wilcoxon_signed_rank = {
      g <- names(df)
      out <- effectsize::rank_biserial(
        df[[g[2]]],
        df[[g[1]]],
        paired = TRUE,
        ci = conf_level
      )
      list(
        value = out$r_rank_biserial,
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
      list(
        value = out$Hedges_g,
        low = out$CI_low,
        high = out$CI_high,
        metric = "Hedges_g"
      )
    }
  )

  spec$fitted$es_value <- es$value
  spec$fitted$es_conf_low <- es$low
  spec$fitted$es_conf_high <- es$high
  spec$fitted$es_metric <- es$metric
  cli::cli_inform("Effect size added: {es$metric}.")
  spec
}
