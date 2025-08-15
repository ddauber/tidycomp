#' @keywords internal
#' @noRd
#' @details
#' **Developer note:** Engines are small, pure functions that accept a
#' prepared `data` frame and `meta` (roles, diagnostics, design) and return
#' a standardized, one-row result. To add a new engine, implement
#' `engine_*()` with the same contract and register it in `.tidycomp_engines()`.

#' Engine registry (internal)
#'
#' Return the named registry of available analysis engines.
#' Keys are user-facing engine names; values are the corresponding
#' engine functions.
#'
#' This is used by the dispatcher to resolve `strategy`/`engine`
#' choices to a concrete test implementation.
#'
#' @return A named list mapping engine ids to functions.
#' @keywords internal
#' @noRd
.tidycomp_engines <- function() {
  list(
    welch_t = engine_welch_t,
    student_t = engine_student_t,
    mann_whitney = engine_mann_whitney,
    paired_t = engine_paired_t,
    wilcoxon_signed_rank = engine_wilcoxon_signed_rank,
    anova_oneway_equal = engine_anova_oneway_equal,
    anova_oneway_welch = engine_anova_oneway_welch,
    kruskal_wallis = engine_kruskal_wallis,
    anova_repeated = engine_anova_repeated,
    anova_repeated_base = engine_anova_repeated_base,
    friedman = engine_friedman
  )
}

#' Set engine-specific options
#'
#' Updates the engine configuration for a model specification with
#' arguments that will be passed to the selected engine's internal
#' function when \code{\link{test}()} is called.
#'
#' This helper allows users to supply parameters that are specific
#' to the current engine (e.g., correction method, output format,
#' or additional arguments for the underlying statistical function)
#' without affecting other engines or the general API.
#'
#' @param x A model specification object created with
#'   \code{\link{comp_spec}()} and configured with
#'   \code{\link{set_engine}()}.
#' @param ... Named arguments to add or update in the engine's
#'   \code{args} list. These will be made available to the
#'   engine function via \code{meta$engine$args}.
#'
#' @return An updated model specification object with engine-specific
#'   options stored in \code{$engine_args}.
#'
#' @seealso \code{\link{set_engine}}, \code{\link{test}}
#' @examples
#' spec <- comp_spec(mtcars) |>
#'   set_roles(outcome = mpg, group = cyl) |>
#'   set_engine("anova_repeated") |>
#'   set_engine_options(correction = "none")
#'
#' spec$engine_args
#'
#' @export
set_engine_options <- function(x, ...) {
  x$engine_args <- utils::modifyList(
    x$engine_args %||% list(),
    list(...)
  )
  x
}


#' Welch two-sample t-test engine (internal)
#'
#' Perform a two-sample Welch *t*-test for a numeric outcome with two groups
#' (unequal variances). Expects a standardized two-column frame containing
#' the outcome and group specified in `meta$roles`.
#'
#' @param data A data frame containing at least the outcome and group columns.
#' @param meta A list carrying analysis metadata (e.g., `roles`, `diagnostics`,
#'   and any design notes) assembled upstream.
#'
#' @details
#' Assumes: numeric outcome, two groups (factor with 2 levels).
#' Computes the Welch test (i.e., unequal variances). Any preflight checks
#' should be handled before calling the engine.
#'
#' @return A one-row tibble (or list) with standardized fields such as:
#'   `method`, `design`, `n`, `statistic`, `df`, `p.value`, `estimate`,
#'   `conf.low`, `conf.high`, `metric`, and `notes`.
#'
#' @keywords internal
#' @noRd
engine_welch_t <- function(data, meta) {
  df <- .standardize_two_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  fit <- stats::t.test(outcome ~ group, data = df, var.equal = FALSE)
  res <- tibble::tibble(
    test = "t",
    method = "Welch t-test",
    engine = "welch_t",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
    p.value = unname(fit$p.value),
    estimate = diff(rev(fit$estimate)), # group2 - group1 (consistent with t.test)
    conf.low = fit$conf.int[1],
    conf.high = fit$conf.int[2],
    metric = "mean_diff",
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' Student (equal-variance) t-test engine (internal)
#'
#' Perform a two-sample Student *t*-test for a numeric outcome with two groups
#' (assumes equal variances / pooled SD).
#'
#' @param data A data frame containing at least the outcome and group columns.
#' @param meta A list with roles/diagnostics and other upstream metadata.
#'
#' @details
#' Assumes: numeric outcome, two groups (factor with 2 levels).
#' Uses the pooled-variance *t* test; use the Welch engine when variances
#' appear heterogeneous.
#'
#' @return A one-row tibble (or list) with standardized result fields:
#'   `method`, `design`, `n`, `statistic`, `df`, `p.value`, `estimate`,
#'   `conf.low`, `conf.high`, `metric`, and `notes`.
#'
#' @keywords internal
#' @noRd
engine_student_t <- function(data, meta) {
  df <- .standardize_two_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  fit <- stats::t.test(outcome ~ group, data = df, var.equal = TRUE)
  res <- tibble::tibble(
    test = "t",
    method = "Student's t-test",
    engine = "student_t",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
    p.value = unname(fit$p.value),
    estimate = diff(rev(fit$estimate)),
    conf.low = fit$conf.int[1],
    conf.high = fit$conf.int[2],
    metric = "mean_diff",
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' Mann-Whitney (Wilcoxon rank-sum) engine (internal)
#'
#' Perform a two-group, distribution-free comparison using the
#' Mann-Whitney / Wilcoxon rank-sum test. Useful when normality/variance
#' assumptions are doubtful or the outcome is ordinal.
#'
#' @param data A data frame containing at least the outcome and group columns.
#' @param meta A list with roles/diagnostics and other upstream metadata.
#'
#' @details
#' Assumes: numeric or ordinal outcome, two groups (factor with 2 levels).
#' Reports a standardized result set; the primary `metric` reflects
#' stochastic ordering (e.g., probability of superiority).
#'
#' @return A one-row tibble (or list) with standardized result fields:
#'   `method`, `design`, `n`, `statistic`, `df` (may be `NA`), `p.value`,
#'   `estimate` (may be `NA` for MVP), `conf.low`/`conf.high` (may be `NA`),
#'   `metric`, and `notes`.
#'
#' @keywords internal
#' @noRd
engine_mann_whitney <- function(data, meta) {
  df <- .standardize_two_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )

  # settings with sensible defaults
  alt <- meta$settings$alternative %||% "two.sided"
  conf_level <- meta$settings$conf_level %||% 0.95
  exact_opt <- meta$settings$exact %||% NULL

  # if ties in outcomes, use normal approximation (exact test not valid)
  if (is.null(exact_opt)) {
    ties_present <- any(duplicated(rank(df$outcome)))
    exact_opt <- if (ties_present) FALSE else NULL
  }

  fit <- stats::wilcox.test(
    outcome ~ group,
    data = df,
    alternative = alt,
    conf.int = TRUE,
    conf.level = conf_level,
    exact = exact_opt, # let R choose unless ties force FALSE
    correct = TRUE # continuity correction for normal approx
  )

  # safe extraction (estimate/CI may be absent in edge cases)
  est <- if (!is.null(fit$estimate)) unname(fit$estimate) else NA_real_
  has_ci <- !is.null(fit$conf.int) && length(fit$conf.int) == 2L
  ci_lo <- if (has_ci) unname(fit$conf.int[1]) else NA_real_
  ci_hi <- if (has_ci) unname(fit$conf.int[2]) else NA_real_
  clvl <- if (has_ci) unname(attr(fit$conf.int, "conf.level")) else conf_level

  res <- tibble::tibble(
    test = "wilcox",
    method = fit$method, # e.g., "Wilcoxon rank sum exact test"
    engine = "mann_whitney",
    n = nrow(df),
    statistic = unname(fit$statistic), # W
    df = NA_real_, # nonparametric ⇒ no df
    p.value = unname(fit$p.value),
    estimate = est, # Hodges–Lehmann location shift
    conf.low = ci_lo,
    conf.high = ci_hi,
    conf.level = clvl,
    metric = "location_shift_hodges_lehmann", # clearer than "stochastic_ordering"
    notes = list(c(
      meta$diagnostics$notes %||% character(),
      if (!has_ci) {
        "CI unavailable (ties/small sample/edge case)."
      } else {
        character()
      }
    ))
  )
  attr(res, "model") <- fit
  res
}

#' Paired t-test engine (internal)
#'
#' Perform a paired t-test for a numeric outcome with two related groups.
#'
#' @param data A data frame containing outcome, group, and id columns.
#' @param meta A list with roles/diagnostics metadata.
#' @keywords internal
#' @noRd
engine_paired_t <- function(data, meta) {
  df <- .standardize_paired_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )
  g <- names(df)
  fit <- stats::t.test(df[[g[2]]], df[[g[1]]], paired = TRUE)
  res <- tibble::tibble(
    test = "t",
    method = "Paired t-test",
    engine = "paired_t",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
    p.value = unname(fit$p.value),
    estimate = unname(fit$estimate),
    conf.low = fit$conf.int[1],
    conf.high = fit$conf.int[2],
    metric = "mean_diff",
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' Wilcoxon signed-rank engine (internal)
#'
#' Perform a paired, distribution-free comparison using the Wilcoxon
#' signed-rank test.
#'
#' @param data A data frame containing outcome, group, and id columns.
#' @param meta A list with roles/diagnostics and optional settings.
#' @keywords internal
#' @noRd
engine_wilcoxon_signed_rank <- function(data, meta) {
  df <- .standardize_paired_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )
  g <- names(df)
  alt <- meta$settings$alternative %||% "two.sided"
  conf_level <- meta$settings$conf_level %||% 0.95
  exact_opt <- meta$settings$exact %||% NULL
  fit <- stats::wilcox.test(
    df[[g[2]]],
    df[[g[1]]],
    paired = TRUE,
    alternative = alt,
    conf.int = TRUE,
    conf.level = conf_level,
    exact = exact_opt,
    correct = TRUE
  )
  est <- if (!is.null(fit$estimate)) unname(fit$estimate) else NA_real_
  has_ci <- !is.null(fit$conf.int) && length(fit$conf.int) == 2L
  ci_lo <- if (has_ci) unname(fit$conf.int[1]) else NA_real_
  ci_hi <- if (has_ci) unname(fit$conf.int[2]) else NA_real_
  clvl <- if (has_ci) unname(attr(fit$conf.int, "conf.level")) else conf_level
  res <- tibble::tibble(
    test = "wilcox_signed_rank",
    method = fit$method,
    engine = "wilcoxon_signed_rank",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df = NA_real_,
    p.value = unname(fit$p.value),
    estimate = est,
    conf.low = ci_lo,
    conf.high = ci_hi,
    conf.level = clvl,
    metric = "location_shift_hodges_lehmann",
    notes = list(c(
      meta$diagnostics$notes %||% character(),
      if (!has_ci) {
        "CI unavailable (ties/small sample/edge case)."
      } else {
        character()
      }
    ))
  )
  attr(res, "model") <- fit
  res
}

#' One-way ANOVA engine assuming equal variances (internal)
#'
#' Perform a classic one-way ANOVA for a numeric outcome across three or more
#' independent groups, assuming equal group variances.
#'
#' @param data A data frame containing outcome and group columns.
#' @param meta A list with roles/diagnostics metadata.
#' @keywords internal
#' @noRd
engine_anova_oneway_equal <- function(data, meta) {
  df <- .standardize_multi_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  fit <- stats::oneway.test(outcome ~ group, data = df, var.equal = TRUE)
  res <- tibble::tibble(
    test = "anova",
    method = "One-way ANOVA",
    engine = "anova_oneway_equal",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df1 = unname(fit$parameter[1]),
    df2 = unname(fit$parameter[2]),
    p.value = unname(fit$p.value),
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    metric = NA_character_,
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' Welch's one-way ANOVA engine (internal)
#'
#' Perform Welch's one-way ANOVA for a numeric outcome across three or more
#' independent groups, allowing for unequal group variances.
#'
#' @param data A data frame containing outcome and group columns.
#' @param meta A list with roles/diagnostics metadata.
#' @keywords internal
#' @noRd
engine_anova_oneway_welch <- function(data, meta) {
  df <- .standardize_multi_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  fit <- stats::oneway.test(outcome ~ group, data = df, var.equal = FALSE)
  res <- tibble::tibble(
    test = "anova",
    method = "Welch's ANOVA",
    engine = "anova_oneway_welch",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df1 = unname(fit$parameter[1]),
    df2 = unname(fit$parameter[2]),
    p.value = unname(fit$p.value),
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    metric = NA_character_,
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' Kruskal-Wallis engine (internal)
#'
#' Perform a Kruskal-Wallis rank-sum test for a numeric outcome across
#' three or more independent groups.
#'
#' @param data A data frame containing outcome and group columns.
#' @param meta A list with roles/diagnostics metadata.
#' @keywords internal
#' @noRd
engine_kruskal_wallis <- function(data, meta) {
  df <- .standardize_multi_group_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  fit <- stats::kruskal.test(outcome ~ group, data = df)
  res <- tibble::tibble(
    test = "kruskal_wallis",
    method = "Kruskal-Wallis rank sum test",
    engine = "kruskal_wallis",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df1 = unname(fit$parameter),
    df2 = NA_real_,
    p.value = unname(fit$p.value),
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    metric = NA_character_,
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' Repeated-measures ANOVA engine (base R fallback, internal)
#'
#' Perform a one-factor repeated-measures ANOVA using `stats::aov()`.
#'
#' @param data A data frame containing outcome, group, and id columns.
#' @param meta A list with roles/diagnostics metadata.
#' @keywords internal
#' @noRd
engine_anova_repeated_base <- function(data, meta) {
  df <- .standardize_repeated_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )
  fit <- stats::aov(outcome ~ group + Error(id / group), data = df)
  summ <- summary(fit)
  nm <- names(summ)
  idx <- tail(grep("Within", nm), 1)
  if (!length(idx)) {
    idx <- tail(grep(":", nm), 1)
  }
  if (!length(idx)) {
    err_idx <- grep("^Error:", nm)
    idx <- err_idx[length(err_idx)]
  }
  within <- summ[[idx]][[1]]
  res <- tibble::tibble(
    test = "anova_repeated",
    method = "Repeated measures ANOVA",
    engine = "anova_repeated_base",
    n = nrow(df),
    statistic = unname(within["group", "F value"]),
    df1 = unname(within["group", "Df"]),
    df2 = unname(within["Residuals", "Df"]),
    p.value = unname(within["group", "Pr(>F)"]),
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    metric = NA_character_,
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}


#' Repeated-measures ANOVA engine via afex (internal)
#'
#' @param data A data frame containing outcome, group, and id columns.
#' @param meta  A list with roles/diagnostics metadata.
#' @keywords internal
#' @noRd
engine_anova_repeated <- function(data, meta) {
  args <- meta$engine$args %||% list()
  user_corr <- args$correction
  afex_args <- args$afex_args %||% list()

  df <- .standardize_repeated_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )

  # Fallback if package missing
  if (!rlang::is_installed("afex")) {
    cli::cli_warn(
      "Package {.pkg afex} not installed; using stats::aov fallback."
    )
    return(engine_anova_repeated_base(data, meta))
  }

  # ---- Step 1: obtain sphericity result -------------------------------
  sp <- meta$diagnostics$sphericity
  p_mauchly <- .extract_sphericity_p(sp)

  if ((is.null(sp) || is.na(p_mauchly)) && rlang::is_installed("performance")) {
    base_args <- list(
      id = "id",
      dv = "outcome",
      within = "group",
      data = df,
      anova_table = list(correction = "none", es = "none")
    )
    fit_uncorr <- try(
      do.call(afex::aov_ez, utils::modifyList(base_args, afex_args)),
      silent = TRUE
    )
    if (!inherits(fit_uncorr, "try-error")) {
      sp <- tryCatch(performance::check_sphericity(fit_uncorr), error = function(e) NULL)
      p_mauchly <- .extract_sphericity_p(sp)
    }
  }

  # ---- Step 2: decide correction -------------------------------------
  if (!is.null(user_corr)) {
    correction <- match.arg(user_corr, c("none", "GG", "HF"))
  } else {
    correction <- if (!is.na(p_mauchly) && p_mauchly < 0.05) "GG" else "none"
  }

  # ---- Step 3: run afex::aov_ez with chosen correction ----------------
  base_args <- list(
    id = "id",
    dv = "outcome",
    within = "group",
    data = df,
    anova_table = list(correction = correction, es = "none")
  )
  fit <- do.call(afex::aov_ez, utils::modifyList(base_args, afex_args))
  tab <- as.data.frame(fit$anova_table)

  F_val <- unname(tab["group", "F"])
  df1_raw <- unname(tab["group", "num Df"])
  df2_raw <- unname(tab["group", "den Df"])

  if (correction == "none") {
    p_val <- unname(tab["group", "Pr(>F)"])
    df1 <- df1_raw
    df2 <- df2_raw
    metric <- "uncorrected"
    notes <- meta$diagnostics$notes %||% character()
  } else if (correction == "GG") {
    eps <- unname(tab["group", "GG eps"])
    df1 <- df1_raw * eps
    df2 <- df2_raw * eps
    p_val <- if ("Pr(>F[GG])" %in% colnames(tab)) {
      unname(tab["group", "Pr(>F[GG])"])
    } else {
      stats::pf(F_val, df1, df2, lower.tail = FALSE)
    }
    metric <- "GG"
    notes <- c(
      meta$diagnostics$notes %||% character(),
      "Reported with GG correction."
    )
  } else if (correction == "HF") {
    eps <- unname(tab["group", "HF eps"])
    df1 <- df1_raw * eps
    df2 <- df2_raw * eps
    p_val <- if ("Pr(>F[HF])" %in% colnames(tab)) {
      unname(tab["group", "Pr(>F[HF])"])
    } else {
      stats::pf(F_val, df1, df2, lower.tail = FALSE)
    }
    metric <- "HF"
    notes <- c(
      meta$diagnostics$notes %||% character(),
      "Reported with HF correction."
    )
  } else {
    cli::cli_warn("Lower-bound correction not implemented in this engine yet.")
    p_val <- unname(tab["group", "Pr(>F)"])
    df1 <- df1_raw
    df2 <- df2_raw
    metric <- "uncorrected"
    notes <- meta$diagnostics$notes %||% character()
  }

  # ---- Step 4: populate fitted ---------------------------------------
  res <- tibble::tibble(
    test = "anova_repeated",
    method = "Repeated measures ANOVA",
    engine = "anova_repeated",
    n_obs = nrow(df),
    n_subjects = length(unique(df$id)),
    statistic = F_val,
    df1 = df1,
    df2 = df2,
    p.value = p_val,
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    metric = metric,
    notes = list(notes)
  )

  sp_df <- NULL
  if (!is.null(sp)) {
    sp_df <- tryCatch(tibble::as_tibble(sp), error = function(e) sp)
  }

  attr(res, "model") <- fit
  attr(res, "diagnostics") <- list(sphericity = sp_df)
  res
}


#' Friedman test engine (internal)
#'
#' Perform a Friedman rank-sum test for repeated-measures designs.
#'
#' @param data A data frame containing outcome, group, and id columns.
#' @param meta A list with roles/diagnostics metadata.
#' @keywords internal
#' @noRd
engine_friedman <- function(data, meta) {
  df <- .standardize_repeated_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )
  fit <- stats::friedman.test(outcome ~ group | id, data = df)
  res <- tibble::tibble(
    test = "friedman",
    method = "Friedman rank sum test",
    engine = "friedman",
    n = nrow(df),
    statistic = unname(fit$statistic),
    df1 = unname(fit$parameter),
    df2 = NA_real_,
    p.value = unname(fit$p.value),
    estimate = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    metric = NA_character_,
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}
