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
    friedman = engine_friedman,
    fisher_exact = engine_fisher_exact,
    chisq_yates = engine_chisq_yates,
    chisq_nxn = engine_chisq_nxn,
    mcnemar_chi2 = engine_mcnemar_chi2,
    mcnemar_chi2_cc = engine_mcnemar_chi2_cc,
    mcnemar_exact = engine_mcnemar_exact
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
  idx <- utils::tail(grep("Within", nm), 1)
  if (!length(idx)) {
    idx <- utils::tail(grep(":", nm), 1)
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
  # ----- helpers -----------------------------------------------------------
  .safe_num1 <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) == 0L) NA_real_ else v[1]
  }
  .first_effect_row <- function(tab) {
    rn <- rownames(tab)
    if (is.null(rn)) {
      1L
    } else {
      j <- which(!grepl("\\(Intercept\\)", rn))
      if (length(j)) j[1] else 1L
    }
  }
  .extract_p_mauchly <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }
    if (is.numeric(x) && length(x) == 1L) {
      return(.safe_num1(x))
    }
    if (is.data.frame(x) && nrow(x) > 0L) {
      pcol <- intersect(
        names(x),
        c("p", "p_value", "p.value", "Pr..W.", "Pr(>W)")
      )[1]
      if (!is.null(pcol)) return(.safe_num1(x[[pcol]][1]))
    }
    NA_real_
  }

  # ----- options & standardization ----------------------------------------
  args <- meta$engine$args %||% meta$engine_args %||% list()
  user_corr <- args$correction
  afex_args <- args$afex_args %||% list()

  df <- .standardize_repeated_numeric(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )

  # ----- fallback if afex missing -----------------------------------------
  if (!rlang::is_installed("afex")) {
    cli::cli_warn(
      "Package {.pkg afex} not installed; using stats::aov fallback."
    )
    return(engine_anova_repeated_base(data, meta))
  }

  # ----- Step 1: obtain/compute sphericity p ------------------------------
  p_mauchly <- .extract_p_mauchly(meta$diagnostics$sphericity)
  if (
    (!is.finite(p_mauchly) || is.na(p_mauchly)) &&
      rlang::is_installed("performance")
  ) {
    fit_unc <- try(
      do.call(
        afex::aov_ez,
        utils::modifyList(
          list(
            id = "id",
            dv = "outcome",
            within = "group",
            data = df,
            anova_table = list(correction = "none", es = "none")
          ),
          afex_args
        )
      ),
      silent = TRUE
    )
    if (!inherits(fit_unc, "try-error")) {
      sp_perf <- tryCatch(
        performance::check_sphericity(fit_unc),
        error = function(e) NULL
      )
      p_mauchly <- .extract_p_mauchly(sp_perf)
    }
  }

  # ----- Step 2: decide correction (user override wins) -------------------
  if (!is.null(user_corr)) {
    correction <- match.arg(user_corr, c("none", "GG", "HF"))
  } else if (is.finite(p_mauchly) && p_mauchly < 0.05) {
    correction <- "GG"
  } else {
    if (!is.finite(p_mauchly) || is.na(p_mauchly)) {
      cli::cli_warn(
        "Sphericity p unavailable; using uncorrected unless overridden."
      )
    }
    correction <- "none"
  }

  # ----- Step 3: fit once with chosen correction -------------------------
  fit <- do.call(
    afex::aov_ez,
    utils::modifyList(
      list(
        id = "id",
        dv = "outcome",
        within = "group",
        data = df,
        anova_table = list(correction = correction, es = "none")
      ),
      afex_args
    )
  )
  tab <- as.data.frame(fit$anova_table)

  # pick the effect row robustly and extract standardized scalars
  i <- .first_effect_row(tab)

  need <- c("num Df", "den Df", "MSE", "F", "Pr(>F)")
  miss <- setdiff(need, names(tab))
  if (length(miss)) {
    cli::cli_abort(c(
      "afex anova_table is missing expected columns.",
      "x" = "Missing: {.val {miss}}",
      "i" = "Available: {.val {names(tab)}}"
    ))
  }

  F_val <- .safe_num1(tab[i, "F", drop = TRUE])
  df1 <- .safe_num1(tab[i, "num Df", drop = TRUE])
  df2 <- .safe_num1(tab[i, "den Df", drop = TRUE])
  p_val <- .safe_num1(tab[i, "Pr(>F)", drop = TRUE])
  if (!is.finite(p_val)) {
    p_val <- stats::pf(F_val, df1, df2, lower.tail = FALSE)
  }

  # ----- Step 4: notes & result ------------------------------------------
  base_notes <- meta$diagnostics$notes %||% character()
  note <- if (is.finite(p_mauchly)) {
    if (identical(correction, "none")) {
      sprintf(
        "Sphericity not violated (Mauchly p = %.3g); uncorrected results reported.",
        p_mauchly
      )
    } else {
      sprintf(
        "Sphericity violated (Mauchly p = %.3g); %s correction reported.",
        p_mauchly,
        correction
      )
    }
  } else {
    "Sphericity p unavailable; uncorrected results reported."
  }

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
    metric = if (identical(correction, "none")) "uncorrected" else correction,
    notes = list(c(base_notes, note))
  )

  # keep whatever sphericity object you had (as tibble if possible)
  sp <- meta$diagnostics$sphericity
  if (!is.null(sp)) {
    sp <- tryCatch(tibble::as_tibble(sp), error = function(e) sp)
  }
  attr(res, "model") <- fit
  attr(res, "diagnostics") <- list(sphericity = sp)
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

#' Fisher's exact test engine (internal)
#'
#' @keywords internal
#' @noRd
engine_fisher_exact <- function(data, meta) {
  df <- .standardize_two_group_factor(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  tbl <- table(df$group, df$outcome)
  fit <- stats::fisher.test(tbl)
  res <- tibble::tibble(
    test = "fisher",
    method = fit$method,
    engine = "fisher_exact",
    n = sum(tbl),
    statistic = NA_real_,
    df = NA_real_,
    p.value = unname(fit$p.value),
    estimate = if (!is.null(fit$estimate)) unname(fit$estimate) else NA_real_,
    conf.low = if (!is.null(fit$conf.int)) fit$conf.int[1] else NA_real_,
    conf.high = if (!is.null(fit$conf.int)) fit$conf.int[2] else NA_real_,
    metric = if (!is.null(fit$estimate)) "odds_ratio" else NA_character_,
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' Chi-squared test with Yates continuity correction (2x2)
#'
#' @keywords internal
#' @noRd
engine_chisq_yates <- function(data, meta) {
  df <- .standardize_two_group_factor(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  tbl <- table(df$group, df$outcome)
  fit <- stats::chisq.test(tbl, correct = TRUE)
  res <- tibble::tibble(
    test = "chi_squared",
    method = fit$method,
    engine = "chisq_yates",
    n = sum(tbl),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
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

#' Chi-squared test for general contingency tables
#'
#' @keywords internal
#' @noRd
engine_chisq_nxn <- function(data, meta) {
  df <- .standardize_two_group_factor(
    data,
    meta$roles$outcome,
    meta$roles$group
  )
  tbl <- table(df$group, df$outcome)
  fit <- stats::chisq.test(tbl, correct = FALSE)
  res <- tibble::tibble(
    test = "chi_squared",
    method = fit$method,
    engine = "chisq_nxn",
    n = sum(tbl),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
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

#' McNemar engines for paired binary data
#'
#' @keywords internal
#' @noRd
engine_mcnemar_chi2 <- function(data, meta) {
  wide <- .standardize_paired_categorical(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )
  tbl <- table(wide[[1]], wide[[2]])
  b <- tbl[1, 2]
  c <- tbl[2, 1]
  discordant <- b + c
  if (discordant < 10) {
    cli::cli_warn("Discordant pairs < 10; chi-square approximation may be invalid.")
  }
  fit <- stats::mcnemar.test(tbl, correct = FALSE)
  args <- meta$engine$args %||% meta$engine_args %||% list()
  conf.level <- args$conf.level %||% 0.95
  if (b == 0 || c == 0) {
    b <- b + 0.5
    c <- c + 0.5
  }
  or <- b / c
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  se <- sqrt(1 / b + 1 / c)
  ci <- exp(log(or) + c(-1, 1) * z * se)
  res <- tibble::tibble(
    test = "mcnemar",
    method = fit$method,
    engine = "mcnemar_chi2",
    n = sum(tbl),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
    p.value = unname(fit$p.value),
    estimate = or,
    conf.low = ci[1],
    conf.high = ci[2],
    metric = "odds_ratio",
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' @keywords internal
#' @noRd
engine_mcnemar_chi2_cc <- function(data, meta) {
  wide <- .standardize_paired_categorical(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )
  tbl <- table(wide[[1]], wide[[2]])
  b <- tbl[1, 2]
  c <- tbl[2, 1]
  discordant <- b + c
  if (discordant < 10) {
    cli::cli_warn("Discordant pairs < 10; chi-square approximation may be invalid.")
  }
  fit <- stats::mcnemar.test(tbl, correct = TRUE)
  args <- meta$engine$args %||% meta$engine_args %||% list()
  conf.level <- args$conf.level %||% 0.95
  if (b == 0 || c == 0) {
    b <- b + 0.5
    c <- c + 0.5
  }
  or <- b / c
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  se <- sqrt(1 / b + 1 / c)
  ci <- exp(log(or) + c(-1, 1) * z * se)
  res <- tibble::tibble(
    test = "mcnemar",
    method = fit$method,
    engine = "mcnemar_chi2_cc",
    n = sum(tbl),
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
    p.value = unname(fit$p.value),
    estimate = or,
    conf.low = ci[1],
    conf.high = ci[2],
    metric = "odds_ratio",
    notes = list(meta$diagnostics$notes %||% character())
  )
  attr(res, "model") <- fit
  res
}

#' @keywords internal
#' @noRd
engine_mcnemar_exact <- function(data, meta) {
  wide <- .standardize_paired_categorical(
    data,
    meta$roles$outcome,
    meta$roles$group,
    meta$roles$id
  )
  tbl <- table(wide[[1]], wide[[2]])
  args <- meta$engine$args %||% meta$engine_args %||% list()
  alternative <- args$alternative %||% "two.sided"
  conf.level <- args$conf.level %||% 0.95
  if (requireNamespace("exact2x2", quietly = TRUE)) {
    fit <- exact2x2::mcnemar.exact(
      tbl,
      alternative = alternative,
      conf.level = conf.level
    )
    estimate <- if (!is.null(fit$estimate)) unname(fit$estimate) else NA_real_
    ci <- if (!is.null(fit$conf.int)) fit$conf.int else c(NA_real_, NA_real_)
    notes <- meta$diagnostics$notes %||% character()
  } else {
    fit <- stats::mcnemar.test(tbl, correct = TRUE)
    b <- tbl[1, 2]
    c <- tbl[2, 1]
    if (b == 0 || c == 0) {
      b <- b + 0.5
      c <- c + 0.5
    }
    estimate <- b / c
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    se <- sqrt(1 / b + 1 / c)
    ci <- exp(log(estimate) + c(-1, 1) * z * se)
    notes <- c(
      meta$diagnostics$notes %||% character(),
      "exact2x2 not installed; used continuity-corrected chi-square."
    )
  }
  res <- tibble::tibble(
    test = "mcnemar",
    method = fit$method,
    engine = "mcnemar_exact",
    n = sum(tbl),
    statistic = if (!is.null(fit$statistic)) unname(fit$statistic) else NA_real_,
    df = if (!is.null(fit$parameter)) unname(fit$parameter) else NA_real_,
    p.value = unname(fit$p.value),
    estimate = estimate,
    conf.low = ci[1],
    conf.high = ci[2],
    metric = "odds_ratio",
    notes = list(notes)
  )
  attr(res, "model") <- fit
  res
}
