# Tests for internal analysis engines
# These tests verify that each engine produces results consistent
# with the corresponding base R statistical tests and that the
# registry exposes the expected engines.

# helper to build meta object used by engines
make_meta <- function() {
  list(
    roles = list(outcome = "outcome", group = "group", id = "id"),
    diagnostics = list(notes = character()),
    settings = list()
  )
}

test_that("engine registry lists available engines", {
  engines <- tidycomp:::.tidycomp_engines()
  expect_named(
    engines,
    c(
      "welch_t",
      "student_t",
      "mann_whitney",
      "paired_t",
      "wilcoxon_signed_rank",
      "anova_oneway_equal",
      "anova_oneway_welch",
      "kruskal_wallis",
      "anova_repeated",
      "anova_repeated_base",
      "friedman",
      "fisher_exact",
      "chisq_yates",
      "chisq_nxn",
      "mcnemar_chi2",
      "mcnemar_chi2_cc",
      "mcnemar_exact"
    )
  )
  purrr::walk(engines, ~ expect_true(is.function(.x)))
})

# Welch t-test ---------------------------------------------------------------

test_that("Welch t engine matches stats::t.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_welch_t(df, meta)
  base <- stats::t.test(outcome ~ group, data = df, var.equal = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$n, nrow(df))
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$df, unname(base$parameter))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, diff(rev(base$estimate)))
  expect_equal(
    c(res$conf.low, res$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

# ANOVA -----------------------------------------------------------------------

test_that("anova_oneway_equal engine matches stats::oneway.test with equal variances", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_anova_oneway_equal(df, meta)
  fit <- stats::oneway.test(outcome ~ group, data = df, var.equal = TRUE)
  expect_equal(res$statistic, unname(fit$statistic))
  expect_equal(res$df1, unname(fit$parameter[1]))
  expect_equal(res$df2, unname(fit$parameter[2]))
  expect_equal(res$p.value, unname(fit$p.value))
})

test_that("anova_oneway_welch engine matches stats::oneway.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_anova_oneway_welch(df, meta)
  fit <- stats::oneway.test(outcome ~ group, data = df, var.equal = FALSE)
  expect_equal(res$statistic, unname(fit$statistic))
  expect_equal(res$df1, unname(fit$parameter[1]))
  expect_equal(res$df2, unname(fit$parameter[2]))
  expect_equal(res$p.value, unname(fit$p.value))
})

# Kruskal-Wallis --------------------------------------------------------------

test_that("kruskal_wallis engine matches stats::kruskal.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_kruskal_wallis(df, meta)
  fit <- stats::kruskal.test(outcome ~ group, data = df)
  expect_equal(res$statistic, unname(fit$statistic))
  expect_equal(res$p.value, unname(fit$p.value))
})

# Repeated measures ANOVA ----------------------------------------------------

test_that("anova_repeated_base engine matches stats::aov", {
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12)
  )
  meta <- make_meta()
  res <- tidycomp:::engine_anova_repeated_base(df, meta)
  fit <- stats::aov(outcome ~ group + Error(id / group), data = df)
  summ <- summary(fit)
  within <- summ[["Error: Within"]][[1]]
  expect_equal(res$statistic, unname(within["group", "F value"]))
  expect_equal(res$p.value, unname(within["group", "Pr(>F)"]))
})

test_that("anova_repeated falls back to base when afex is missing", {
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12)
  )
  meta <- make_meta()
  res_base <- tidycomp:::engine_anova_repeated_base(df, meta)

  res_main <- testthat::with_mocked_bindings(
    is_installed = function(...) FALSE, # pretend afex is absent
    .package = "rlang",
    tidycomp:::engine_anova_repeated(df, meta)
  )

  expect_equal(res_main, res_base)
})

test_that("anova_repeated: uses uncorrected when sphericity OK; corrected when violated", {
  skip_if_not_installed("afex")
  skip_if_not_installed("performance")

  set.seed(42)

  # -------- Good sphericity (finite p > .05) ----------
  # Three levels with similar difference variances; no singular patterns
  N <- 60
  A <- rnorm(N, 0, 1)
  B <- A + rnorm(N, 0, 1.0) # similar spread to A
  C <- A + rnorm(N, 0, 1.0) # similar spread to A/B

  df_good <- tibble::tibble(
    id = rep(seq_len(N), each = 3),
    group = factor(rep(c("A", "B", "C"), times = N), levels = c("A", "B", "C")),
    outcome = c(A, B, C)
  )

  spec_good <- comp_spec(df_good) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    diagnose() |>
    test()

  p_good <- as.numeric(spec_good$diagnostics$sphericity$p)[1]
  expect_true(is.finite(p_good))
  expect_gt(p_good, 0.05)

  # Engine should report uncorrected line when sphericity is OK
  expect_true(nrow(spec_good$fitted) >= 1)
  expect_identical(spec_good$fitted$metric[1], "uncorrected")

  # -------- Bad sphericity (finite p < .05) -----------
  set.seed(2)
  N <- 60

  Sigma_bad <- matrix(
    c(
      1.0,
      0.85,
      0.10,
      0.85,
      1.50,
      -0.20,
      0.10,
      -0.20,
      4.00
    ),
    3,
    3,
    byrow = TRUE
  )
  mu <- c(0, 0, 0)

  # Base R multivariate normal
  L <- chol(Sigma_bad) # Cholesky decomposition
  Z <- matrix(rnorm(N * length(mu)), ncol = length(mu))
  Xb <- Z %*% L + matrix(mu, nrow = N, ncol = length(mu), byrow = TRUE)

  df_bad <- tibble::tibble(
    id = rep(seq_len(N), each = length(mu)),
    group = factor(rep(c("A", "B", "C"), times = N), levels = c("A", "B", "C")),
    outcome = as.numeric(t(Xb))
  )

  spec_bad <- comp_spec(df_bad) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    diagnose() |>
    test()

  p_bad <- as.numeric(spec_bad$diagnostics$sphericity$p)[1]
  expect_true(is.finite(p_bad))
  expect_lt(p_bad, 0.05)
  expect_identical(spec_bad$fitted$metric[1], "GG")
  expect_true(any(grepl("GG correction", spec_bad$fitted$notes[[1]])))

  # Respect user preference for HF when provided
  spec_bad_hf <- comp_spec(df_bad) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    set_engine_options(correction = "HF") |>
    diagnose() |>
    test()
  expect_identical(spec_bad_hf$fitted$metric[1], "HF")
  expect_true(any(grepl("HF correction", spec_bad_hf$fitted$notes[[1]])))

  # Respect user request for uncorrected despite violation
  spec_bad_none <- comp_spec(df_bad) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    set_engine_options(correction = "none") |>
    diagnose() |>
    test()
  expect_identical(spec_bad_none$fitted$metric[1], "uncorrected")
})

test_that("anova_repeated works with non-standard group column names", {
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    wave = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12)
  )
  meta <- list(
    roles = list(outcome = "outcome", group = "wave", id = "id"),
    diagnostics = list(notes = character()),
    settings = list()
  )
  res <- tidycomp:::engine_anova_repeated(df, meta)
  expect_true(all(c("statistic", "df1", "df2", "p.value") %in% names(res)))
  expect_false(is.na(res$statistic))
  expect_false(is.na(res$df1))
  expect_false(is.na(res$df2))
  expect_false(is.na(res$p.value))
})

# Friedman --------------------------------------------------------------------

test_that("friedman engine matches stats::friedman.test", {
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 2, 4, 6, 3, 6, 9, 4, 8, 12)
  )
  meta <- make_meta()
  res <- tidycomp:::engine_friedman(df, meta)
  fit <- stats::friedman.test(outcome ~ group | id, data = df)
  expect_equal(res$statistic, unname(fit$statistic))
  expect_equal(res$p.value, unname(fit$p.value))
})

# Student t-test -------------------------------------------------------------

test_that("Student t engine matches pooled variance t-test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_student_t(df, meta)
  base <- stats::t.test(outcome ~ group, data = df, var.equal = TRUE)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$df, unname(base$parameter))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, diff(rev(base$estimate)))
})

# Mann-Whitney ---------------------------------------------------------------

test_that("Mann-Whitney engine matches stats::wilcox.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 2, 3, 4, 5),
    group = factor(rep(c("A", "B"), each = 4))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_mann_whitney(df, meta)
  base <- stats::wilcox.test(
    outcome ~ group,
    data = df,
    conf.int = TRUE,
    exact = FALSE,
    correct = TRUE
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, unname(base$estimate))
  expect_equal(
    c(res$conf.low, res$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

test_that("mann_whitney records note when CI is unavailable (has_ci = FALSE)", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney")

  testthat::with_mocked_bindings(
    wilcox.test = function(...) {
      list(
        method = "Wilcoxon rank sum test",
        statistic = structure(5, names = "W"),
        p.value = 0.5,
        estimate = 0,
        conf.int = NULL
      )
    },
    .package = "stats",
    {
      res <- test(spec)

      expect_true(
        any(grepl(
          "CI unavailable \\(ties/small sample/edge case\\)\\.",
          res$fitted$notes[[1]]
        ))
      )
      expect_true(all(is.na(c(res$fitted$conf.low, res$fitted$conf.high))))
    }
  )
})

# Paired t-test ---------------------------------------------------------------

test_that("paired t engine matches stats::t.test with paired = TRUE", {
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    outcome = c(1, 4, 2, 2, 3, 10, 11, 12, 12, 16),
    group = factor(rep(c("A", "B"), times = 5))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_paired_t(df, meta)
  wide <- tidyr::pivot_wider(
    df,
    id_cols = "id",
    names_from = "group",
    values_from = "outcome"
  )
  base <- stats::t.test(wide$B, wide$A, paired = TRUE)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$df, unname(base$parameter))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, unname(base$estimate))
  expect_equal(
    c(res$conf.low, res$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

# Wilcoxon signed-rank --------------------------------------------------------

test_that("wilcoxon_signed_rank engine matches stats::wilcox.test paired", {
  df <- tibble::tibble(
    id = rep(1:6, each = 2),
    outcome = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7),
    group = factor(rep(c("A", "B"), times = 6))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_wilcoxon_signed_rank(df, meta)
  wide <- tidyr::pivot_wider(
    df,
    id_cols = "id",
    names_from = "group",
    values_from = "outcome"
  )
  base <- stats::wilcox.test(
    wide$B,
    wide$A,
    paired = TRUE,
    conf.int = TRUE,
    exact = FALSE,
    correct = TRUE
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, unname(base$estimate))
  expect_equal(
    c(res$conf.low, res$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

test_that("wilcoxon_signed_rank records note when CI is unavailable (has_ci = FALSE)", {
  df <- tibble::tibble(
    id = rep(1:3, each = 2),
    outcome = c(1, 2, 3, 4, 5, 6),
    group = factor(rep(c("A", "B"), times = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("wilcoxon_signed_rank")

  testthat::with_mocked_bindings(
    wilcox.test = function(...) {
      list(
        method = "Wilcoxon signed rank test",
        statistic = structure(5, names = "V"),
        p.value = 0.5,
        estimate = 0,
        conf.int = NULL # This makes has_ci = FALSE
      )
    },
    .package = "stats",
    {
      res <- test(spec)

      expect_true(
        any(grepl(
          "CI unavailable \\(ties/small sample/edge case\\)\\.",
          res$fitted$notes[[1]]
        ))
      )
      expect_true(all(is.na(c(res$fitted$conf.low, res$fitted$conf.high))))
    }
  )
})


test_that("anova_repeated computes p-value when afex table lacks it", {
  df <- tibble::tibble(
    id = rep(1:2, each = 2),
    group = factor(rep(c("A", "B"), times = 2)),
    outcome = c(1, 2, 3, 4)
  )
  meta <- make_meta()
  meta$diagnostics$sphericity <- 0.5

  F <- 2
  df1 <- 1
  df2 <- 10
  expected <- stats::pf(F, df1, df2, lower.tail = FALSE)

  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      tidycomp:::engine_anova_repeated(df, meta),
      aov_ez = function(...) {
        list(
          anova_table = data.frame(
            `num Df` = df1,
            `den Df` = df2,
            `MSE` = 1,
            `F` = F,
            `Pr(>F)` = NA_real_,
            check.names = FALSE
          )
        )
      },
      .package = "afex"
    ),
    is_installed = function(...) TRUE,
    .package = "rlang"
  )

  expect_true(is.finite(res$p.value))
  expect_equal(res$p.value, expected)
})

# Contingency table engines --------------------------------------------------

test_that("fisher_exact engine matches stats::fisher.test", {
  df <- tibble::tibble(
    outcome = factor(c("yes", "no", "yes", "no", "yes", "no")),
    group = factor(c("A", "A", "B", "B", "A", "B"))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_fisher_exact(df, meta)
  base <- stats::fisher.test(table(df$group, df$outcome))
  expect_equal(res$p.value, unname(base$p.value))
})

test_that("chisq_yates engine matches stats::chisq.test", {
  df <- tibble::tibble(
    outcome = factor(c(
      rep("yes", 5),
      rep("no", 5),
      rep("yes", 5),
      rep("no", 5)
    )),
    group = factor(rep(c("A", "B"), each = 10))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_chisq_yates(df, meta)
  base <- stats::chisq.test(table(df$group, df$outcome), correct = TRUE)
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$p.value, unname(base$p.value))
})

test_that("chisq_nxn engine matches stats::chisq.test", {
  df <- tidyr::expand_grid(
    outcome = factor(c("a", "b", "c")),
    group = factor(c("G1", "G2", "G3"))
  ) |>
    tidyr::uncount(5)
  meta <- make_meta()
  res <- tidycomp:::engine_chisq_nxn(df, meta)
  base <- stats::chisq.test(table(df$group, df$outcome), correct = FALSE)
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$p.value, unname(base$p.value))
})

test_that("mcnemar_chi2 engine matches stats::mcnemar.test", {
  df <- tibble::tibble(
    id = rep(1:30, each = 2),
    group = factor(rep(c("A", "B"), times = 30)),
    outcome = factor(c(
      rep(c("yes", "no"), 15),
      rep(c("no", "yes"), 15)
    ))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_mcnemar_chi2(df, meta)
  wide <- tidyr::pivot_wider(df, names_from = group, values_from = outcome)
  base <- stats::mcnemar.test(table(wide$A, wide$B), correct = FALSE)
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, 1)
  expect_false(is.na(res$conf.low))
  expect_false(is.na(res$conf.high))
})

test_that("mcnemar_chi2_cc engine matches continuity-corrected test", {
  df <- tibble::tibble(
    id = rep(1:30, each = 2),
    group = factor(rep(c("A", "B"), times = 30)),
    outcome = factor(c(
      rep(c("yes", "no"), 15),
      rep(c("no", "yes"), 15)
    ))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_mcnemar_chi2_cc(df, meta)
  wide <- tidyr::pivot_wider(df, names_from = group, values_from = outcome)
  base <- stats::mcnemar.test(table(wide$A, wide$B), correct = TRUE)
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$p.value, unname(base$p.value))
})

test_that("mcnemar_exact engine matches exact2x2::exact2x2", {
  skip_if_not_installed("exact2x2")
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = factor(c(
      "yes",
      "no",
      "yes",
      "no",
      "yes",
      "yes",
      "no",
      "no",
      "yes",
      "no"
    ))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_mcnemar_exact(df, meta)
  wide <- tidyr::pivot_wider(df, names_from = group, values_from = outcome)
  base <- exact2x2::exact2x2(
    table(wide$A, wide$B),
    paired = TRUE,
    alternative = "two.sided",
    tsmethod = "central",
    conf.level = 0.95,
    midp = FALSE
  )
  expect_equal(res$p.value, unname(base$p.value))
  expect_false(is.na(res$conf.low))
  expect_false(is.na(res$conf.high))
})

test_that("mcnemar_exact falls back when exact2x2 is missing", {
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = factor(c(
      "yes",
      "no",
      "yes",
      "no",
      "yes",
      "yes",
      "no",
      "no",
      "yes",
      "no"
    ))
  )

  meta <- make_meta()

  res <- testthat::with_mocked_bindings(
    tidycomp:::engine_mcnemar_exact(df, meta),
    requireNamespace = function(pkg, quietly = TRUE) FALSE,
    .package = "base" # <-- mock in base namespace
  )

  expect_true(any(grepl("exact2x2 not installed", unlist(res$notes))))
})

test_that("mcnemar OR uses continuity correction when exact2x2 is unavailable", {
  df <- tibble::tibble(
    id = rep(1:12, each = 2),
    group = factor(rep(c("A", "B"), times = 12)),
    outcome = factor(c(
      rep(c("no", "yes"), 11),
      "yes",
      "yes"
    ))
  )

  meta <- make_meta()

  # Simulate missing exact2x2 by mocking requireNamespace in base
  res <- testthat::with_mocked_bindings(
    tidycomp:::engine_mcnemar_exact(df, meta),
    requireNamespace = function(pkg, quietly = TRUE) FALSE,
    .package = "base" # <-- mock in base namespace
  )

  # b = 0, c = 11 -> corrected OR = 11 / 0.5 = 22
  expect_equal(res$estimate, 22)
})
