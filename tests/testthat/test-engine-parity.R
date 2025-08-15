# Ensure that comp_spec with explicit engines matches base R results

# Welch t-test ----------------------------------------------------------------

test_that("welch_t via comp_spec matches stats::t.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    test()

  base <- stats::t.test(outcome ~ group, data = df, var.equal = FALSE)
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$df, unname(base$parameter))
  expect_equal(fit$p.value, unname(base$p.value))
  expect_equal(fit$estimate, diff(rev(base$estimate)))
  expect_equal(
    c(fit$conf.low, fit$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

# Student t-test --------------------------------------------------------------

test_that("student_t via comp_spec matches stats::t.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("student_t") |>
    test()

  base <- stats::t.test(outcome ~ group, data = df, var.equal = TRUE)
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$df, unname(base$parameter))
  expect_equal(fit$p.value, unname(base$p.value))
  expect_equal(fit$estimate, diff(rev(base$estimate)))
  expect_equal(
    c(fit$conf.low, fit$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

# Mann-Whitney ----------------------------------------------------------------

test_that("mann_whitney via comp_spec matches stats::wilcox.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 2, 3, 4, 5),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney") |>
    test()

  base <- stats::wilcox.test(
    outcome ~ group,
    data = df,
    conf.int = TRUE,
    exact = FALSE,
    correct = TRUE
  )
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$p.value, unname(base$p.value))
  expect_equal(fit$estimate, unname(base$estimate))
  expect_equal(
    c(fit$conf.low, fit$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

# Paired t-test ---------------------------------------------------------------

test_that("paired_t via comp_spec matches stats::t.test paired", {
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    outcome = c(1, 4, 2, 2, 3, 10, 11, 12, 12, 16),
    group = factor(rep(c("A", "B"), times = 5))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("paired_t") |>
    test()

  wide <- tidyr::pivot_wider(
    df,
    id_cols = "id",
    names_from = "group",
    values_from = "outcome"
  )
  base <- stats::t.test(wide$B, wide$A, paired = TRUE)
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$df, unname(base$parameter))
  expect_equal(fit$p.value, unname(base$p.value))
  expect_equal(fit$estimate, unname(base$estimate))
  expect_equal(
    c(fit$conf.low, fit$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

# Wilcoxon signed-rank -------------------------------------------------------

test_that("wilcoxon_signed_rank via comp_spec matches stats::wilcox.test paired", {
  df <- tibble::tibble(
    id = rep(1:6, each = 2),
    outcome = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7),
    group = factor(rep(c("A", "B"), times = 6))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("wilcoxon_signed_rank") |>
    test()

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
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$p.value, unname(base$p.value))
  expect_equal(fit$estimate, unname(base$estimate))
  expect_equal(
    c(fit$conf.low, fit$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

# One-way ANOVA (equal variances) --------------------------------------------

test_that("anova_oneway_equal via comp_spec matches stats::oneway.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    test()

  base <- stats::oneway.test(outcome ~ group, data = df, var.equal = TRUE)
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$df1, unname(base$parameter[1]))
  expect_equal(fit$df2, unname(base$parameter[2]))
  expect_equal(fit$p.value, unname(base$p.value))
})

# One-way ANOVA (Welch) ------------------------------------------------------

test_that("anova_oneway_welch via comp_spec matches stats::oneway.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_welch") |>
    test()

  base <- stats::oneway.test(outcome ~ group, data = df, var.equal = FALSE)
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$df1, unname(base$parameter[1]))
  expect_equal(fit$df2, unname(base$parameter[2]))
  expect_equal(fit$p.value, unname(base$p.value))
})

# Kruskal-Wallis -------------------------------------------------------------

test_that("kruskal_wallis via comp_spec matches stats::kruskal.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("kruskal_wallis") |>
    test()

  base <- stats::kruskal.test(outcome ~ group, data = df)
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$p.value, unname(base$p.value))
})

# Repeated-measures ANOVA ----------------------------------------------------

test_that("anova_repeated via comp_spec matches afex::aov_ez (uncorrected)", {
  testthat::skip_if_not_installed("afex")

  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(2, 4, 3, 4, 8, 7, 7, 8, 9, 10, 11, 12)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    set_engine_options(
      correction = "none",
      return_df = "uncorrected"
    ) |>
    test()

  # Reference with the same backend + options
  af <- afex::aov_ez(
    id = "id",
    dv = "outcome",
    within = "group",
    data = df,
    anova_table = list(correction = "none", es = "none"),
    type = 3
  )
  tab <- as.data.frame(af$anova_table)
  F_ref <- unname(tab["group", "F"])
  p_ref <- unname(tab["group", "Pr(>F)"])

  fit <- spec$fitted
  expect_equal(fit$statistic, F_ref, tolerance = 1e-8)
  expect_equal(fit$p.value, p_ref, tolerance = 1e-12)
})

# Friedman -------------------------------------------------------------------

test_that("friedman via comp_spec matches stats::friedman.test", {
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 2, 4, 6, 3, 6, 9, 4, 8, 12)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("friedman") |>
    test()

  base <- stats::friedman.test(outcome ~ group | id, data = df)
  fit <- spec$fitted

  expect_equal(fit$statistic, unname(base$statistic))
  expect_equal(fit$p.value, unname(base$p.value))
})
