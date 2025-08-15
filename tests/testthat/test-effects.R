library(testthat)
library(tidycomp)

# default resolution without set_effects()

test_that("effects() uses engine hint when no set_effects()", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    test() |>
    effects()

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "d")
})

# automatic computation during test()

test_that("set_effects(compute=TRUE) triggers computation in test()", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    set_effects(compute = TRUE)

  spec <- test(spec)

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "d")
})

# effects() runs test() if needed

test_that("effects() runs test() on spec if needed", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t")

  spec <- effects(spec)

  expect_s3_class(spec$fitted, "comp_result")
  expect_s3_class(spec$effects, "tbl_df")
})

# effects on fitted result with attached model

test_that("effects() works on a fitted result", {
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

  fit <- spec$fitted
  es <- effects(fit)

  expect_s3_class(es, "tbl_df")
  expect_false(is.null(attr(es, "model")))
})

# effects on repeated-measures ANOVA

test_that("effects() locates model for repeated measures", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    test()

  fit <- spec$fitted
  es <- effects(fit)

  expect_s3_class(es, "tbl_df")
  expect_false(is.null(attr(es, "model")))
})

# Cohen's d for paired designs
test_that("effects() computes Cohen's d for paired design", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = c(3, 4, 4, 6, 5, 9, 6, 7, 7, 10)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("paired_t") |>
    test() |>
    effects()

  wide <- tidycomp:::.standardize_paired_numeric(df, "outcome", "group", "id")
  g <- names(wide)
  expected <- effectsize::cohens_d(wide[[g[2]]], wide[[g[1]]], paired = TRUE)

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$estimate, expected$Cohens_d)
})

# Rank-biserial for paired Wilcoxon tests
test_that("effects() computes rank-biserial for paired design", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = c(3, 4, 4, 6, 5, 9, 6, 7, 7, 10)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("wilcoxon_signed_rank") |>
    test() |>
    effects()

  wide <- tidycomp:::.standardize_paired_numeric(df, "outcome", "group", "id")
  g <- names(wide)
  expected <- effectsize::rank_biserial(
    wide[[g[2]]],
    wide[[g[1]]],
    paired = TRUE
  )

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$estimate, expected[[1]])
})

# Kendall's W for Friedman tests
test_that("effects() computes Kendall's W for Friedman design", {
  skip_if_not_installed("effectsize")

  set.seed(1)
  df <- tibble::tibble(
    id = rep(1:5, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 5)),
    outcome = rnorm(15)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("friedman") |>
    test() |>
    effects()

  expected <- effectsize::kendalls_w(outcome ~ group | id, data = df)

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$estimate, expected[[1]])
})

# Additional parity tests for effect sizes ------------------------------------

for (eng in c("welch_t", "student_t")) {
  test_that(paste0("effects() computes Cohen's d for ", eng), {
    skip_if_not_installed("effectsize")

    df <- tibble::tibble(
      outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
      group = factor(rep(c("A", "B"), each = 4))
    )

    spec <- comp_spec(df) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine(eng) |>
      test() |>
      effects()

    expected <- effectsize::cohens_d(outcome ~ group, data = df)
    expect_equal(spec$effects$estimate, expected$Cohens_d)
  })
}

test_that("effects() computes rank-biserial for mann_whitney", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 2, 3, 4, 5),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney") |>
    test() |>
    effects()

  rb <- effectsize::rank_biserial(outcome ~ group, data = df)
  est <- rb$r_rank_biserial
  if (is.null(est)) {
    est <- rb$Rank_biserial
  }
  if (is.null(est)) {
    est <- rb[[1]]
  }
  expect_equal(spec$effects$estimate, est)
})

test_that("effects() computes omega squared for one-way ANOVA", {
  skip_if_not_installed("effectsize")

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

  # Fit a comparable ANOVA model for reference
  expected <- effectsize::omega_squared(aov(outcome ~ group, data = df))

  # Get the relevant column name
  col <- intersect(c("Omega2", "omega.sq", "omega_sq"), names(expected))[1]
  expect_true(length(col) == 1, info = "No matching omega-squared column found")

  spec <- effects(spec)

  expect_equal(
    spec$effects$estimate,
    expected[[col]][1],
    tolerance = 1e-8
  )
})

test_that("effects() computes omega squared for Welch ANOVA", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_welch") |>
    test() |>
    effects(type = "omega2")
  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "omega2")
  expect_false(is.na(spec$effects$estimate))
})

test_that("effects() computes rank epsilon squared for kruskal_wallis", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("kruskal_wallis") |>
    test() |>
    effects()

  expected <- effectsize::rank_epsilon_squared(outcome ~ group, data = df)
  expect_equal(spec$effects$estimate, expected[[1]])
})

test_that("effects() computes generalized eta squared for repeated measures", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    test()

  es <- effectsize::eta_squared(attr(spec$fitted, "model"), generalized = TRUE)
  spec <- effects(spec)
  col <- intersect(
    c(
      "GES",
      "Eta2_G",
      "Eta2_generalized",
      "eta.sq.gen",
      "eta_sq_generalized",
      "Eta2"
    ),
    names(es)
  )[1]
  expect_equal(spec$effects$estimate, es[[col]][1])
})
