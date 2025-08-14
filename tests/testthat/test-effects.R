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
    outcome = c(1,2,3,4,5,6,7,8,9),
    group = factor(rep(c("A","B","C"), each = 3))
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
    outcome = 1:12
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
  expected <- effectsize::rank_biserial(wide[[g[2]]], wide[[g[1]]], paired = TRUE)

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$estimate, expected$Rank_biserial)
})
