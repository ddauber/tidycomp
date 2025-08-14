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

# paired designs --------------------------------------------------------------

test_that("effects() computes Cohen's d for paired data", {
  df <- tibble::tibble(
    id = rep(1:4, each = 2),
    group = rep(c("A", "B"), times = 4),
    outcome = c(1, 2, 2, 4, 3, 6, 4, 8)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("paired_t") |>
    test() |>
    effects()

  df_wide <- tidycomp:::.standardize_paired_numeric(df, "outcome", "group", "id")
  g <- names(df_wide)
  diffs <- df_wide[[g[2]]] - df_wide[[g[1]]]
  manual_d <- mean(diffs) / stats::sd(diffs)

  expect_equal(spec$effects$estimate, manual_d)
})

test_that("effects() computes rank-biserial for paired data", {
  df <- tibble::tibble(
    id = rep(1:4, each = 2),
    group = rep(c("A", "B"), times = 4),
    outcome = c(1, 2, 2, 4, 3, 6, 4, 8)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("wilcoxon_signed_rank") |>
    test() |>
    effects()

  expect_equal(spec$effects$estimate, 1)
})
