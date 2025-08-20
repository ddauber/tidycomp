library(testthat)
library(tidycomp)

df <- tibble::tibble(
  outcome = c(1,2,3,4, 5,6,7,8, 9,10,11,12),
  group = factor(rep(c("A","B","C"), each = 4))
)

test_that("post_hoc defaults to pairwise.t.test", {
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    test() |>
    post_hoc()

  expect_s3_class(spec$post_hoc, "tbl_df")
  expect_equal(unique(spec$post_hoc$method), "pairwise.t.test")
})

 test_that("set_post_hoc overrides method", {
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    set_post_hoc(method = "tukey") |>
    test() |>
    post_hoc()

  expect_true(any(grepl("Tukey", spec$post_hoc$method)))
})

test_that("post_hoc skips when omnibus not significant", {
  df2 <- tibble::tibble(
    outcome = rep(1:4, 3),
    group = factor(rep(c("A","B","C"), each = 4))
  )
  spec <- comp_spec(df2) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    test() |>
    post_hoc()

  expect_equal(nrow(spec$post_hoc), 0)
  expect_true(isTRUE(attr(spec$post_hoc, "skipped")))
})

test_that("kruskal-wallis uses pairwise.wilcox.test by default", {
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("kruskal_wallis") |>
    test() |>
    post_hoc()

  expect_equal(unique(spec$post_hoc$method), "pairwise.wilcox.test")
})

df_bin <- tibble::tibble(
  outcome = factor(c(1,1,0,0,1,0,1,0,1,0,1,0), levels = c(0,1)),
  group = factor(rep(c("A","B","C"), each = 4))
)

test_that("binary outcomes use pairwise.prop.test", {
  spec <- comp_spec(df_bin) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("binary") |>
    set_engine("chisq_nxn") |>
    test() |>
    post_hoc()

  expect_equal(unique(spec$post_hoc$method), "pairwise.prop.test")
})
