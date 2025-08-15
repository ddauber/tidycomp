library(testthat)
library(tidycomp)

test_that("engines attach fitted model", {
  df_two <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  df_three <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  df_paired <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = c(1, 4, 2, 2, 3, 10, 11, 12, 12, 16)
  )

  df_repeated <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12)
  )

  specs <- list(
    welch_t = comp_spec(df_two) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine("welch_t"),
    student_t = comp_spec(df_two) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine("student_t"),
    mann_whitney = comp_spec(df_two) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine("mann_whitney"),
    paired_t = comp_spec(df_paired) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("paired") |>
      set_outcome_type("numeric") |>
      set_engine("paired_t"),
    wilcoxon_signed_rank = comp_spec(df_paired) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("paired") |>
      set_outcome_type("numeric") |>
      set_engine("wilcoxon_signed_rank"),
    anova_oneway_equal = comp_spec(df_three) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine("anova_oneway_equal"),
    anova_oneway_welch = comp_spec(df_three) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine("anova_oneway_welch"),
    kruskal_wallis = comp_spec(df_three) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine("kruskal_wallis"),
    anova_repeated = comp_spec(df_repeated) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("repeated") |>
      set_outcome_type("numeric") |>
      set_engine("anova_repeated"),
    anova_repeated_base = comp_spec(df_repeated) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("repeated") |>
      set_outcome_type("numeric") |>
      set_engine("anova_repeated_base"),
    friedman = comp_spec(df_repeated) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("repeated") |>
      set_outcome_type("numeric") |>
      set_engine("friedman")
  )

  for (sp in specs) {
    fit_spec <- test(sp)
    expect_false(is.null(attr(fit_spec$fitted, "model")))
  }
})
