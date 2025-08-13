# These tests cover input validation, engine selection, and diagnostic warnings
# for the main `test()` workflow.

library(tibble)

test_that("test() requires a comp_spec", {
  expect_error(test("not a spec"), "comp_spec")
})

test_that("test() requires outcome and group roles", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_design("independent") |>
      set_outcome_type("numeric")
  )
  expect_error(test(spec), "set_roles")
})

test_that("test() only supports independent design", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(outcome = mpg, group = am) |>
      set_design("paired") |>
      set_outcome_type("numeric")
  )
  expect_error(test(spec), "independent")
})

test_that("test() requires numeric outcome type", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(outcome = mpg, group = am) |>
      set_design("independent") |>
      set_outcome_type("binary")
  )
  expect_error(test(spec), "numeric")
})

test_that("default engine is welch_t and warns if diagnostics missing", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(outcome = mpg, group = am) |>
      set_design("independent") |>
      set_outcome_type("numeric")
  )
  expect_warning(res <- suppressMessages(test(spec)), "diagnose")
  expect_s3_class(res$fitted, "comp_result")
  expect_equal(res$fitted$engine, "welch_t")
})

test_that("parametric strategy uses student_t engine", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(outcome = mpg, group = am) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_strategy("parametric")
  )
  expect_warning(res <- suppressMessages(test(spec)), "diagnose")
  expect_equal(res$fitted$engine, "student_t")
})

test_that("nudge toward Mann-Whitney for small n and non-normal data", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(outcome = mpg, group = am) |>
      set_design("independent") |>
      set_outcome_type("numeric")
  )
  spec$diagnostics <- list(
    group_sizes = tibble(n = c(10, 14)),
    normality = tibble(p_shapiro = c(0.001, 0.001)),
    notes = character()
  )
  expect_warning(suppressMessages(test(spec)), "mann_whitney")
})

test_that("test() errors when engine registry lacks the selected engine", {
  # Start with a valid engine
  spec <- comp_spec(mtcars) |>
    set_roles(outcome = mpg, group = am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("student_t")

  # Temporarily remove all engines from the registry so lookup returns NULL
  testthat::local_mocked_bindings(
    .tidycomp_engines = function() list(), # no engines available
    .env = asNamespace("tidycomp")
  )

  expect_error(
    test(spec),
    regexp = "Selected engine `student_t` not available\\.",
    fixed = FALSE
  )
})
