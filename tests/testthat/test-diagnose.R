test_that("diagnose attaches diagnostics with expected structure", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(mpg, am) |>
      set_design("independent") |>
      set_outcome_type("numeric")
  )
  spec <- suppressMessages(diagnose(spec))

  expect_named(
    spec$diagnostics,
    c("group_sizes", "normality", "var_bf_p", "sphericity", "notes")
  )
  expect_s3_class(spec$diagnostics$group_sizes, "tbl_df")
  expect_s3_class(spec$diagnostics$normality, "tbl_df")
  expect_true(is.numeric(spec$diagnostics$var_bf_p))
  expect_type(spec$diagnostics$notes, "character")
})

test_that("diagnose errors when outcome is not numeric", {
  # Create a spec with non-numeric outcome
  df <- tibble::tibble(
    outcome = c("low", "medium", "high", "low", "medium", "high"), # character outcome
    group = factor(c("A", "A", "B", "B", "A", "B"))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") # This claims it's numeric but it's not

  # This should trigger the error on line 53
  expect_error(
    diagnose(spec),
    "`diagnose\\(\\)` currently supports numeric outcomes\\.",
    class = "rlang_error"
  )
})

test_that("diagnose errors with different non-numeric outcome types", {
  # Test with factor outcome
  df_factor <- tibble::tibble(
    outcome = factor(c("low", "medium", "high", "low", "medium", "high")),
    group = factor(c("A", "A", "B", "B", "A", "B"))
  )

  spec_factor <- comp_spec(df_factor) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")

  expect_error(
    diagnose(spec_factor),
    "`diagnose\\(\\)` currently supports numeric outcomes\\.",
    class = "rlang_error"
  )

  # Test with logical outcome
  df_logical <- tibble::tibble(
    outcome = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    group = factor(c("A", "A", "B", "B", "A", "B"))
  )

  spec_logical <- comp_spec(df_logical) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")

  expect_error(
    diagnose(spec_logical),
    "`diagnose\\(\\)` currently supports numeric outcomes\\.",
    class = "rlang_error"
  )
})

test_that("diagnose computes sphericity p-value for repeated design", {
  testthat::skip_if_not_installed("afex")
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = rnorm(12)
  )
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric")
  spec <- diagnose(spec)
  expect_s3_class(spec$diagnostics$sphericity, "tbl_df")
  expect_true(is.numeric(spec$diagnostics$sphericity$p[1]))
})
