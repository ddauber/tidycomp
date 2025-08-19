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

test_that("diagnose reports contingency info for binary outcomes", {
  df <- tibble::tibble(
    outcome = factor(c("yes", "no", "yes", "no")),
    group = factor(c("A", "A", "B", "B"))
  )
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("binary")
  spec <- diagnose(spec)
  expect_named(spec$diagnostics, c("table", "expected", "engine", "notes"))
  expect_equal(spec$diagnostics$engine, "fisher_exact")
})

test_that("diagnose warns about zero cells in unpaired contingency tables", {
  df <- tibble::tibble(
    outcome = factor(c("yes", "yes", "no", "no")),
    group = factor(c("A", "A", "B", "B"))
  )
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("binary")
  expect_warning(spec <- diagnose(spec), "Zero cell detected")
  expect_true(any(grepl("Zero cell", spec$diagnostics$notes)))
})

test_that("diagnose warns about zero cells in paired contingency tables", {
  df <- tibble::tibble(
    id = c(1, 1, 2, 2),
    group = factor(c("A", "B", "A", "B")),
    outcome = factor(c("yes", "yes", "yes", "no"))
  )
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("binary")
  expect_warning(spec <- diagnose(spec), "Zero cell detected")
  expect_true(any(grepl("Zero cell", spec$diagnostics$notes)))
})

test_that("diagnose errors when outcome is not numeric", {
  # Create a spec with non-numeric outcome
  df <- tibble::tibble(
    outcome = c("low", "medium", "high", "low", "medium", "high"),
    group = factor(c("A", "A", "B", "B", "A", "B"))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")

  expect_error(
    diagnose(spec),
    "supports numeric or binary outcomes",
    class = "rlang_error"
  )
})

test_that("diagnose treats two-level non-numeric outcomes as binary", {
  df_factor <- tibble::tibble(
    outcome = factor(c("yes", "no", "yes", "no")),
    group = factor(c("A", "A", "B", "B"))
  )

  spec_factor <- comp_spec(df_factor) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")

  spec_factor <- diagnose(spec_factor)
  expect_named(
    spec_factor$diagnostics,
    c("table", "expected", "engine", "notes")
  )

  # Test with logical outcome
  df_logical <- tibble::tibble(
    outcome = c(TRUE, FALSE, TRUE, FALSE),
    group = factor(c("A", "A", "B", "B"))
  )

  spec_logical <- comp_spec(df_logical) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")

  spec_logical <- diagnose(spec_logical)
  expect_named(
    spec_logical$diagnostics,
    c("table", "expected", "engine", "notes")
  )
})

test_that("diagnose computes sphericity p-value for repeated design", {
  testthat::skip_if_not_installed("afex")
  testthat::skip_if_not_installed("performance")
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
