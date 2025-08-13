# comp_spec ---------------------------------------------------------------

test_that("comp_spec validates input and initializes structure", {
  expect_error(comp_spec(1:3), "data frame")
  spec <- comp_spec(mtcars)
  expect_s3_class(spec, "comp_spec")
  expect_named(
    spec,
    c(
      "data_raw",
      "data_prepared",
      "roles",
      "design",
      "outcome_type",
      "strategy",
      "engine",
      "diagnostics",
      "prep_log",
      "fitted"
    )
  )
  expect_true(tibble::is_tibble(spec$data_raw))
  expect_equal(spec$strategy, "auto")
})

# set_roles ---------------------------------------------------------------

test_that("set_roles captures specified columns", {
  spec <- comp_spec(mtcars)
  spec <- suppressMessages(set_roles(spec, outcome = mpg, group = am))
  expect_equal(spec$roles$outcome, "mpg")
  expect_equal(spec$roles$group, "am")
  expect_error(
    suppressMessages(set_roles(spec, outcome = not_here, group = am)),
    "not found"
  )
})


# set_design --------------------------------------------------------------

test_that("set_design stores valid design and rejects others", {
  spec <- comp_spec(mtcars)
  spec <- suppressMessages(set_design(spec, "paired"))
  expect_equal(spec$design, "paired")
  expect_error(
    suppressMessages(set_design(spec, "unknown")),
    "must be one of"
  )
})

# set_outcome_type -------------------------------------------------------

test_that("set_outcome_type records type and validates input", {
  spec <- comp_spec(mtcars)
  spec <- suppressMessages(set_outcome_type(spec, "numeric"))
  expect_equal(spec$outcome_type, "numeric")
  expect_error(
    suppressMessages(set_outcome_type(spec, "funky")),
    "must be one of"
  )
})

# set_strategy -----------------------------------------------------------

test_that("set_strategy records strategy and validates input", {
  spec <- comp_spec(mtcars)
  spec <- suppressMessages(set_strategy(spec, "robust"))
  expect_equal(spec$strategy, "robust")
  expect_error(
    suppressMessages(set_strategy(spec, "spaceship")),
    "must be one of"
  )
})

# set_engine -------------------------------------------------------------

test_that("set_engine assigns known engines and rejects unknown ones", {
  spec <- comp_spec(mtcars)
  spec <- suppressMessages(set_engine(spec, "welch_t"))
  expect_equal(spec$engine, "welch_t")
  expect_error(
    suppressMessages(set_engine(spec, "coffee_machine")),
    "Unknown engine"
  )
})

# print.comp_spec --------------------------------------------------------

test_that("print.comp_spec shows key fields", {
  spec <- comp_spec(mtcars)
  spec <- suppressMessages(set_roles(spec, mpg, am))
  spec <- suppressMessages(set_design(spec, "independent"))
  spec <- suppressMessages(set_outcome_type(spec, "numeric"))
  out <- capture.output(print(spec))
  expect_equal(out[1], "<comp_spec>")
  expect_equal(out[2], "  roles: outcome, group")
  expect_equal(out[3], "  design: independent")
  expect_equal(out[4], "  outcome_type: numeric")
  expect_equal(out[5], "  strategy: auto")
  expect_equal(out[6], "  engine: auto")
})

# %||% -------------------------------------------------------------------

test_that("%||% returns fallback for NULL", {
  fallback <- tidycomp:::`%||%`
  expect_equal(fallback(NULL, "a"), "a")
  expect_equal(fallback("b", "a"), "b")
})

test_that("set_roles() captures optional id and weights when supplied (tidy-eval)", {
  df <- tibble::tibble(
    id = 1:8,
    weights = c(1, 2, 1, 3, 1, 2, 1, 3),
    group = factor(rep(c("A", "B"), each = 4)),
    outcome = c(1, 2, 3, 4, 2, 3, 4, 5)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id, weights = weights)

  # id/weights branches executed -> stored as strings in roles
  expect_identical(spec$roles$outcome, "outcome")
  expect_identical(spec$roles$group, "group")
  expect_identical(spec$roles$id, "id")
  expect_identical(spec$roles$weights, "weights")
})

test_that("set_roles() leaves id and weights NULL when omitted", {
  df <- tibble::tibble(
    id = 1:6,
    w = c(1, 1, 2, 2, 3, 3),
    group = factor(c("A", "A", "A", "B", "B", "B")),
    outcome = 1:6
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) # no id/weights

  expect_null(spec$roles$id)
  expect_null(spec$roles$weights)
})
