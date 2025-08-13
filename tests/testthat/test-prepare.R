# --- prepare() --------------------------------------------------------------

test_that("prepare() passes data through when no steps are supplied", {
  spec <- comp_spec(mtcars)

  expect_message(
    res <- prepare(spec),
    "No preparation steps supplied"
  )

  # The prepared data should be identical to the raw data
  expect_equal(res$data_prepared, spec$data_raw)
  # The preparation log should exist but be empty
  expect_s3_class(res$prep_log, "tbl_df")
  expect_equal(nrow(res$prep_log), 0)
})

test_that("prepare() applies steps sequentially and logs each one", {
  spec <- comp_spec(mtcars)

  # Define two simple steps for testing
  step_add_col <- function(df, spec) {
    list(
      df = dplyr::mutate(df, added = 1),
      log = tibble::tibble(step = "add_col")
    )
  }
  step_double_col <- function(df, spec) {
    list(
      df = dplyr::mutate(df, added = added * 2),
      log = tibble::tibble(step = "double_col")
    )
  }

  res <- prepare(spec, steps = list(step_add_col, step_double_col))

  # The final data contains the transformed column
  expect_equal(res$data_prepared$added[1], 2)
  # Two steps should be logged in order
  expect_equal(res$prep_log$step, c("add_col", "double_col"))
})

# --- step_trim_outliers() ---------------------------------------------------

test_that("step_trim_outliers() removes outliers when action = 'remove'", {
  df <- tibble::tibble(x = c(1:10, 100))
  spec <- comp_spec(df)
  step <- step_trim_outliers(x, method = "iqr", k = 1.5, action = "remove")

  res <- step(df, spec)

  # One row (the outlier) should be removed
  expect_equal(nrow(res$df), 10)
  expect_equal(res$log$n_changed, 1)
})

test_that("step_trim_outliers() winsorizes values when action = 'winsorize'", {
  df <- tibble::tibble(x = c(rnorm(10), 100))
  spec <- comp_spec(df)
  step <- step_trim_outliers(x, method = "iqr", k = 1.5, action = "winsorize")

  res <- step(df, spec)

  # Row count should remain unchanged
  expect_equal(nrow(res$df), nrow(df))
  # Extreme value should be reduced toward the fence
  expect_lt(max(res$df$x), max(df$x))
  expect_gt(res$log$n_changed, 0)
})

test_that("step_trim_outliers() errors on non-numeric variables", {
  df <- tibble::tibble(x = letters[1:5])
  spec <- comp_spec(df)
  step <- step_trim_outliers(x, method = "iqr", action = "remove")

  expect_error(step(df, spec), "requires a numeric variable")
})
