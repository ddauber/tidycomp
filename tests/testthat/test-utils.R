# -----------------------------------------------------------------------------
# .validate_cols()
# -----------------------------------------------------------------------------

# This ensures no error occurs if the data frame contains the needed columns.

test_that(".validate_cols passes when required columns exist", {
  data <- data.frame(a = 1, b = 2)
  expect_no_error(tidycomp:::.validate_cols(data, c("a", "b")))
})

# errors with a helpful message when columns are missing

test_that(".validate_cols aborts when columns are missing", {
  data <- data.frame(a = 1)
  expect_error(
    tidycomp:::.validate_cols(data, c("a", "b")),
    "Missing required column"
  )
})

# -----------------------------------------------------------------------------
# .capture_role()
# -----------------------------------------------------------------------------

# returns the name of a valid column supplied via tidy-eval

# Here we capture the name of column `a` and expect it to be returned as a string.

test_that(".capture_role resolves a valid column", {
  data <- data.frame(a = 1, b = 2)
  expect_identical(tidycomp:::.capture_role(data, a), "a")
})

# errors when the column is missing or not provided

test_that(".capture_role aborts on missing or unknown columns", {
  data <- data.frame(a = 1)
  expect_error(tidycomp:::.capture_role(data, b), "Column `b` not found")
  expect_error(
    tidycomp:::.capture_role(data, NULL),
    "A valid, unquoted column name is required"
  )
})

# -----------------------------------------------------------------------------
# .standardize_two_group_numeric()
# -----------------------------------------------------------------------------

# selects and renames columns, enforcing numeric outcome and two-level factor

# A minimal data set with a numeric outcome and two groups is standardized.
# We check the resulting column types and names.

test_that(".standardize_two_group_numeric returns expected tibble", {
  data <- data.frame(y = 1:4, g = rep(c("A", "B"), each = 2))
  res <- tidycomp:::.standardize_two_group_numeric(data, "y", "g")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("outcome", "group"))
  expect_true(is.numeric(res$outcome))
  expect_true(is.factor(res$group))
  expect_equal(levels(res$group), c("A", "B"))
})

# validates types and level counts

test_that(".standardize_two_group_numeric validates input types", {
  data_bad_outcome <- data.frame(
    y = letters[1:4],
    g = rep(c("A", "B"), each = 2)
  )
  expect_error(
    tidycomp:::.standardize_two_group_numeric(data_bad_outcome, "y", "g"),
    "Outcome must be numeric"
  )

  data_bad_group <- data.frame(y = 1:4, g = c("A", "B", "C", "A"))
  expect_error(
    tidycomp:::.standardize_two_group_numeric(data_bad_group, "y", "g"),
    "Group must have exactly 2 levels"
  )
})

# -----------------------------------------------------------------------------
# .flag_outliers()
# -----------------------------------------------------------------------------

# computes fences for each supported method

# For a simple numeric vector, we verify that the lower and upper fences
# match the expected values under IQR, MAD, and SD rules.

test_that(".flag_outliers returns expected fences", {
  x <- 1:10
  iqr_res <- tidycomp:::.flag_outliers(x, method = "iqr")
  mad_res <- tidycomp:::.flag_outliers(x, method = "mad")
  sd_res <- tidycomp:::.flag_outliers(x, method = "sd")

  expect_equal(iqr_res$lo, -10.25)
  expect_equal(iqr_res$hi, 21.25)

  expect_equal(mad_res$lo, -2)
  expect_equal(mad_res$hi, 13)

  expect_equal(sd_res$lo, -3.582951, tolerance = 1e-6)
  expect_equal(sd_res$hi, 14.58295, tolerance = 1e-6)
})

# handles vectors without finite values

test_that(".flag_outliers returns NA when no finite values", {
  x <- c(NA, Inf, -Inf)
  res <- tidycomp:::.flag_outliers(x)
  expect_true(is.na(res$lo))
  expect_true(is.na(res$hi))
})

# -----------------------------------------------------------------------------
# .brown_forsythe_2g()
# -----------------------------------------------------------------------------

# returns a p-value for two groups

# Using a small example where the group variances differ, we expect
# a specific p-value from the Brown–Forsythe test implementation.

test_that(".brown_forsythe_2g computes a p-value for two groups", {
  y <- c(1, 2, 1, 2, 1, 1, 4, 1, 4, 1)
  g <- rep(c("A", "B"), each = 5)
  p <- tidycomp:::.brown_forsythe_2g(y, g)
  expect_type(p, "double")
  expect_equal(p, 0.3319086, tolerance = 1e-6)
})

# returns NA when more than two groups are supplied

test_that(".brown_forsythe_2g returns NA for != 2 groups", {
  y <- 1:3
  g <- c("A", "B", "C")
  expect_true(is.na(tidycomp:::.brown_forsythe_2g(y, g)))
})

test_that(".capture_role errors for empty string column name", {
  df <- tibble::tibble(a = 1:3)

  expect_error(
    tidycomp:::.capture_role(df, !!rlang::sym("")), # empty symbol
    "A valid, unquoted column name is required\\.",
    fixed = FALSE
  )
})

test_that(".capture_role errors when as_name returns empty string", {
  df <- tibble::tibble(outcome = c(1, 2, 3))

  testthat::with_mocked_bindings(
    as_name = function(...) "",
    .package = "rlang",
    {
      expect_error(
        .capture_role(df, outcome),
        "A valid, unquoted column name is required\\.",
        class = "rlang_error"
      )
    }
  )
})
