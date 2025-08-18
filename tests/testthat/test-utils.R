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
# .standardize_paired_numeric()
# -----------------------------------------------------------------------------

test_that(".standardize_paired_numeric returns wide tibble", {
  data <- tibble::tibble(
    id = rep(1:3, each = 2),
    g = factor(rep(c("A", "B"), times = 3)),
    y = c(1, 2, 3, 4, 5, 6)
  )
  res <- tidycomp:::.standardize_paired_numeric(data, "y", "g", "id")
  expect_s3_class(res, "tbl_df")
  expect_equal(ncol(res), 2)
})

test_that(".standardize_paired_numeric validates pairing", {
  data_bad <- tibble::tibble(
    id = c(1, 1, 2),
    g = factor(c("A", "A", "B")),
    y = 1:3
  )
  expect_error(
    tidycomp:::.standardize_paired_numeric(data_bad, "y", "g", "id"),
    "Each id must appear exactly twice"
  )
})

test_that(".standardize_paired_numeric validates numeric outcome", {
  data_bad <- tibble::tibble(
    id = rep(1:3, each = 2),
    g = factor(rep(c("A", "B"), times = 3)),
    y = letters[1:6] # character instead of numeric
  )
  expect_error(
    tidycomp:::.standardize_paired_numeric(data_bad, "y", "g", "id"),
    "Outcome must be numeric for the current engine"
  )
})

test_that(".standardize_paired_numeric validates id is provided", {
  data <- tibble::tibble(
    g = factor(rep(c("A", "B"), times = 3)),
    y = 1:6
  )
  expect_error(
    tidycomp:::.standardize_paired_numeric(data, "y", "g", NULL),
    "Paired design requires an `id` role"
  )
})

test_that(".standardize_paired_numeric validates group has exactly 2 levels", {
  # Test with 1 level
  data_one_level <- tibble::tibble(
    id = rep(1:3, each = 2),
    g = factor(rep("A", times = 6)),
    y = 1:6
  )
  expect_error(
    tidycomp:::.standardize_paired_numeric(data_one_level, "y", "g", "id"),
    "Group must have exactly 2 levels for this engine"
  )

  # Test with 3 levels
  data_three_levels <- tibble::tibble(
    id = rep(1:3, each = 2),
    g = factor(rep(c("A", "B", "C"), times = 2)),
    y = 1:6
  )
  expect_error(
    tidycomp:::.standardize_paired_numeric(data_three_levels, "y", "g", "id"),
    "Group must have exactly 2 levels for this engine"
  )
})

test_that(".standardize_paired_numeric validates each id has one observation per group", {
  data_bad <- tibble::tibble(
    id = c(1, 1, 2, 2), # Each id appears exactly twice
    g = factor(c("A", "B", "A", "A")), # But id=2 has 2 A's and 0 B's
    y = 1:4
  )
  expect_error(
    tidycomp:::.standardize_paired_numeric(data_bad, "y", "g", "id"),
    "Each id must have one observation for each group"
  )
})

test_that(".standardize_paired_numeric validates no missing outcomes", {
  data_missing <- tibble::tibble(
    id = rep(1:3, each = 2),
    g = factor(rep(c("A", "B"), times = 3)),
    y = c(1, 2, NA, 4, 5, 6) # missing value for id=2, group=A
  )
  expect_error(
    tidycomp:::.standardize_paired_numeric(data_missing, "y", "g", "id"),
    "Missing outcomes for at least one id"
  )
})

test_that(".standardize_paired_numeric converts non-factor group to factor", {
  data_char_group <- tibble::tibble(
    id = rep(1:3, each = 2),
    g = rep(c("A", "B"), times = 3), # character, not factor
    y = 1:6
  )
  # This should not error - it should convert to factor
  res <- tidycomp:::.standardize_paired_numeric(data_char_group, "y", "g", "id")
  expect_s3_class(res, "tbl_df")
  expect_equal(ncol(res), 2)
})

# -----------------------------------------------------------------------------
# .standardize_multi_group_numeric()
# -----------------------------------------------------------------------------

test_that(".standardize_multi_group_numeric handles >2 groups", {
  data <- data.frame(y = 1:9, g = rep(c("A", "B", "C"), each = 3))
  res <- tidycomp:::.standardize_multi_group_numeric(data, "y", "g")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("outcome", "group"))
  expect_equal(nlevels(res$group), 3)
})

# -----------------------------------------------------------------------------
# .standardize_repeated_numeric()
# -----------------------------------------------------------------------------

test_that(".standardize_repeated_numeric validates structure", {
  data <- tibble::tibble(
    id = rep(1:3, each = 3),
    g = factor(rep(c("A", "B", "C"), times = 3)),
    y = rnorm(9)
  )
  res <- tidycomp:::.standardize_repeated_numeric(data, "y", "g", "id")
  expect_s3_class(res, "tbl_df")
  expect_equal(ncol(res), 3)
})


test_that(".standardize_repeated_numeric returns expected tibble", {
  data <- tibble::tibble(
    id = rep(1:3, each = 3),
    g = factor(rep(c("A", "B", "C"), times = 3)),
    y = as.numeric(1:9)
  )
  res <- tidycomp:::.standardize_repeated_numeric(data, "y", "g", "id")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("outcome", "group", "id"))
  expect_true(is.numeric(res$outcome))
  expect_true(is.factor(res$group))
  expect_equal(nlevels(res$group), 3)
})

test_that(".standardize_repeated_numeric converts non-factor group to factor", {
  data <- tibble::tibble(
    id = rep(1:2, each = 3),
    g = rep(c("A", "B", "C"), times = 2), # character
    y = rnorm(6)
  )
  res <- tidycomp:::.standardize_repeated_numeric(data, "y", "g", "id")
  expect_s3_class(res, "tbl_df")
  expect_true(is.factor(res$group))
})

test_that(".standardize_repeated_numeric validates numeric outcome", {
  data <- tibble::tibble(
    id = rep(1:2, each = 3),
    g = factor(rep(c("A", "B", "C"), times = 2)),
    y = letters[1:6] # not numeric
  )
  expect_error(
    tidycomp:::.standardize_repeated_numeric(data, "y", "g", "id"),
    "\\QOutcome must be numeric for the current engine.\\E"
  )
})

test_that(".standardize_repeated_numeric validates id is provided", {
  data <- tibble::tibble(
    g = factor(rep(c("A", "B"), times = 3)),
    y = 1:6
  )
  expect_error(
    tidycomp:::.standardize_repeated_numeric(data, "y", "g", NULL),
    "\\QRepeated measures design requires an `id` role.\\E"
  )
})

test_that(".standardize_repeated_numeric requires >= 2 group levels", {
  # Single group level
  data_one <- tibble::tibble(
    id = rep(1:3, each = 1),
    g = factor(rep("A", 3)),
    y = rnorm(3)
  )
  expect_error(
    tidycomp:::.standardize_repeated_numeric(data_one, "y", "g", "id"),
    "\\QGroup must have at least 2 levels for this engine.\\E"
  )
})

test_that(".standardize_repeated_numeric errors if any id lacks a level", {
  # id = 2 is missing "C" and has "A" duplicated
  data_missing <- tibble::tibble(
    id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    g = factor(c("A", "B", "C", "A", "A", "B", "A", "B", "C")),
    y = rnorm(9)
  )

  expect_error(
    tidycomp:::.standardize_repeated_numeric(data_missing, "y", "g", "id"),
    "\\QEach id must have one observation for each group.\\E"
  )
})

test_that(".standardize_repeated_numeric errors on duplicated id-group rows", {
  # id=1, group=A appears twice (n == 2)
  data_dup <- tibble::tibble(
    id = c(1, 1, 1, 1, 2, 2, 2),
    g = factor(c("A", "A", "B", "C", "A", "B", "C")),
    y = rnorm(7)
  )
  expect_error(
    tidycomp:::.standardize_repeated_numeric(data_dup, "y", "g", "id"),
    "\\QEach id must have one observation for each group.\\E"
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
# .brown_forsythe()
# -----------------------------------------------------------------------------

test_that(".brown_forsythe computes p-value for multiple groups", {
  y <- c(1, 2, 1, 2, 1, 1, 4, 1, 4)
  g <- rep(c("A", "B", "C"), each = 3)
  p <- tidycomp:::.brown_forsythe(y, g)
  expect_type(p, "double")
  expect_false(is.na(p))
})

test_that(".brown_forsythe returns NA when <2 groups", {
  y <- 1:3
  g <- rep("A", 3)
  expect_true(is.na(tidycomp:::.brown_forsythe(y, g)))
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

test_that(".extract_sphericity_p returns NA when as_tibble fails", {
  sp <- function() NULL
  res <- tidycomp:::.extract_sphericity_p(sp)
  expect_type(res, "double")
  expect_length(res, 1)
  expect_identical(res, NA_real_)
})

test_that(".extract_sphericity_p returns NA when required columns missing", {
  sp <- tibble::tibble(effect = "group", p = 0.1)
  res <- tidycomp:::.extract_sphericity_p(sp)
  expect_type(res, "double")
  expect_length(res, 1)
  expect_identical(res, NA_real_)
})

test_that(".extract_sphericity_p returns NA when effect not present", {
  sp <- tibble::tibble(Effect = "other", p = 0.1)
  res <- tidycomp:::.extract_sphericity_p(sp)
  expect_type(res, "double")
  expect_length(res, 1)
  expect_identical(res, NA_real_)
})

test_that(".extract_sphericity_p works with check_sphericity output", {
  skip_if_not_installed("performance")
  skip_if_not_installed("afex")
  data <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = rnorm(12)
  )
  mod <- afex::aov_ez("id", "outcome", data, within = "group")
  sp <- performance::check_sphericity(mod)
  res <- tidycomp:::.extract_sphericity_p(sp)
  expect_type(res, "double")
  expect_length(res, 1)
})

test_that(".standardize_multi_group_numeric aborts when outcome is non-numeric", {
  data <- data.frame(y = letters[1:4], g = rep(c("A", "B"), each = 2))
  expect_error(
    tidycomp:::.standardize_multi_group_numeric(data, "y", "g"),
    "Outcome must be numeric"
  )
})

test_that(".standardize_multi_group_numeric aborts when group has <2 levels", {
  data <- data.frame(y = 1:3, g = rep("A", 3))
  expect_error(
    tidycomp:::.standardize_multi_group_numeric(data, "y", "g"),
    "Group must have at least 2 levels"
  )
})
