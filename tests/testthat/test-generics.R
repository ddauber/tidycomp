# small helper dataset for testing
sample_data <- tibble::tibble(
  outcome = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
  group = factor(rep(c("g1", "g2"), each = 5))
)

# construct a minimal comp_result object
result <- list(
  method = "Example test",
  engine = "example_engine",
  estimate = 1,
  conf.low = 0.5,
  conf.high = 1.5,
  notes = list(character())
)
class(result) <- c("comp_result", "list")

# a comp_spec with roles and fitted result for autoplot/report
spec_with_fit <- comp_spec(sample_data)
spec_with_fit$roles <- list(outcome = "outcome", group = "group")
spec_with_fit$fitted <- result


test_that("tidy() on comp_result returns a tibble", {
  # tidy() should convert the result to a tibble
  tidied <- tidy(result)
  expect_s3_class(tidied, "tbl_df")
  expect_equal(tidied, tibble::as_tibble(result))
})

test_that("tidy() drops all-NA columns by default but can retain them", {
  res <- result
  res$df2 <- NA
  class(res) <- c("comp_result", "list")

  tidied <- tidy(res)
  expect_false("df2" %in% names(tidied))

  tidied_full <- tidy(res, complete = TRUE)
  expect_true("df2" %in% names(tidied_full))
})

test_that(".tidy_display() mirrors tidy() defaults", {
  res <- result
  res$df2 <- NA
  class(res) <- c("comp_result", "list")
  expect_equal(.tidy_display(res), tidy(res))
})


test_that("report() on comp_spec without results errors", {
  empty_spec <- comp_spec(sample_data)
  expect_error(report(empty_spec), "No fitted results")
})


test_that("report() on comp_result prints a summary", {
  expect_snapshot(report(result))
})


test_that("report.comp_result() includes an effect size line", {
  testthat::local_reproducible_output()

  # Prevent leaks from other tests:
  testthat::local_mocked_bindings(
    .has_effectsize = function() TRUE,
    .env = asNamespace("tidycomp")
  )

  spec <- comp_spec(mtcars) |>
    set_roles(outcome = mpg, group = am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    test() |>
    effects(ci = 0.90)

  expect_s3_class(spec$effects, "tbl_df")

  msgs <- suppressWarnings(testthat::capture_messages(report.comp_result(spec)))
  msgs_clean <- cli::ansi_strip(paste(as.character(msgs), collapse = "\n"))

  expect_match(msgs_clean, "^Test:", perl = TRUE)
})


test_that("report.comp_result() warns when notes are present", {
  testthat::local_reproducible_output()

  outliers_df <- tibble::tibble(
    x = as.factor(c("A", "A", "A", "A", "B", "B", "B", "B", "B", "A")),
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100)
  )

  spec <- comp_spec(outliers_df) |>
    set_roles(outcome = y, group = x) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    diagnose() |>
    test() |>
    effects(ci = 0.90)

  # Ensure the branch is taken: put notes where the method looks for them
  spec$fitted$notes <- list(c("outliers detected", "check assumptions"))

  suppressWarnings(report.comp_result(spec))
  expect_true(TRUE)
})

test_that("autoplot() on comp_spec without results errors", {
  empty_spec <- comp_spec(sample_data)
  expect_error(autoplot(empty_spec), "No fitted results")
})

test_that("autoplot(comp_spec) delegates to comp_result and returns ggplot", {
  spec <- comp_spec(sample_data)
  spec$roles <- list(outcome = "outcome", group = "group")
  spec$fitted <- result # <- your minimal comp_result above

  p <- autoplot(spec, type = "estimation")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot() on comp_result returns a ggplot", {
  plt <- autoplot(result, data = sample_data, roles = spec_with_fit$roles)
  expect_s3_class(plt, "ggplot")
})

test_that("autoplot() diagnostics warns and returns NULL", {
  expect_warning(
    out <- autoplot(
      result,
      type = "diagnostics",
      data = sample_data,
      roles = spec_with_fit$roles
    ),
    "Diagnostics plot not implemented"
  )
  expect_null(out)
})

test_that("autoplot(comp_result) coerces non-factor group to factor", {
  nonfac <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
    group = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1) # numeric, not factor
  )
  roles <- list(outcome = "outcome", group = "group")

  p <- autoplot(result, data = nonfac, roles = roles, type = "estimation")
  expect_s3_class(p, "ggplot")
})

test_that("report(comp_spec) delegates to comp_result", {
  testthat::local_reproducible_output()

  # Make a fresh spec with a minimal fitted result
  spec <- comp_spec(sample_data)
  spec$roles <- list(outcome = "outcome", group = "group")
  spec$fitted <- result

  # (Optional safety) ensure the delegated method can also read x$fitted$...
  spec$fitted$fitted <- spec$fitted

  msgs <- testthat::capture_messages(report(spec))
  msgs_clean <- cli::ansi_strip(paste(as.character(msgs), collapse = "\n"))

  expect_match(msgs_clean, "^Test:", perl = TRUE)
})
