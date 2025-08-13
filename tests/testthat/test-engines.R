# Tests for internal analysis engines
# These tests verify that each engine produces results consistent
# with the corresponding base R statistical tests and that the
# registry exposes the expected engines.

# helper to build meta object used by engines
make_meta <- function() {
  list(
    roles = list(outcome = "outcome", group = "group"),
    diagnostics = list(notes = character()),
    settings = list()
  )
}

test_that("engine registry lists available engines", {
  engines <- tidycomp:::.tidycomp_engines()
  expect_named(engines, c("welch_t", "student_t", "mann_whitney"))
  purrr::walk(engines, ~ expect_true(is.function(.x)))
})

# Welch t-test ---------------------------------------------------------------

test_that("Welch t engine matches stats::t.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_welch_t(df, meta)
  base <- stats::t.test(outcome ~ group, data = df, var.equal = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$n, nrow(df))
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$df, unname(base$parameter))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, diff(rev(base$estimate)))
  expect_equal(
    c(res$conf.low, res$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})

# Student t-test -------------------------------------------------------------

test_that("Student t engine matches pooled variance t-test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_student_t(df, meta)
  base <- stats::t.test(outcome ~ group, data = df, var.equal = TRUE)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$df, unname(base$parameter))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, diff(rev(base$estimate)))
})

# Mann-Whitney ---------------------------------------------------------------

test_that("Mann-Whitney engine matches stats::wilcox.test", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 2, 3, 4, 5),
    group = factor(rep(c("A", "B"), each = 4))
  )
  meta <- make_meta()
  res <- tidycomp:::engine_mann_whitney(df, meta)
  base <- stats::wilcox.test(
    outcome ~ group,
    data = df,
    conf.int = TRUE,
    exact = FALSE,
    correct = TRUE
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(res$statistic, unname(base$statistic))
  expect_equal(res$p.value, unname(base$p.value))
  expect_equal(res$estimate, unname(base$estimate))
  expect_equal(
    c(res$conf.low, res$conf.high),
    unname(base$conf.int),
    ignore_attr = TRUE
  )
})


test_that("mann_whitney records note when CI is unavailable (has_ci = FALSE)", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney")

  testthat::with_mocked_bindings(
    wilcox.test = function(...) {
      list(
        method = "Wilcoxon rank sum test",
        statistic = structure(5, names = "W"),
        p.value = 0.5,
        estimate = 0,
        conf.int = NULL
      )
    },
    .package = "stats",
    {
      res <- test(spec)

      expect_true(
        any(grepl(
          "CI unavailable \\(ties/small sample/edge case\\)\\.",
          res$fitted$notes[[1]]
        ))
      )
      expect_true(all(is.na(c(res$fitted$conf.low, res$fitted$conf.high))))
    }
  )
})
