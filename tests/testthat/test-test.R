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

test_that("test() supports paired design when id provided", {
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = c(1, 4, 2, 2, 3, 10, 11, 12, 12, 16)
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("paired") |>
      set_outcome_type("numeric")
  )
  res <- suppressMessages(test(spec))
  expect_s3_class(res$fitted, "comp_result")
  expect_equal(res$fitted$engine, "paired_t")
})

test_that("test() supports repeated design when id provided", {
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = rnorm(12)
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("repeated") |>
      set_outcome_type("numeric")
  )
  res <- suppressMessages(test(spec))
  expect_true(res$fitted$engine %in% c("anova_repeated", "anova_repeated_base"))
})

test_that("test() requires id for paired design", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(outcome = mpg, group = am) |>
      set_design("paired") |>
      set_outcome_type("numeric")
  )
  expect_error(test(spec), "id role")
})

test_that("test() requires id for repeated design", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(outcome = mpg, group = cyl) |>
      set_design("repeated") |>
      set_outcome_type("numeric")
  )
  expect_error(test(spec), "id role")
})

# removed: test for unsupported designs as repeated design is now supported

test_that("test() errors when binary outcome column is not factor", {
  spec <- suppressMessages(
    comp_spec(mtcars) |>
      set_roles(outcome = mpg, group = am) |>
      set_design("independent") |>
      set_outcome_type("binary")
  )
  expect_error(test(spec), "factor")
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

test_that("independent design with >2 groups defaults to anova_oneway_welch", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "anova_oneway_welch")
})

test_that("parametric strategy uses anova_oneway_equal engine for >2 groups", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_strategy("parametric")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "anova_oneway_equal")
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

test_that("can use wilcoxon_signed_rank engine for paired data", {
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 6)
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("paired") |>
      set_outcome_type("numeric") |>
      set_engine("wilcoxon_signed_rank")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "wilcoxon_signed_rank")
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
    var_bf_p = NA,
    sphericity = NULL,
    notes = character()
  )
  expect_warning(suppressMessages(test(spec)), "mann_whitney")
})

test_that("nudge toward Friedman when sphericity violated", {
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = rnorm(12)
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("repeated") |>
      set_outcome_type("numeric")
  )
  spec$diagnostics <- list(
    group_sizes = tibble(n = c(4, 4, 4)),
    normality = tibble(p_shapiro = c(0.5, 0.5, 0.5)),
    var_bf_p = NA,
    sphericity = tibble(Effect = "group", p = 0.01),
    notes = character()
  )
  expect_warning(suppressMessages(test(spec)), "friedman")
})

test_that("can use kruskal_wallis engine for independent multi-group data", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine("kruskal_wallis")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "kruskal_wallis")
})

test_that("can use friedman engine for repeated data", {
  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 2, 4, 6, 3, 6, 9, 4, 8, 12)
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("repeated") |>
      set_outcome_type("numeric") |>
      set_engine("friedman")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "friedman")
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

test_that("test() errors when `design` is NULL or invalid", {
  # Minimal comp_spec double that satisfies earlier guards
  make_spec <- function(design) {
    structure(
      list(
        roles = list(outcome = "y", group = "g"),
        design = design,
        outcome_type = "numeric",
        strategy = "auto",
        data_raw = data.frame(y = 1:4, g = rep(0:1, 2))
      ),
      class = "comp_spec"
    )
  }

  # 1) NULL design triggers the guard
  spec_null <- make_spec(NULL)
  expect_error(
    test(spec_null),
    "currently supports `design = 'independent'`, 'paired', or 'repeated'",
    fixed = FALSE
  )

  # 2) Invalid design value triggers the same guard
  spec_bad <- make_spec("wrong_value")
  expect_error(
    test(spec_bad),
    "currently supports `design = 'independent'`, 'paired', or 'repeated'",
    fixed = FALSE
  )
})

test_that("warns on severe non-normality with very small n (anova_oneway)", {
  testthat::local_reproducible_output(unicode = FALSE)
  withr::local_options(cli.width = 1e6)

  fake_diag <- list(
    group_sizes = data.frame(n = c(10, 20, 25)),
    normality = data.frame(p_shapiro = c(0.005, 0.5, 0.6))
  )
  df <- data.frame(y = rnorm(27), g = factor(rep(c("A", "B", "C"), each = 9)))

  spec <- structure(
    list(
      roles = list(outcome = "y", group = "g"),
      design = "independent",
      outcome_type = "numeric",
      strategy = "auto", # selector may pick Welch
      diagnostics = fake_diag,
      data_raw = df
    ),
    class = "comp_spec"
  )

  testthat::with_mocked_bindings(
    .tidycomp_engines = function() {
      list(
        anova_oneway = function(...) list(dummy = TRUE),
        anova_oneway_welch = function(...) list(dummy = TRUE)
      )
    },
    .env = asNamespace("tidycomp"),
    {
      expect_warning(
        test(spec),
        regexp = "\\Q`set_engine('kruskal_wallis')`\\E"
      )
    }
  )
})

# Binary outcome engine selection --------------------------------------------

test_that("small counts trigger fisher_exact", {
  df <- tibble::tibble(
    outcome = factor(c("yes", "no", "yes", "no")),
    group = factor(c("A", "A", "B", "B"))
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("binary")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "fisher_exact")
})

test_that("adequate counts use chisq_yates", {
  df <- tibble::tibble(
    outcome = factor(c(rep("yes", 5), rep("no", 5), rep("yes", 5), rep("no", 5))),
    group = factor(rep(c("A", "B"), each = 10))
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("binary")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "chisq_yates")
})

test_that("multi-level table uses chisq_nxn", {
  df <- tidyr::expand_grid(
    outcome = factor(c("a", "b", "c")),
    group = factor(c("G1", "G2", "G3"))
  ) |> tidyr::uncount(5)
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("binary")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "chisq_nxn")
})

test_that("paired binary uses chi2 when counts sufficient", {
  df <- tibble::tibble(
    id = rep(1:30, each = 2),
    group = factor(rep(c("A", "B"), times = 30)),
    outcome = factor(c(rep(c("yes", "no"), 15), rep(c("no", "yes"), 15)))
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("paired") |>
      set_outcome_type("binary")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "mcnemar_chi2")
})

test_that("paired binary with small counts uses mcnemar_exact", {
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = factor(c("yes", "no", "yes", "no", "yes", "yes", "no", "no", "yes", "no"))
  )
  spec <- suppressMessages(
    comp_spec(df) |>
      set_roles(outcome = outcome, group = group, id = id) |>
      set_design("paired") |>
      set_outcome_type("binary")
  )
  res <- suppressMessages(test(spec))
  expect_equal(res$fitted$engine, "mcnemar_exact")
})
