# effects() should error when called before test() has populated spec$fitted
# -------------------------------------------------------------------------
test_that("effects() errors if no fitted results are present", {
  spec <- comp_spec(mtcars)
  spec <- set_roles(spec, outcome = mpg, group = am)
  expect_snapshot(error = TRUE, effects(spec))
})

# Engine-specific effect size calculations
# ----------------------------------------

# Student's t-test returns Cohen's d ----

test_that("effects() computes Cohen's d for student_t engine (active)", {
  out <-
    comp_spec(mtcars) |>
    set_roles(outcome = mpg, group = am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("student_t") |>
    test() |>
    effects(conf_level = 0.90)

  expect_equal(out$fitted$es_metric, "Cohens_d")
  expect_type(out$fitted$es_value, "double")
  expect_type(out$fitted$es_conf_low, "double")
  expect_type(out$fitted$es_conf_high, "double")
})

# Welch's t-test defaults to Hedges' g ----
test_that("effects() computes Hedges g for welch_t engine", {
  out <- comp_spec(mtcars) |>
    set_roles(outcome = mpg, group = am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    test() |>
    effects(conf_level = 0.90)

  expect_equal(out$fitted$es_metric, "Hedges_g")
  expect_type(out$fitted$es_value, "double")
  expect_type(out$fitted$es_conf_low, "double")
  expect_type(out$fitted$es_conf_high, "double")
})

# Welch's t-test can compute Cohen's d when requested
# ---------------------------------------------------
test_that("effects() allows Cohen's d via effect argument", {
  out <-
    comp_spec(mtcars) |>
    set_roles(outcome = mpg, group = am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    test() |>
    effects(effect = "cohens_d", conf_level = 0.90)

  expect_equal(out$fitted$es_metric, "Cohens_d")
  expect_type(out$fitted$es_value, "double")
  expect_type(out$fitted$es_conf_low, "double")
  expect_type(out$fitted$es_conf_high, "double")
})

# Mann-Whitney uses rank biserial correlation ----
test_that("effects() computes rank biserial for mann_whitney engine", {
  out <-
    comp_spec(mtcars) |>
    set_roles(outcome = mpg, group = am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney") |>
    test() |>
    effects(conf_level = 0.90)

  expect_equal(out$fitted$es_metric, "r_Wilcoxon")
  expect_type(out$fitted$es_value, "double")
  expect_type(out$fitted$es_conf_low, "double")
  expect_type(out$fitted$es_conf_high, "double")
})

# Paired t and Wilcoxon signed-rank ----

test_that("effects() computes Cohen's d for paired_t engine", {
  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = c(1, 4, 2, 2, 3, 10, 11, 12, 12, 16)
  )
  out <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("paired_t") |>
    test() |>
    effects(conf_level = 0.90)
  expect_equal(out$fitted$es_metric, "Cohens_d")
  expect_type(out$fitted$es_value, "double")
})

test_that("effects() computes rank biserial for wilcoxon_signed_rank engine", {
  df <- tibble::tibble(
    id = rep(1:6, each = 2),
    group = factor(rep(c("A", "B"), times = 6)),
    outcome = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7)
  )
  out <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("wilcoxon_signed_rank") |>
    test() |>
    effects(conf_level = 0.90)
  expect_equal(out$fitted$es_metric, "r_Wilcoxon")
  expect_type(out$fitted$es_value, "double")
})

# Multi-group engines ----

test_that("effects() computes eta squared for anova_oneway_equal engine", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")
  spec$fitted <- list(engine = "anova_oneway_equal")
  out <- effects(spec, conf_level = 0.90)
  expect_equal(out$fitted$es_metric, "Eta2")
  expect_type(out$fitted$es_value, "double")
})

test_that("effects() computes omega squared for anova_oneway_welch engine", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")
  spec$fitted <- list(engine = "anova_oneway_welch")
  out <- effects(spec, conf_level = 0.90)
  expect_equal(out$fitted$es_metric, "Omega2")
  expect_type(out$fitted$es_value, "double")
})

test_that("effects() computes epsilon squared for kruskal_wallis engine", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")
  spec$fitted <- list(engine = "kruskal_wallis")
  out <- effects(spec, conf_level = 0.90)
  expect_equal(out$fitted$es_metric, "Epsilon2")
  expect_type(out$fitted$es_value, "double")
})

test_that("effects() computes partial eta squared for anova_repeated engine", {
  set.seed(42)
  df <- tibble::tibble(
    id = rep(1:8, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 8)),
    outcome = rep(rnorm(8, 10, 1), each = 3) + rep(c(0, 0.5, 1), times = 8) + rnorm(24, 0, 0.1)
  )
  out <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    test() |>
    effects(conf_level = 0.90)
  expect_equal(out$fitted$es_metric, "Eta2_partial")
  expect_type(out$fitted$es_value, "double")
})

test_that("effects() computes Kendalls W for friedman engine", {
  df <- tibble::tibble(
    id = rep(1:5, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 5)),
    outcome = c(3, 2, 1, 2, 1, 3, 1, 3, 2, 2, 3, 1, 3, 1, 2)
  )
  out <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("friedman") |>
    test() |>
    effects(conf_level = 0.90)
  expect_equal(out$fitted$es_metric, "Kendalls_W")
  expect_type(out$fitted$es_value, "double")
})

test_that("effects() allows custom effect size functions", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric")
  spec$fitted <- list(engine = "anova_oneway_equal")
  out <- effects(spec, effect = "omega_squared", conf_level = 0.90)
  expect_equal(out$fitted$es_metric, "Omega2")
  expect_type(out$fitted$es_value, "double")
})

# Unknown engines fallback to Hedges' g with a warning ----
test_that("effects() defaults to Hedges g for unknown engine (with warning)", {
  fit <- comp_spec(mtcars) |>
    set_roles(outcome = mpg, group = am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("student_t") |>
    test()

  # Simulate an unknown engine AFTER fitting
  fit$fitted$engine <- "unknown_engine"

  expect_warning(
    out <- effects(fit, conf_level = 0.90),
    regexp = "Unknown engine" # keep it loose to avoid punctuation mismatches
  )

  expect_equal(out$fitted$es_metric, "Hedges_g")
  expect_type(out$fitted$es_value, "double")
  expect_type(out$fitted$es_conf_low, "double")
  expect_type(out$fitted$es_conf_high, "double")
})

# Multi-group engines are standardised correctly ----
test_that("effects() standardizes data for multi-group engines", {
  out <- comp_spec(iris) |>
    set_roles(outcome = Sepal.Length, group = Species) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("kruskal_wallis") |>
    test() |>
    effects(conf_level = 0.90)

  expect_type(out$fitted$es_value, "double")
})

# When effectsize package is not available ----
test_that("effects() warns and returns input when effectsize is absent", {
  testthat::local_mocked_bindings(
    .has_effectsize = function() FALSE,
    .env = asNamespace("tidycomp")
  )

  fit <-
    comp_spec(mtcars) |>
    set_roles(outcome = mpg, group = am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("student_t") |>
    test()

  expect_warning(out <- effects(fit), "effectsize")
  expect_identical(out, fit)
})
