library(testthat)
library(tidycomp)

# default resolution without set_effects()

test_that("effects() uses engine hint when no set_effects()", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    test() |>
    effects()

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "d")
})

# automatic computation during test()

test_that("set_effects(compute=TRUE) triggers computation in test()", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    set_effects(compute = TRUE)

  spec <- test(spec)

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "d")
})

# effects() runs test() if needed

test_that("effects() runs test() on spec if needed", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t")

  spec <- effects(spec)

  expect_s3_class(spec$fitted, "comp_result")
  expect_s3_class(spec$effects, "tbl_df")
})

# effects on fitted result with attached model

test_that("effects() works on a fitted result", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    test()

  fit <- spec$fitted
  es <- effects(fit)

  expect_s3_class(es, "tbl_df")
  expect_false(is.null(attr(es, "model")))
})

# effects on repeated-measures ANOVA

test_that("effects() locates model for repeated measures", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 4, 5, 7, 7, 8, 9, 10, 11, 12)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    test()

  fit <- spec$fitted
  es <- effects(fit)

  expect_s3_class(es, "tbl_df")
  expect_false(is.null(attr(es, "model")))
})

# Cohen's d for paired designs
test_that("effects() computes Cohen's d for paired design", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = c(3, 4, 4, 6, 5, 9, 6, 7, 7, 10)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("paired_t") |>
    test() |>
    effects()

  wide <- tidycomp:::.standardize_paired_numeric(df, "outcome", "group", "id")
  g <- names(wide)
  expected <- effectsize::cohens_d(wide[[g[2]]], wide[[g[1]]], paired = TRUE)

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$estimate, expected$Cohens_d)
})

# Rank-biserial for paired Wilcoxon tests
test_that("effects() computes rank-biserial for paired design", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:5, each = 2),
    group = factor(rep(c("A", "B"), times = 5)),
    outcome = c(3, 4, 4, 6, 5, 9, 6, 7, 7, 10)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("numeric") |>
    set_engine("wilcoxon_signed_rank") |>
    test() |>
    effects()

  wide <- tidycomp:::.standardize_paired_numeric(df, "outcome", "group", "id")
  g <- names(wide)
  expected <- effectsize::rank_biserial(
    wide[[g[2]]],
    wide[[g[1]]],
    paired = TRUE
  )

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$estimate, expected[[1]])
})

# Kendall's W for Friedman tests
test_that("effects() computes Kendall's W for Friedman design", {
  skip_if_not_installed("effectsize")

  set.seed(1)
  df <- tibble::tibble(
    id = rep(1:5, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 5)),
    outcome = rnorm(15)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("friedman") |>
    test() |>
    effects()

  expected <- effectsize::kendalls_w(outcome ~ group | id, data = df)

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$estimate, expected[[1]])
})

# Additional parity tests for effect sizes ------------------------------------

for (eng in c("welch_t", "student_t")) {
  test_that(paste0("effects() computes Cohen's d for ", eng), {
    skip_if_not_installed("effectsize")

    df <- tibble::tibble(
      outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
      group = factor(rep(c("A", "B"), each = 4))
    )

    spec <- comp_spec(df) |>
      set_roles(outcome = outcome, group = group) |>
      set_design("independent") |>
      set_outcome_type("numeric") |>
      set_engine(eng) |>
      test() |>
      effects()

    expected <- effectsize::cohens_d(outcome ~ group, data = df)
    expect_equal(spec$effects$estimate, expected$Cohens_d)
  })
}

test_that("effects() computes rank-biserial for mann_whitney", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 2, 3, 4, 5),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney") |>
    test() |>
    effects()

  rb <- effectsize::rank_biserial(outcome ~ group, data = df)
  est <- rb$r_rank_biserial
  if (is.null(est)) {
    est <- rb$Rank_biserial
  }
  if (is.null(est)) {
    est <- rb[[1]]
  }
  expect_equal(spec$effects$estimate, est)
})

test_that("effects() computes omega squared for one-way ANOVA", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    test()

  # Fit a comparable ANOVA model for reference
  expected <- effectsize::omega_squared(aov(outcome ~ group, data = df))

  # Get the relevant column name
  col <- intersect(c("Omega2", "omega.sq", "omega_sq"), names(expected))[1]
  expect_true(length(col) == 1, info = "No matching omega-squared column found")

  spec <- effects(spec)

  expect_equal(
    spec$effects$estimate,
    expected[[col]][1],
    tolerance = 1e-8
  )
})

test_that("effects() computes omega squared for Welch ANOVA", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_welch") |>
    test() |>
    effects(type = "omega2")
  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "omega2")
  expect_false(is.na(spec$effects$estimate))
})

test_that("effects() computes rank epsilon squared for kruskal_wallis", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("kruskal_wallis") |>
    test() |>
    effects()

  expected <- effectsize::rank_epsilon_squared(outcome ~ group, data = df)
  expect_equal(spec$effects$estimate, expected[[1]])
})

test_that("effects() computes generalized eta squared for repeated measures", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:4, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 4)),
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("anova_repeated") |>
    test()

  es <- effectsize::eta_squared(attr(spec$fitted, "model"), generalized = TRUE)
  spec <- effects(spec)
  col <- intersect(
    c(
      "GES",
      "Eta2_G",
      "Eta2_generalized",
      "eta.sq.gen",
      "eta_sq_generalized",
      "Eta2"
    ),
    names(es)
  )[1]
  expect_equal(spec$effects$estimate, es[[col]][1])
})

test_that(".default_effect_type honors spec$effects_hint", {
  spec <- structure(list(effects_hint = "omega2"), class = "comp_spec")
  out <- tidycomp:::.default_effect_type(
    spec_or_fit = spec,
    fitted = NULL,
    model = NULL
  )
  expect_identical(out, "omega2")
})

test_that(".default_effect_type honors fitted attr 'engine_hint'", {
  fitted <- list()
  attr(fitted, "engine_hint") <- "ges"
  out <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = fitted,
    model = NULL
  )
  expect_identical(out, "ges")
})

test_that(".default_effect_type returns 'ges' for afex/aovlist classes", {
  for (cls in list(
    c("afex_aov"),
    c("Anova.mlm"),
    c("aovlist")
  )) {
    model <- structure(list(), class = cls)
    out <- tidycomp:::.default_effect_type(
      spec_or_fit = list(),
      fitted = NULL,
      model = model
    )
    expect_identical(out, "ges")
  }
})

test_that(".default_effect_type returns 'omega2' for aov/lm", {
  # lm
  mdl_lm <- lm(mpg ~ factor(cyl), data = mtcars)
  out_lm <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = NULL,
    model = mdl_lm
  )
  expect_identical(out_lm, "omega2")

  # aov
  mdl_aov <- aov(mpg ~ factor(cyl), data = mtcars)
  out_aov <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = NULL,
    model = mdl_aov
  )
  expect_identical(out_aov, "omega2")
})

test_that(".default_effect_type returns 'r2' for glm", {
  mdl_glm <- glm(am ~ mpg, data = mtcars, family = binomial())
  out <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = NULL,
    model = mdl_glm
  )
  expect_identical(out, "r2")
})

test_that(".default_effect_type returns 'd' for htest with t statistic", {
  mdl_t <- structure(list(statistic = c(t = 2.3)), class = "htest")
  out <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = NULL,
    model = mdl_t
  )
  expect_identical(out, "d")
})

test_that(".default_effect_type returns 'rank_biserial' for Wilcoxon via chain", {
  set.seed(1)
  df <- tibble::tibble(
    y = c(rnorm(10, 0), rnorm(10, 0.8)),
    g = factor(rep(c("A", "B"), each = 10))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = y, group = g) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney") |>
    test()

  model <- attr(spec$fitted, "model")
  out <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = spec$fitted,
    model = model
  )
  expect_identical(out, "rank_biserial")
})

test_that(".default_effect_type returns current behavior for Friedman via chain", {
  df <- tibble::tibble(
    id = rep(1:8, each = 3),
    g = factor(rep(c("A", "B", "C"), times = 8)),
    y = c(
      rnorm(8, 0),
      rnorm(8, 0.5),
      rnorm(8, 1.0)
    )
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = y, group = g, id = id) |>
    set_design("repeated") |>
    set_outcome_type("numeric") |>
    set_engine("friedman") |>
    test() |>
    effects()

  # model <- attr(spec$fitted, "model") # stats::friedman.test -> htest
  # out <- tidycomp:::.default_effect_type(
  #   spec_or_fit = list(),
  #   fitted = spec$fitted,
  #   model = model
  # )

  expect_identical(spec$effects$type, "kendalls_w")
})

test_that(".default_effect_type falls back to 'eta2' otherwise", {
  mdl_other <- list() # no classes, no method/statistic
  out <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = NULL,
    model = mdl_other
  )
  expect_identical(out, "eta2")
})

test_that("effects() errors for rank epsilon squared when effectsize missing", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("kruskal_wallis") |>
    test()

  testthat::with_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "rlang",
    {
      expect_error(
        effects(spec, type = "epsilon2"),
        "Package 'effectsize' is required for epsilon squared.",
        fixed = TRUE
      )
    }
  )
})

# effects require effectsize for d/g

test_that("effects() errors without effectsize for Cohen's d/g", {
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    group = factor(rep(c("A", "B"), each = 4))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    test()

  testthat::with_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "rlang",
    {
      expect_error(
        effects(spec, type = "d"),
        "Package 'effectsize' is required for Cohen's d / Hedges' g.",
        fixed = TRUE
      )
    }
  )
})

test_that("Cohen's d requires original data/roles when called on a fitted object", {
  # Minimal two-group data
  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6),
    group = factor(rep(c("A", "B"), each = 3))
  )

  # Fit a t-test engine via spec, then drop to the fitted object
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("welch_t") |>
    test()

  fit <- spec$fitted # no parent spec passed to effects()

  expect_error(
    effects(fit, type = "d"),
    "Cohen's d/g require the original data and roles; call effects() on a spec.",
    fixed = TRUE
  )
})

test_that("Hedges's g requires original data/roles when called on a fitted object", {
  # Minimal two-group data
  df <- tibble::tibble(
    outcome = c(10, 11, 12, 20, 21, 22),
    group = factor(rep(c("A", "B"), each = 3))
  )

  # Fit a t-test engine via spec, then drop to the fitted object
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("student_t") |>
    test()

  fit <- spec$fitted # no parent spec passed to effects()

  expect_error(
    effects(fit, type = "g"),
    "Cohen's d/g require the original data and roles; call effects() on a spec.",
    fixed = TRUE
  )
})

test_that("omega squared for Welch ANOVA errors without original data", {
  skip_if_not_installed("effectsize", minimum_version = "0.8.7")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_welch") |>
    test()

  attr(spec$fitted, "model")$method <- "Welch's ANOVA"
  fit <- spec$fitted

  expect_error(
    effects(fit, type = "omega2"),
    "Omega squared for Welch's ANOVA requires the original data",
    fixed = TRUE
  )
})

test_that("effects() computes epsilon squared for one-way ANOVA", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    test()

  expected <- effectsize::epsilon_squared(aov(outcome ~ group, data = df))
  col <- intersect(c("Epsilon2", "epsilon.sq", "epsilon_sq"), names(expected))[
    1
  ]

  spec <- effects(spec, type = "epsilon2")

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "epsilon2")
  expect_equal(spec$effects$estimate, expected[[col]][1], tolerance = 1e-8)
})

test_that("effects() errors for kendalls_w on htest without spec", {
  df <- tibble::tibble(
    id = rep(1:3, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 3)),
    outcome = c(1, 2, 3, 2, 3, 4, 3, 4, 5)
  )

  fit <- list()
  attr(fit, "model") <- stats::friedman.test(outcome ~ group | id, data = df)

  expect_error(
    effects(fit, type = "kendalls_w"),
    "Kendall's W requires the original data and roles; call effects() on a spec.",
    fixed = TRUE
  )
})

test_that(".compute_effects(omega2) on Welch ANOVA without spec errors with helpful message", {
  # Fake an htest object with "Welch" in the method
  welch_htest <- structure(
    list(method = "Welch Two Sample ANOVA"),
    class = "htest"
  )

  expect_error(
    tidycomp:::.compute_effects(
      model = welch_htest,
      type = "omega2",
      conf_level = 0.95,
      parent_spec = NULL
    ),
    "Omega squared for Welch's ANOVA requires the original data; call effects\\(\\) on a spec\\.",
    perl = TRUE
  )
})

test_that(".compute_effects errors when effectsize is not installed", {
  mdl <- lm(mpg ~ cyl, data = mtcars)

  testthat::with_mock(
    `rlang::is_installed` = function(pkg) FALSE,
    {
      expect_error(
        tidycomp:::.compute_effects(
          model = mdl,
          type = "eta2",
          conf_level = 0.95
        ),
        "Package 'effectsize' is required for ANOVA effect sizes."
      )
    }
  )
})


test_that(".compute_effects (Kendall's W) errors when effectsize is unavailable", {
  # Build a real Friedman test result (htest)
  set.seed(1)
  df <- tibble::tibble(
    id = rep(1:6, each = 3),
    g = factor(rep(c("A", "B", "C"), times = 6)),
    y = rnorm(18)
  )
  mdl <- stats::friedman.test(y ~ g | id, data = df)

  # Minimal spec carrying the original data/roles/engine
  parent_spec <- structure(
    list(
      data_raw = df,
      roles = list(outcome = "y", group = "g", id = "id"),
      engine = "friedman"
    ),
    class = "comp_spec"
  )

  # Mock rlang::is_installed() to simulate "effectsize" not installed
  testthat::with_mock(
    `rlang::is_installed` = function(pkg) FALSE,
    {
      expect_error(
        tidycomp:::.compute_effects(
          model = mdl,
          type = "kendalls_w",
          conf_level = 0.95,
          parent_spec = parent_spec
        ),
        regexp = "(?i)Package 'effectsize' is required for Kendall's W\\.", # covers line 463
        perl = TRUE
      )
    }
  )
})

test_that(".compute_effects (Kendall's W) errors when id role is missing", {
  skip_if_not_installed("effectsize") # allow the test to run the next guard

  set.seed(2)
  df <- tibble::tibble(
    id = rep(1:6, each = 3),
    g = factor(rep(c("A", "B", "C"), times = 6)),
    y = rnorm(18)
  )
  mdl <- stats::friedman.test(y ~ g | id, data = df)

  # Deliberately omit the id role to trigger the specific error (lines 469–471)
  parent_spec <- structure(
    list(
      data_raw = df,
      roles = list(outcome = "y", group = "g"), # <-- no id
      engine = "friedman"
    ),
    class = "comp_spec"
  )

  expect_error(
    tidycomp:::.compute_effects(
      model = mdl,
      type = "kendalls_w",
      conf_level = 0.95,
      parent_spec = parent_spec
    ),
    "Kendall's W for Friedman requires an 'id' role in the spec\\.", # lines 469–471
    perl = TRUE
  )
})

test_that("effects() auto-runs test() for an unfitted spec and returns spec with effects", {
  skip_if_not_installed("effectsize") # Cohen's d etc. used by default in two-group t

  # Build an unfitted spec (no test() call on purpose)
  spec <- comp_spec(mtcars) |>
    set_roles(outcome = "mpg", group = "am") |>
    set_design("independent") |>
    set_outcome_type("numeric")

  # Call effects(); this should trigger the auto-fit branch and return a spec
  spec2 <- effects(spec)

  # We get back a spec
  expect_s3_class(spec2, "comp_spec")

  # Auto-fit happened: fitted exists and has a model attached
  expect_false(is.null(spec2$fitted))
  expect_false(is.null(attr(spec2$fitted, "model", exact = TRUE)))

  # Effects are computed and stored on the spec
  expect_s3_class(spec2$effects, "tbl_df")
  expect_true(nrow(spec2$effects) >= 1)

  # Sanity on the effect type column
  expect_true("type" %in% names(spec2$effects))
})

test_that("effects() errors when no attached model is present", {
  # Pass a bare tibble (not a spec, no 'model' attribute)
  bad_fitted <- tibble::tibble(x = 1:3)

  expect_error(
    effects(bad_fitted),
    "No attached model found; cannot compute effect sizes\\.",
    perl = TRUE
  )
})

test_that(".compute_effects errors if no matching estimate column exists (eta2)", {
  skip_if_not_installed("effectsize") # needed so we can mock its function

  # An ANOVA-like model so the eta2 route is taken
  mdl <- stats::aov(mpg ~ factor(cyl), data = mtcars)

  # Mock effectsize::eta_squared() to return a tibble with *no* candidate columns
  # (eta2 expects one of: "Eta2", "eta.sq", "eta_sq", etc.)
  testthat::with_mock(
    `effectsize::eta_squared` = function(...) {
      tibble::tibble(foo = 0.11, bar = 0.22) # wrong column names on purpose
    },
    {
      expect_error(
        tidycomp:::.compute_effects(
          model = mdl,
          type = "eta2",
          conf_level = 0.95
        ),
        # Message includes the type and lists *available* columns
        "Could not find an estimate column for type 'eta2'. Available columns: foo, bar"
      )
    }
  )
})

test_that("effects() conf_level only changes CI, not estimate", {
  skip_if_not_installed("effectsize")

  set.seed(123)
  df <- tibble::tibble(
    y = c(rnorm(40, 0), rnorm(40, 0.4), rnorm(40, 0.8)),
    g = factor(rep(letters[1:3], each = 40))
  )

  spec <- df |>
    comp_spec() |>
    set_roles(outcome = y, group = g) |>
    set_design("independent") |>
    set_engine("kruskal_wallis") |>
    set_outcome_type("numeric") |>
    diagnose() |>
    test()

  eff_90 <- effects(spec, conf_level = 0.90)
  eff_70 <- effects(spec, conf_level = 0.70)

  # Estimate must be identical
  expect_equal(
    eff_90$effects$estimate,
    eff_70$effects$estimate,
    tolerance = 1e-12
  )

  # At least one CI bound should differ
  expect_false(
    all.equal(
      eff_90$effects$conf.low,
      eff_70$effects$conf.low,
      tolerance = 1e-12
    ) ==
      TRUE &&
      all.equal(
        eff_90$effects$conf.high,
        eff_70$effects$conf.high,
        tolerance = 1e-12
      ) ==
        TRUE
  )
})

test_that("rank-biserial errors if effectsize is unavailable (Wilcoxon path)", {
  # Build a Mann–Whitney spec so the fitted model is an htest
  df <- tibble::tibble(
    y = c(1, 2, 3, 10, 11, 12),
    g = factor(rep(c("A", "B"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = y, group = g) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney") |>
    test()

  # Temporarily pretend effectsize is not installed
  orig_is_installed <- get("is_installed", envir = asNamespace("rlang"))
  assignInNamespace("is_installed", function(pkg) FALSE, ns = "rlang")
  on.exit(
    assignInNamespace("is_installed", orig_is_installed, ns = "rlang"),
    add = TRUE
  )

  # Call effects() on the SPEC (so parent_spec is not NULL) with rank_biserial
  expect_error(
    effects(spec, type = "rank_biserial"),
    "Package 'effectsize' is required for rank-biserial.",
    fixed = TRUE
  )
})

test_that("rank-biserial returns a tibble when effectsize is available (Wilcoxon path)", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    y = c(2, 3, 5, 7, 11, 13),
    g = factor(rep(c("A", "B"), each = 3))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = y, group = g) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney") |>
    test()

  es <- effects(spec, type = "rank_biserial")
  expect_s3_class(es$effects, "tbl_df")
  expect_equal(nrow(es$effects), 1L)
  expect_true(all(c("effect", "type", "estimate") %in% names(es$effects)))
  expect_identical(es$effects$type, "rank_biserial")
})

test_that(".compute_effects errors for Welch ANOVA without original data", {
  mdl <- stats::oneway.test(mpg ~ factor(cyl), data = mtcars, var.equal = FALSE)
  mdl$method <- "Welch's ANOVA"
  expect_error(
    .compute_effects(
      mdl,
      type = "omega2",
      conf_level = 0.95,
      parent_spec = NULL
    ),
    "Omega squared for Welch's ANOVA requires the original data; call effects() on a spec.",
    fixed = TRUE
  )
})

test_that("rank-biserial requires original data/roles when called on a fitted htest", {
  # Minimal two-group data for a Wilcoxon/Mann–Whitney test (htest model)
  df <- tibble::tibble(
    y = c(1, 2, 3, 10, 11, 12),
    g = factor(rep(c("A", "B"), each = 3))
  )

  # Build spec and fit Mann–Whitney (produces an htest model under the hood)
  spec <- comp_spec(df) |>
    set_roles(outcome = y, group = g) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("mann_whitney") |>
    test()

  fit <- spec$fitted # <- call effects() on the fitted result (no parent_spec)

  expect_error(
    effects(fit, type = "rank_biserial"),
    "Rank-biserial requires the original data and roles; call effects() on a spec.",
    fixed = TRUE
  )
})

test_that(".default_effect_type returns 'rank_biserial' for htest with W statistic", {
  mdl_w <- stats::wilcox.test(
    x = c(1, 3, 5, 7),
    y = c(2, 4, 6, 8),
    exact = FALSE
  )
  out <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = NULL,
    model = mdl_w
  )
  expect_identical(out, "rank_biserial")
})

test_that(".default_effect_type returns 'kendalls_w' for Friedman htest", {
  set.seed(456)
  df <- tibble::tibble(
    id = rep(1:12, each = 3),
    g = factor(rep(c("A", "B", "C"), times = 12)),
    y = c(
      rnorm(12, 0.0),
      rnorm(12, 0.3),
      rnorm(12, 0.9)
    )
  )

  mdl <- stats::friedman.test(y ~ g | id, data = df)
  out <- tidycomp:::.default_effect_type(
    spec_or_fit = list(),
    fitted = NULL,
    model = mdl
  )
  expect_identical(out, "kendalls_w")
})


test_that(".compute_effects computes model R2 for glm", {
  skip_if_not_installed("performance")

  mdl <- glm(am ~ mpg, data = mtcars, family = binomial())
  class(mdl) <- setdiff(class(mdl), "lm")

  testthat::with_mock(
    `performance::r2` = function(model) list(r2 = 0.5),
    {
      res <- tidycomp:::.compute_effects(
        model = mdl,
        type = "r2",
        conf_level = 0.95
      )
      expect_s3_class(res, "tbl_df")
      expect_equal(res$effect, "model")
      expect_equal(res$type, "r2")
      expect_equal(res$estimate, 0.5)
      expect_true(is.na(res$conf.low))
      expect_true(is.na(res$conf.high))
    }
  )
})

test_that(".compute_effects computes model R2 for lmerMod", {
  skip_if_not_installed("performance")
  skip_if_not_installed("lme4")

  mdl <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)

  testthat::with_mock(
    `performance::r2` = function(model) list(r2 = 0.42),
    {
      res <- tidycomp:::.compute_effects(
        model = mdl,
        type = "r2",
        conf_level = 0.95
      )
      expect_s3_class(res, "tbl_df")
      expect_equal(res$effect, "model")
      expect_equal(res$type, "r2")
      expect_equal(res$estimate, 0.42)
      expect_true(is.na(res$conf.low))
      expect_true(is.na(res$conf.high))
    }
  )
})

test_that(".compute_effects (r2) errors when performance is unavailable", {
  mdl <- glm(am ~ mpg, data = mtcars, family = binomial())
  class(mdl) <- setdiff(class(mdl), "lm")

  testthat::with_mock(
    `rlang::is_installed` = function(pkg) FALSE,
    {
      expect_error(
        tidycomp:::.compute_effects(
          model = mdl,
          type = "r2",
          conf_level = 0.95
        ),
        "Package 'performance' is required for R2 measures.",
        fixed = TRUE
      )
    }
  )
})

test_that("effects(..., type='omega2') uses parent data for Welch ANOVA (formula route)", {
  skip_if_not_installed("effectsize", minimum_version = "0.8.7")

  set.seed(1)
  df <- tibble::tibble(
    outcome = c(rnorm(30, 0, 1), rnorm(30, 0, 2), rnorm(30, 0, 3)),
    group = factor(rep(c("A", "B", "C"), each = 30))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_welch") |>
    diagnose() |>
    test()

  # effects() returns the spec back; pull the table from $effects
  res <- effects(spec, type = "omega2", conf_level = 0.90)
  es <- if (inherits(res, "comp_spec")) res$effects else res

  # Reference: omega^2 from the model fit using the same data
  expected <- effectsize::omega_squared(
    lm(outcome ~ group, data = df),
    ci = 0.90
  )

  expect_s3_class(es, "tbl_df")
  expect_equal(es$type, "omega2")
  expect_equal(es$estimate, expected$Omega2[1], tolerance = 1e-6)
})

test_that("effects(…, type = 'omega2') uses model route for classic ANOVA (non-Welch)", {
  skip_if_not_installed("effectsize", minimum_version = "0.8.7")

  set.seed(2)
  df <- tibble::tibble(
    outcome = c(rnorm(30, 0, 1), rnorm(30, 0, 1.1), rnorm(30, 0, 0.9)),
    group = factor(rep(c("A", "B", "C"), each = 30))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    diagnose() |>
    test()

  res <- effects(spec, type = "omega2", conf_level = 0.90)
  es <- if (inherits(res, "comp_spec")) res$effects else res

  expected <- effectsize::omega_squared(
    aov(outcome ~ group, data = df),
    ci = 0.90
  )

  expect_s3_class(es, "tbl_df")
  expect_equal(es$type, "omega2")
  expect_equal(es$estimate, expected$Omega2[1], tolerance = 1e-6)
})

# Categorical effect sizes ----------------------------------------------------

test_that("effects() computes phi for fisher_exact", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    outcome = factor(c("yes", "no", "yes", "no", "yes", "no")),
    group = factor(c("A", "A", "B", "B", "A", "B"))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("binary") |>
    set_engine("fisher_exact") |>
    test() |>
    effects()

  expected <- effectsize::phi(table(df$group, df$outcome))

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "phi")
  expect_equal(spec$effects$estimate, expected$Phi[1], tolerance = 1e-6)
})

test_that("effects() computes cramers_v for chisq_nxn", {
  skip_if_not_installed("effectsize")

  df <- tidyr::expand_grid(
    outcome = factor(c("a", "b", "c")),
    group = factor(c("G1", "G2", "G3"))
  ) |>
    tidyr::uncount(5)

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("binary") |>
    set_engine("chisq_nxn") |>
    test() |>
    effects()

  expected <- effectsize::cramers_v(table(df$group, df$outcome))

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "cramers_v")
  expect_equal(spec$effects$estimate, expected$Cramers_v[1], tolerance = 1e-6)
})

test_that("effects() computes oddsratio for mcnemar", {
  skip_if_not_installed("effectsize")

  df <- tibble::tibble(
    id = rep(1:30, each = 2),
    group = factor(rep(c("A", "B"), times = 30)),
    outcome = factor(c(rep(c("yes", "no"), 15), rep(c("no", "yes"), 15)))
  )

  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group, id = id) |>
    set_design("paired") |>
    set_outcome_type("binary") |>
    set_engine("mcnemar_chi2") |>
    test() |>
    effects()

  wide <- tidycomp:::.standardize_paired_categorical(df, "outcome", "group", "id")
  expected <- effectsize::oddsratio(table(wide[[1]], wide[[2]]))

  expect_s3_class(spec$effects, "tbl_df")
  expect_equal(spec$effects$type, "oddsratio")
  expect_equal(spec$effects$estimate, expected$Odds_ratio[1], tolerance = 1e-6)
})
