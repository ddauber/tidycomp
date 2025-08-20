library(testthat)
library(tidycomp)

df <- tibble::tibble(
  outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  group = factor(rep(c("A", "B", "C"), each = 4))
)

test_that("post_hoc defaults to TukeyHSD", {
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    test() |>
    post_hoc()

  expect_s3_class(spec$post_hoc, "tbl_df")
  expect_equal(unique(spec$post_hoc$method), "TukeyHSD")
  expect_equal(unique(spec$post_hoc$p.adj.method), "tukey")
})

test_that("set_post_hoc overrides method and adjustment", {
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    set_post_hoc(method = "pairwise_t_test", adjust = "bonferroni") |>
    test() |>
    post_hoc()

  expect_equal(unique(spec$post_hoc$method), "t.test")
  expect_equal(unique(spec$post_hoc$p.adj.method), "bonferroni")
})

test_that("pairwise_t_test uses stats::pairwise.t.test", {
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_post_hoc(method = "pairwise_t_test", adjust = "bonferroni") |>
    test() |>
    post_hoc()

  pw_raw <- stats::pairwise.t.test(
    df$outcome,
    df$group,
    pool.sd = TRUE,
    p.adjust.method = "none"
  )$p.value
  pw_adj <- stats::pairwise.t.test(
    df$outcome,
    df$group,
    pool.sd = TRUE,
    p.adjust.method = "bonferroni"
  )$p.value
  combs <- utils::combn(levels(df$group), 2, simplify = FALSE)
  expected <- purrr::map_dfr(combs, function(pair) {
    tibble::tibble(
      group1 = pair[1],
      group2 = pair[2],
      p.value = pw_raw[pair[2], pair[1]],
      p.adj = pw_adj[pair[2], pair[1]]
    )
  })
  spec_res <- dplyr::arrange(spec$post_hoc, group1, group2)
  expected <- dplyr::arrange(expected, group1, group2)
  expect_equal(spec_res$p.value, expected$p.value)
  expect_equal(spec_res$p.adj, expected$p.adj)
})

test_that("post_hoc skips when omnibus not significant", {
  df2 <- tibble::tibble(
    outcome = rep(1:4, 3),
    group = factor(rep(c("A", "B", "C"), each = 4))
  )
  spec <- comp_spec(df2) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("anova_oneway_equal") |>
    test() |>
    post_hoc()

  expect_equal(nrow(spec$post_hoc), 0)
  expect_true(isTRUE(attr(spec$post_hoc, "skipped")))
})

test_that("kruskal-wallis uses appropriate default", {
  spec <- comp_spec(df) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    set_engine("kruskal_wallis") |>
    test() |>
    post_hoc()

  if (requireNamespace("rstatix", quietly = TRUE)) {
    expect_equal(unique(spec$post_hoc$method), "dunn_test")
  } else {
    expect_equal(unique(spec$post_hoc$method), "wilcox.test")
  }
})

df_bin <- tibble::tibble(
  outcome = factor(c(1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0), levels = c(0, 1)),
  group = factor(rep(c("A", "B", "C"), each = 4))
)

test_that("binary outcomes use pairwise_prop_test", {
  spec <- comp_spec(df_bin) |>
    set_roles(outcome = outcome, group = group) |>
    set_design("independent") |>
    set_outcome_type("binary") |>
    set_engine("chisq_nxn") |>
    test() |>
    post_hoc(force = TRUE)

  expect_equal(unique(spec$post_hoc$method), "prop.test")
  expect_equal(unique(spec$post_hoc$p.adj.method), "bonferroni")
})

test_that(".default_post_hoc_method honors spec$post_hoc_hint", {
  spec <- structure(
    list(post_hoc_hint = "pairwise_t_test"),
    class = "comp_spec"
  )
  out <- tidycomp:::.default_post_hoc_method(
    spec_or_fit = spec,
    fitted = NULL,
    design = NULL,
    outcome_type = "numeric"
  )
  expect_identical(out, "pairwise_t_test")
})

test_that(".default_post_hoc_method honors fitted attr 'post_hoc_hint'", {
  fitted <- list()
  attr(fitted, "post_hoc_hint") <- "tukey"
  out <- tidycomp:::.default_post_hoc_method(
    spec_or_fit = list(),
    fitted = fitted,
    design = NULL,
    outcome_type = "numeric"
  )
  expect_identical(out, "tukey")
})

test_that(".ph_games_howell uses rstatix when available", {
  df_gh <- tibble::tibble(
    outcome = c(1, 2, 3, 4),
    group = factor(rep(c("A", "B"), each = 2))
  )

  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, quietly = TRUE) TRUE,
    .package = "base"
  )

  testthat::local_mocked_bindings(
    games_howell_test = function(data, formula, conf.level) {
      tibble::tibble(
        group1 = "A",
        group2 = "B",
        estimate = 1,
        conf.low = 0.5,
        conf.high = 1.5,
        p = 0.05,
        p.adj = 0.05
      )
    },
    .package = "rstatix"
  )

  res <- tidycomp:::.ph_games_howell(
    df_gh,
    "outcome",
    "group",
    conf_level = 0.95
  )

  expect_equal(res$method, "games_howell_test")
  expect_equal(res$p.adj.method, "games_howell")
  expect_equal(res$group1, "A")
  expect_equal(res$group2, "B")
})

test_that(".ph_games_howell matches rstatix::games_howell_test and respects conf_level", {
  skip_if_not_installed("rstatix")
  set.seed(123)
  # Unequal variances / unequal n to make GH appropriate
  df <- dplyr::bind_rows(
    data.frame(y = rnorm(35, mean = 0.0, sd = 1.0), g = "A"),
    data.frame(y = rnorm(45, mean = 0.7, sd = 1.8), g = "B"),
    data.frame(y = rnorm(50, mean = 1.2, sd = 2.2), g = "C")
  )
  df$g <- droplevels(factor(df$g, levels = c("A", "B", "C")))

  # Reference via rstatix (conf.level = .95)
  fml <- stats::as.formula("y ~ g")
  ref95 <- rstatix::games_howell_test(df, fml, conf.level = 0.95)

  # Our wrapper (conf.level = .95)
  out95 <- tidycomp:::`.ph_games_howell`(df, "y", "g", conf_level = 0.95)

  # Sort for stable comparison
  ord <- order(out95$group1, out95$group2)
  out95 <- out95[ord, ]
  ref95 <- ref95[order(ref95$group1, ref95$group2), ]

  # Column-by-column checks
  expect_equal(out95$group1, ref95$group1)
  expect_equal(out95$group2, ref95$group2)
  # rstatix may use `estimate` or `mean.difference`; wrapper handles both
  ref_est <- ref95$estimate %||% ref95$mean.difference
  expect_equal(out95$estimate, ref_est, tolerance = 1e-12)
  expect_equal(out95$conf.low, ref95$conf.low, tolerance = 1e-8)
  expect_equal(out95$conf.high, ref95$conf.high, tolerance = 1e-8)
  expect_equal(out95$p.value, ref95$p, tolerance = 1e-12)
  expect_equal(out95$p.adj, ref95$p.adj, tolerance = 1e-12)
  expect_identical(out95$p.adj.method, rep("games_howell", nrow(out95)))
  expect_identical(out95$method, rep("games_howell_test", nrow(out95)))

  # Now check conf_level plumbing changes CI width (0.90 narrower than 0.99)
  out90 <- tidycomp:::`.ph_games_howell`(df, "y", "g", conf_level = 0.90)
  out99 <- tidycomp:::`.ph_games_howell`(df, "y", "g", conf_level = 0.99)
  width90 <- out90$conf.high - out90$conf.low
  width99 <- out99$conf.high - out99$conf.low
  expect_true(all(width90 < width99))
})

test_that(".ph_pairwise_wilcox returns expected results for independent design", {
  dat <- tibble::tibble(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  res <- tidycomp:::.ph_pairwise_wilcox(
    data = dat,
    outcome = "outcome",
    group = "group",
    design = "independent",
    p.adjust = "bonferroni",
    conf_level = 0.95
  )

  combs <- utils::combn(levels(dat$group), 2, simplify = FALSE)
  expected <- purrr::map_dfr(combs, function(pair) {
    x1 <- dat$outcome[dat$group == pair[1]]
    x2 <- dat$outcome[dat$group == pair[2]]
    wt <- stats::wilcox.test(
      x1,
      x2,
      paired = FALSE,
      conf.int = TRUE,
      conf.level = 0.95,
      exact = FALSE
    )
    est <- if (is.null(wt$estimate)) NA_real_ else unname(wt$estimate)
    tibble::tibble(
      group1 = pair[1],
      group2 = pair[2],
      estimate = est,
      conf.low = wt$conf.int[1],
      conf.high = wt$conf.int[2],
      p.value = wt$p.value
    )
  })
  expected$p.adj <- stats::p.adjust(expected$p.value, method = "bonferroni")
  expected$p.adj.method <- "bonferroni"
  expected$method <- "wilcox.test"

  res <- dplyr::arrange(res, group1, group2)
  expected <- dplyr::arrange(expected, group1, group2)
  expect_equal(res, expected)
})

# paired design

test_that(".ph_pairwise_wilcox handles paired design", {
  dat <- tibble::tibble(
    outcome = c(1, 2, 3, 2, 3, 4, 3, 4, 5),
    group = factor(rep(c("A", "B", "C"), each = 3))
  )

  res <- tidycomp:::.ph_pairwise_wilcox(
    data = dat,
    outcome = "outcome",
    group = "group",
    design = "paired",
    p.adjust = "holm",
    conf_level = 0.95
  )

  combs <- utils::combn(levels(dat$group), 2, simplify = FALSE)
  expected <- purrr::map_dfr(combs, function(pair) {
    x1 <- dat$outcome[dat$group == pair[1]]
    x2 <- dat$outcome[dat$group == pair[2]]
    wt <- stats::wilcox.test(
      x1,
      x2,
      paired = TRUE,
      conf.int = TRUE,
      conf.level = 0.95,
      exact = FALSE
    )
    est <- if (is.null(wt$estimate)) NA_real_ else unname(wt$estimate)
    tibble::tibble(
      group1 = pair[1],
      group2 = pair[2],
      estimate = est,
      conf.low = wt$conf.int[1],
      conf.high = wt$conf.int[2],
      p.value = wt$p.value
    )
  })
  expected$p.adj <- stats::p.adjust(expected$p.value, method = "holm")
  expected$p.adj.method <- "holm"
  expected$method <- "wilcox.test"

  res <- dplyr::arrange(res, group1, group2)
  expected <- dplyr::arrange(expected, group1, group2)
  expect_equal(res, expected)
})
