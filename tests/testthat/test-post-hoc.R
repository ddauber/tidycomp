library(testthat)
library(tidycomp)

df <- tibble::tibble(
  outcome = c(1,2,3,4, 5,6,7,8, 9,10,11,12),
  group = factor(rep(c("A","B","C"), each = 4))
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
    group = factor(rep(c("A","B","C"), each = 4))
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
  outcome = factor(c(1,1,0,0,1,0,1,0,1,0,1,0), levels = c(0,1)),
  group = factor(rep(c("A","B","C"), each = 4))
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
  spec <- structure(list(post_hoc_hint = "pairwise_t_test"), class = "comp_spec")
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
