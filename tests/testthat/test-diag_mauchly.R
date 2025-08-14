test_that("diag_mauchly returns W and p", {
  set.seed(123)
  df <- tibble::tibble(
    id = rep(1:6, each = 3),
    group = factor(rep(c("A", "B", "C"), times = 6)),
    outcome = rnorm(18)
  )
  res <- diag_mauchly(df, outcome, group, id)
  expect_s3_class(res, "tbl_df")
  expect_equal(names(res), c("Effect", "W", "p"))
  expect_true(is.numeric(res$W))
  expect_true(is.numeric(res$p))
})
