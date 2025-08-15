test_that(".extract_sphericity_p handles tibble input", {
  sp <- tibble::tibble(Effect = c("group", "other"), p = c(0.02, 0.5))
  res <- tidycomp:::.extract_sphericity_p(sp)
  expect_type(res, "double")
  expect_length(res, 1)
  expect_equal(res, 0.02)
})

test_that(".extract_sphericity_p handles named numeric vector", {
  sp <- c(group = 0.03, other = 0.5)
  res <- tidycomp:::.extract_sphericity_p(sp)
  expect_type(res, "double")
  expect_length(res, 1)
  expect_equal(res, 0.03)
})

test_that(".extract_sphericity_p handles unnamed numeric vector", {
  sp <- c(0.04, 0.5)
  res <- tidycomp:::.extract_sphericity_p(sp)
  expect_type(res, "double")
  expect_length(res, 1)
  expect_identical(res, NA_real_)
})

test_that(".extract_sphericity_p handles NULL input", {
  res <- tidycomp:::.extract_sphericity_p(NULL)
  expect_type(res, "double")
  expect_length(res, 1)
  expect_identical(res, NA_real_)
})
