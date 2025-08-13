# Compare engines produces one row per engine ------------------------------
test_that("compare_engines returns a row per engine with labels", {
  spec <- comp_spec(mtcars) |>
    set_roles(mpg, am) |>
    set_design("independent") |>
    set_outcome_type("numeric") |>
    diagnose()

  engines <- c("welch_t", "student_t")

  results <- suppressMessages(compare_engines(spec, engines))

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), length(engines))
  expect_equal(results$engine_forced, engines)
})

# compare_engines requires a comp_spec -------------------------------------
test_that("compare_engines validates its input", {
  expect_error(compare_engines("not_a_spec"))
})

# decision_tree is informational -------------------------------------------
test_that("decision_tree returns invisibly", {
  expect_invisible(decision_tree())
})
