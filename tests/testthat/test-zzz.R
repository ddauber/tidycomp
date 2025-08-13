test_that("package startup message is informative", {
  # Calls .onAttach and asserts the emitted message
  expect_message(
    tidycomp:::.onAttach(NULL, "tidycomp"),
    regexp = "Welcome to tidycomp! Type \\?tidycomp to get started\\."
  )
})
