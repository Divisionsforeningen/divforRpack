test_def_transform <- function() {
  # Test case 1: Correct function
  input_1x <- c(5, 100, 2)
  input_1y <- 10
  output_1 <- def_transform(x = input_1x, y = input_1y)
  expect_equal(output_1, c(10 - 5, 10 - 100, 10 - 2))

  # Test case 2: Input not numeric
  input_2x <- c(1, 514, 355, 1570)
  input_2y <- "Text"
  expect_error(def_transform(x = input_2x, y = input_2y), "Inputs are not numeric")
}


test_that("Defensive transformation working as intended", {
  test_def_transform()
})
