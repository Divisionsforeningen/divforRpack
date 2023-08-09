test_comp_order <- function() {
  # Test case 1: Correct function
  input_1 <- c(355, 1305, 1570, 43149)
  output_1 <- comp_order(input_1)
  expect_equal(output_1, c(1, 11, 21, 31))

  # Test case 2: Some not in danish league system
  input_2 <- c(1, 514, 355, 1570)
  output_2 <- comp_order(input_2)
  expect_equal(output_2, c(999, 999, 1, 21))

  # Test case 3: Input not numeric
  input_3 <- c("Text", "More", "Text")
  expect_error(comp_order(input_3), "Inputs are not numeric")
}


test_that("Comp order working as intended", {
  test_comp_order()
})
