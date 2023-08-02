library(ggplot2)

# Helper function to generate test data
generate_test_data <- function() {
  data.frame(Probability = c(0.3, 0.2, 0.5))
}

test_probability_chart <- function() {
  # Test if the function runs without errors
  expect_no_error(probability_chart(generate_test_data()))

  # Test if the output is a ggplot object
  p <- probability_chart(generate_test_data())
  expect_true(inherits(p, "gg"))

  # Test for correct x-axis labels
  expect_equal(ggplot_build(p)[["layout"]][["panel_params"]][[1]][["x"]][["breaks"]][1], "Home")
  expect_equal(ggplot_build(p)[["layout"]][["panel_params"]][[1]][["x"]][["breaks"]][2], "Draw")
  expect_equal(ggplot_build(p)[["layout"]][["panel_params"]][[1]][["x"]][["breaks"]][3], "Away")

  # Test for correct y-axis labels
  expect_equal(ggplot_build(p)$data[[1]]$y, c(0.3, 0.2, 0.5))

  # Add more specific tests here if needed
  # ...
}

# Run the test battery
test_that("test probability chart", {
  test_probability_chart()
})
