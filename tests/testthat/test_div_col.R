test_div_col <- function() {
  # Test case 1: Custom color input
  color_input <- "#FF5733"
  expect_equal(div_col(color = color_input), color_input)

  # Test case 2: Custom color input with incorrect format
  invalid_color_input <- "invalid_color"
    expect_error(div_col(color = invalid_color_input), "Color is not usable")

  # Test case 3: Predefined color type "reference"
  reference_color <- "red"
  expect_equal(div_col(type = "reference"), reference_color)

  # Test case 4: Predefined color type "highlight"
  highlight_color <- "#007bff"
  expect_equal(div_col(type = "highlight"), highlight_color)

  # Test case 5: Predefined color type "b_text"
  b_text_color <- "black"
  expect_equal(div_col(type = "b_text"), b_text_color)

  # Test case 6: Predefined color type "fill"
  fill_color <- "lightgrey"
  expect_equal(div_col(type = "fill"), fill_color)

  # Test case 7: Predefined color type "top"
  top_color <- "forestgreen"
  expect_equal(div_col(type = "top"), top_color)

  # Test case 8: Predefined color type "chosen"
  chosen_color <- "yellow"
  expect_equal(div_col(type = "chosen"), chosen_color)

  # Test case 9: Predefined color type "goal"
  goal_color <- "red"
  expect_equal(div_col(type = "goal"), goal_color)
}

test_that("div_col returns correct colors", {
  test_div_col()
})
