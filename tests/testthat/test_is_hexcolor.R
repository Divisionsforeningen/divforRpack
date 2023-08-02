# Define a test function for is_hexcolor
test_is_hexcolor <- function() {
  test_that("is_hexcolor correctly identifies valid hex color codes", {
    # Valid hex color codes
    expect_true(is_hexcolor("#ffffff"))
    expect_true(is_hexcolor("#000000"))
    expect_true(is_hexcolor("#FF00cc"))
    expect_true(is_hexcolor("#a1b2c3"))
    expect_true(is_hexcolor("#123"))

    # Invalid hex color codes
    expect_false(is_hexcolor("ffffff"))
    expect_false(is_hexcolor("#12345"))
    expect_false(is_hexcolor("#ABCGHI"))
    expect_false(is_hexcolor("#12"))
    expect_false(is_hexcolor("#abcde"))

    # Non-hex color codes (not starting with #)
    expect_false(is_hexcolor("red"))
    expect_false(is_hexcolor("rgb(0,0,0)"))
    expect_false(is_hexcolor("hsl(100,50%,50%)"))

    # NA value should return FALSE
    expect_false(is_hexcolor(NA))

    # Empty string should return FALSE
    expect_false(is_hexcolor(""))

    # NULL should return FALSE
    expect_false(is_hexcolor(NULL))
  })
}

# Call the test function
test_is_hexcolor()
