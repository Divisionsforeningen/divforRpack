test_normalize <- function() {
    # Test case 1: 0-1 normalization
    input_01 <- c(0, 5, 10)
    output_01 <- normalize(input_01, type = "O")
    expect_equal(output_01, c(0, 0.5, 1))

    # Test case 2: z-score normalization
    input_z <- c(0, 5, 10)
    output_z <- normalize(input_z, type = "Z")
    expect_equal(output_z, c(-1, 0, 1))
  }


test_that("normalize returns correct values", {
          test_normalize()
  })
