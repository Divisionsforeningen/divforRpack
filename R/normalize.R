#' normalize
#'
#' Normalizes a series of values to be between 0 and 1
#'
#' @param x Series of values or a data frame column
#' @param type Choose between Z-score or 0 to 1 normalization
#'
#' @return Series of values scaled between 0 and 1
#' @export
#'
#' @examples
#' normalize(c(0, 5, 10), type = "Z")
#'
#' normalize(c(0, 5, 10), type = "O") * 100
normalize <- function(x, type = c("Z", "O")) {
  #TODO Write test battery
  if (type == "O") {
    # Takes a series of values and returns the 0-1 scaled version
    (x - min(x)) / (max(x) - min(x))
  }
  if (type == "Z") {
    # Takes a series of values and returns the z-score
    (x - mean(x)) / sd(x)
  }
}