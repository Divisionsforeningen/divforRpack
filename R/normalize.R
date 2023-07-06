#' normalize
#'
#' Normalizes a series of values to be between 0 and 1
#'
#' @param x Series of values or a data frame column
#'
#' @return Series of values scaled between 0 and 1
#' @export
#'
#' @examples
#' normalize(c(0, 5, 10))
#'
#' normalize(c(0, 5, 10)) * 100
normalize <- function(x) {
  # Takes a series of values and returns the 0-1 scaled version
  (x - min(x)) / (max(x) - min(x))
}
