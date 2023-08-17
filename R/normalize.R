#' normalize
#'
#' Normalizes a series of values to be between 0 and 1
#'
#' @param x Series of values or a data frame column
#' @param type Choose between Z-score ("Z") or 0 to 1 normalization ("1")
#'
#' @return Series of values scaled between 0 and 1
#' @export
#'
#' @examples
#' normalize(c(0, 5, 10), type = "Z")
#'
#' normalize(c(0, 5, 10), type = "1") * 100
normalize <- function(x, type = NA) {
  if (!is.numeric(x)) {
    stop("Inputs are not numeric")
  }

  if(type %nin% c("Z","1")){
    stop("Selected type is not usable")
  }

  # TODO Write test battery
  if (type == "1") {
    # Takes a series of values and returns the 0-1 scaled version
    return((x - min(x)) / (max(x) - min(x)))
  }
  else if (type == "Z") {
    # Takes a series of values and returns the z-score
    return((x - mean(x)) / sd(x))
  }
  else(
    "Something is not right!"
  )
}
