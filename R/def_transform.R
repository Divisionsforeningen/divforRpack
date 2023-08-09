#' def_transform
#'
#' Subtracts x from y
#'
#' @param x data
#'
#' @return x subtracted from y
#' @export
#'
#' @examples
#'
#' def_transform(10)
def_transform <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Inputs are not numeric")
  }

  return((y - x))
}
