#' Default Transformation
#'
#' Subtracts x from y.
#'
#' @param x Value or list of values.
#' @param y Value to use for transformation - either length 1 or same length as x.
#'
#' @return x subtracted from y.
#' @export
#'
#' @examples
#'
#' def_transform(10, 5)

def_transform <- function(x, y) {
  # Throw error messsage if imputs are not numeric
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Inputs are not numeric")
  }

  # Simply return the computed number
  return((y - x))
}
