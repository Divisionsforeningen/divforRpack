#' Not In Operator (%nin%)
#'
#' Checks if an object is not in another object.
#'
#' @param a Object to search for.
#' @param b Object to search in.
#'
#' @return TRUE if 'a' is not in 'b', else FALSE.
#' @export
#'
#' @examples
#' a <- 1
#' b <- c(1, 3, 5)
#' a %nin% b
"%nin%" <- function(a, b) {
  # Define function as the reverse of %in% function
  !a %in% b
}
