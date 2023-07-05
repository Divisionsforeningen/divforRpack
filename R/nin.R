#' %nin%
#'
#' @param a Object to search for
#' @param b Object to search in
#'
#' @return TRUE if a is in b, else FALSE
#' @export
#'
#' @examples a <- 1
#' b <- c(1, 3, 5)
#' a %nin% b
"%nin%" <- function(a, b) {
  # Define function as the reverse of %in% function
  !a %in% b
}
