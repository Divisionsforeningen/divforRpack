#' is_hexcolor
#'
#' @param x String to test
#'
#' @return True is hexcode, false if not
#' @export
#'
#' @examples is_hexcolor("#ffffff")

is_hexcolor <- function(x) {
  pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}|[A-Fa-f0-9]{8})$"

  return(stringr::str_detect(x, pattern))
}
