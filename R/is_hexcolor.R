#' is_hexcolor
#'
#' @param text String to test
#'
#' @return True is hexcode, false if not
#' @export
#'
#' @examples is_hexcolor("#ffffff")
is_hexcolor <- function(text = NA) {
  # If no input throw error
  #if (is.na(text)) stop("No input")
  # Checks input using regex
  pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}|[A-Fa-f0-9]{8})$"

  # Returns true if hexcolor, false if not
  return(stringr::str_detect(text,pattern))
}
