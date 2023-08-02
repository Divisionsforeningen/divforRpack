#' is_hexcolor
#'
#' Check if the input text is a valid hex color code.
#'
#' @param text String to test for hex color code.
#'
#' @return TRUE if valid hex color code, FALSE otherwise.
#' @importFrom stringr str_detect
#' @export
#'
#' @examples is_hexcolor("#ffffff")
is_hexcolor <- function(text = NA) {
  if(is.na(text) || text=="" || is.null(text)){
    return(FALSE)
  }

  # Define the regex pattern to check for valid hex color codes
  pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}|[A-Fa-f0-9]{8})$"

  # Check if the input text matches the hex color code pattern using the str_detect function
  is_valid_hex <- stringr::str_detect(text, pattern)

  # Return TRUE if the input is a valid hex color code, FALSE otherwise
  return(is_valid_hex)
}
