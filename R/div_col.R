#' div_col
#'
#' @param type String indicating type for default colors
#' @param color Custom color input - either a color from colors() or a color in hex code
#'
#' @return Color
#' @export
#'
#' @examples div_col(color = "white")
div_col <- function(type = NA, color = NA) {
  # Checks for custom color and returns it if it is a color or hex code
  if (!is.na(color) & (color %in% colors() | is_hexcolor(text = color)
  )) {
    return(color)
  }
  # Checks for input - if not found throws error
  if (is.na(type) & !(!is.na(color) & (color %in% colors() | is_hexcolor(text = color)
  ))) {
    stop("No type or color chosen")
  }
  col <- dplyr::case_when(
    type == "bar_chart_mean" ~ "red",
    TRUE ~ NA
  )
  # If no usable output is generated throw error
  if (is.na(col)) stop("No color chosen")
  # Returns color
  return(col)
}
