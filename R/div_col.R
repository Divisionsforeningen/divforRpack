#' div_col
#'
#' Given either type or custom input checks if the input is a color and returns it for plotting use
#'
#' @param type String indicating type for default colors
#' @param color Custom color input - either a color from colors() or a color in hex code
#'
#' @return Color
#' @import grDevices
#' @export
#'
#' @examples
#' div_col(color = "white")
#' div_col(type = "reference")
div_col <- function(type = NA, color = NA) {
  # TODO Write test battery
  # Checks for input - if not found throws error
  if (is.na(type) & is.na(color)) {
    stop("No type or color chosen")
  }
  # Checks for custom color and returns it if it is a color or hex code
  else if (!is.na(color)) {
    if (color %in% grDevices::colors()) {
      return(color)
    } else if (is_hexcolor(color)) {
      return(color)
    } else {
      stop("Color is not usable")
    }
  }
  # If custom color is not chosen then check type and return color
  else {
    col <- dplyr::case_when(
      # General colors
      type == "w_text" ~ "white",
      type == "b_text" ~ "black",
      type == "reference" ~ "red",
      type == "axis" ~ "grey",
      # Chart with comparisons
      type == "fill" ~ "lightgrey",
      type == "highlight" ~ "#007bff",
      # Charts based on 4 different levels - best to worst
      type == "top" ~ "forestgreen",
      type == "above" ~ "gold",
      type == "below" ~ "orange",
      type == "bottom" ~ "red",
      # Standard radar color
      type == "radar" ~ "#00AFBB",
      # If type is not in defined set
      TRUE ~ NA
    )
    # If no usable output is generated throw error
    if (is.na(col)) stop("No color chosen")
    # Returns color
    return(col)
  }
}
