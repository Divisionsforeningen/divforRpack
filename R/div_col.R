# Based on 2 inputs - type and custom color - return standard or custom color

div_col <- function(type = NA, color = NA) {
  if (!is.na(color) & (color %in% colors() | is_hexcolor(color))) {
    return(color)
  }
  if (is.na(type) & !(!is.na(color) & (color %in% colors() | is_hexcolor(color)))) stop("No type or color chosen")
  col <- dplyr::case_when(
    type == "bar_chart_mean" ~ "red",
    TRUE ~ NA
  )
  if (is.na(col)) stop("No color chosen")
  return(col)
}

div_col(color = "test")
