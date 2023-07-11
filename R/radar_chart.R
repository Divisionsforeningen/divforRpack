#' radar chart
#'
#' @param data Data
#' @param color Radar color
#' @param axisCol Axis color
#' @param vlabels Axis labels
#' @param vlcex Axis size
#' @param caxislabels Caxis labels
#' @param title Title
#' @param ...
#'
#' @return A costum version of the fmsb radar chart
#' @export
#' @import fmsb
#' @import scales
#'
create_beautiful_radarchart <- function(data, color = NA, axisCol = NA,
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...) {
  # TODO Make vlables costumizable
  # TODO Write test battery

  fmsb::radarchart(
    # Input data
    data,
    # Select axis type
    axistype = 4,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(ifelse(is.na(color), div_col(type = "radar"), div_col(color = color)), 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = ifelse(is.na(color), div_col(type = "axis"), div_col(color = axisCol)), cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = ifelse(is.na(color), div_col(type = "axis"), div_col(color = axisCol)),
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}
