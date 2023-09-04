#' Line Chart on Continuous X Axis
#'
#' Creates a line chart on a continuous x axis.
#'
#' @param df Data frame.
#' @param x Column with data for the x axis.
#' @param y Column with data for the y axis.
#' @param labels Column with point labels.
#' @param ref Reference value on the y axis.
#' @param trend Set to "TRUE" to add smoothed trend.
#' @param lCol Line color.
#' @param KPI Y axis label.
#' @param Periods X axis label.
#' @param Title Title of the plot.
#' @param Subtitle Subtitle of the plot.
#' @param provider Data provider.
#' @param refCol Color of the reference line.
#'
#' @return A line chart on a continuous x axis.
#' @export
#' @import ggplot2
#' @import dplyr
#' @import ggrepel
#'
line_chart_con <- function(df, x, y, labels, ref = NA, trend = NA, lCol = NA, KPI = "Put KPI here", Periods = "Put x label here", Title = NA, Subtitle = NA, provider = "Put provider here", refCol = NA) {
  # TODO Write test battery!

  # Check if 'trend' is not TRUE or is NA, then set it to FALSE.
  if (trend != TRUE || is.na(trend)) {
    trend <- FALSE
  }

  # Calculate the range of the data for the 'x' variable.
  range <- df %>%
    dplyr::ungroup() %>%
    dplyr::summarise(max = max(.data[[x]]), min = min(.data[[x]]))

  df <- df %>% arrange(.data[[x]])

  # Create a ggplot object 'p'.
  p <- ggplot(
    data = df,
    aes(x = as.numeric(.data[[x]]), y = .data[[y]], group = 1)
  ) +
    # Add line
    geom_path(aes(),
      alpha = 1, col = ifelse(is.na(lCol), divforRpack::div_col(type = "highlight"), divforRpack::div_col(color = lCol)),
      linewidth = 2
    ) +
    # Add points
    geom_point(aes(), cex = 5, fill = divforRpack::div_col(type = "fill"), pch = 21, col = "black") +
    # Add labels using ggrepel package
    ggrepel::geom_text_repel(data = df, aes(label = .data[[labels]])) +
    # Scale x axis
    scale_x_continuous(breaks = seq(from = as.numeric(range[2]), to = as.numeric(range[1]), by = 1)) +
    # Add labels, title, subtitle, and caption with data provider
    labs(
      y = KPI,
      x = Periods,
      title = Title,
      subtitle = Subtitle,
      caption = paste0("Data by ", provider)
    ) +
    # Control theme settings
    theme(
      panel.grid.minor = element_blank(),
      panel.grid = element_line(color = divforRpack::div_col(type = "fill")),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 15),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 12),
      plot.background = element_rect(fill = "NA"),
      panel.background = element_rect(fill = "NA")
    )

  # Check if 'ref' is NA.
  if (is.na(ref)) {
    # If 'trend' is FALSE, return the 'p' plot as it is.
    if (trend == F) {
      p
    } else {
      # If 'trend' is TRUE, add a smoothed line for trend and return the updated 'p' plot.
      p <- p +
        geom_smooth(aes(), se = FALSE)
      p
    }
  } else {
    # If 'ref' is not NA.
    # If 'trend' is FALSE, add a reference line and return the updated 'p' plot.
    if (trend == F) {
      p <- p +
        geom_hline(aes(yintercept = ref), color = div_col("reference", ifelse(is.na(refCol), NA, refCol)), linewidth = 2)
      p
    } else {
      # If 'trend' is TRUE, add a reference line and a smoothed line for trend, then return the updated 'p' plot.
      p <- p +
        geom_hline(aes(yintercept = ref), color = div_col("reference", ifelse(is.na(refCol), NA, refCol)), linewidth = 2) +
        geom_smooth(aes(), se = FALSE)
      p
    }
  }
}
