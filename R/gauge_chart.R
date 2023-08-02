#' gauge
#'
#' Creates gauge chart for comparison across teams
#'
#' @param value Percentile value * 100
#' @param KPI Plot title
#' @param teams Number of teams in league
#' @param textCol Text color
#' @param bestCol Best quantile color
#' @param secCol Second quantile color
#' @param thrCol Third quantile color
#' @param worstCol Fourth quantile color
#'
#' @return Gauge plot with text based on 12 team league
#' @export
#' @import ggplot2
#'
#' @examples
#' gauge(50, "Test")
#' gauge(50, "Test", worstCol = "white", bestCol = "steelblue", secCol = "yellow", thrCol = "lightblue")
gauge <- function(value, KPI, teams = 12, textCol = NA, bestCol = NA, secCol = NA, thrCol = NA, worstCol = NA) {
  # TODO Write test battery
  # TODO Adjust annotations if teams>20

  # Define quantile breaks for the gauge plot based on the number of teams
  breaks <- c(0, 100 - ((teams / 4) / (teams / 4 + 1) * 100), 50, (teams / 4) / (teams / 4 + 1) * 100, 100)

  # Define the gauge function with quantile breaks
  gg.gauge <- function(pos, breaks = breaks) {
    # Function to generate semi-circle coordinates for each quantile segment
    get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
      th.start <- pi * (1 - a / 100)
      th.end <- pi * (1 - b / 100)
      th <- seq(th.start, th.end, length = 100)
      x <- c(r1 * cos(th), rev(r2 * cos(th)))
      y <- c(r1 * sin(th), rev(r2 * sin(th)))
      return(data.frame(x, y))
    }

    # Start creating the gauge plot
    ggplot() +
      # Add lower quantile segment
      geom_polygon(data = get.poly(breaks[1], breaks[2]), aes(x, y), fill = div_col("bottom", ifelse(is.na(worstCol), NA, worstCol))) +
      # Add third quantile segment
      geom_polygon(data = get.poly(breaks[2], breaks[3]), aes(x, y), fill = div_col("below", ifelse(is.na(thrCol), NA, thrCol))) +
      # Add second quantile segment
      geom_polygon(data = get.poly(breaks[3], breaks[4]), aes(x, y), fill = div_col("above", ifelse(is.na(secCol), NA, secCol))) +
      # Add top quantile segment
      geom_polygon(data = get.poly(breaks[4], breaks[5]), aes(x, y), fill = div_col("top", ifelse(is.na(bestCol), NA, bestCol))) +
      # Add indicator value
      geom_polygon(data = get.poly(pos - 1, pos + 1, 0.2), aes(x, y), alpha = 0.5, fill = div_col("b_text", ifelse(is.na(textCol), NA, textCol))) +
      # Add partition text
      annotate("text", x = -0.7, y = 0.2, label = toupper("Bottom 3"), vjust = 0, size = 4, fontface = "bold", color = div_col("b_text", ifelse(is.na(textCol), NA, textCol))) +
      annotate("text", x = 0.7, y = 0.2, label = toupper("Top 3"), vjust = 0, size = 4, fontface = "bold", color = div_col("b_text", ifelse(is.na(textCol), NA, textCol))) +
      annotate("text", x = 0.3, y = 0.7, label = toupper(ifelse(teams == 12, "Top 6", "Top Half")), vjust = 0, size = 4, fontface = "bold", color = div_col("b_text", ifelse(is.na(textCol), NA, textCol))) +
      annotate("text", x = -0.3, y = 0.7, label = toupper(ifelse(teams == 12, "Bottom 6", "Bottom Half")), vjust = 0, size = 4, fontface = "bold", color = div_col("b_text", ifelse(is.na(textCol), NA, textCol))) +
      # Add KPI text
      annotate("text", x = 0, y = 0, label = toupper(paste0(KPI)), vjust = 0, size = 8, fontface = "bold") +
      # Fix coordinates in place to create a circular plot
      coord_fixed() +
      # Add title
      labs(title = "Your rank based on all games") +
      # Set theme
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent")
      )
  }

  # Call the gg.gauge function with the specified percentile value
  # and return the gauge plot
  return(gg.gauge(pos = value, breaks = breaks))
}
