#' bar_chart
#'
#' Bar chart for comparison across multiple teams - id team will be highlighted
#' Reference line with either mean or median is added
#' Bar chart can be reordered using positiive=false if "lower is better"
#'
#' @param df Data frame containing KPI and Label, x and y
#' @param x Column containing KPI
#' @param y Label
#' @param id Id to highlight
#' @param KPI Selected KPI - from shiny input
#' @param positive True if higher is better for KPI
#' @param median Use median instead of mean
#' @param fillCol Color of non-highlighted ids
#' @param highCol Highlighted id color
#' @param refCol Color of reference line
#'
#' @return A bar plot, chosen id is highlighted, and mean is added as a red line - ordering is done by "positive"
#' @import ggplot2
#' @import grDevices
#' @import dplyr
#' @importFrom stats reorder
#'
#' @export
#'
bar_chart <- function(df, x, y, id, KPI, negative = c(TRUE, FALSE), median = c(TRUE, FALSE), fillCol = NA, highCol = NA, refCol = NA) {
  # TODO Write test battery

  # Calculate the reference value based on mean or median
  ref <- ifelse(median == TRUE,
    as.numeric(df %>% mutate(median = median(.data[[y]])) %>% summarise(median = max(median))),
    as.numeric(df %>% mutate(mean = mean(.data[[y]])) %>% summarise(mean = max(mean)))
  )

  # Create ggplot from df, using x, and y. Reorder if needed
  ggplot(data = df, aes(x = .data[[x]], y = stats::reorder(.data[[x]], .data[[y]], decreasing = negative))) +
    # Add bar for all data points
    geom_bar(stat = "identity", width = 0.3, fill = div_col("fill", ifelse(is.na(fillCol), NA, fillCol))) +
    # Add bar for highlighted data point
    geom_bar(data = df %>%
      dplyr::filter(.data[[x]] == id), aes(), stat = "identity", fill = div_col("highlight", ifelse(is.na(highCol), NA, highCol)), width = 0.3) +
    # Add point at the end of the bar for all data points
    geom_point(data = df %>%
      dplyr::filter(.data[[x]] != id), aes(), cex = 12, color = div_col("fill", ifelse(is.na(fillCol), NA, fillCol))) +
    # Add point at the end of the bar for highlighted data point
    geom_point(data = df %>%
      dplyr::filter(.data[[x]] == id), cex = 12, color = div_col("highlight", ifelse(is.na(highCol), NA, highCol))) +
    # Add reference line
    geom_vline(aes(xintercept = ref, color = "League average"), linewidth = 2, alpha = 0.8) +
    # Define color of reference line
    scale_color_manual("", values = c("League average" = div_col("reference", ifelse(is.na(refCol), NA, refCol)))) +
    # Define labels and title
    labs(
      x = paste0(KPI),
      y = "",
      title = "Your rank compared to the chosen teams"
    ) +
    # Define location of legend and text size
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12)
    ) +
    coord_flip()
}
