#' Bar Chart
#'
#' Creates a bar chart for comparing multiple teams, highlighting a specific team with an optional reference line.
#'
#' @param df A data frame containing KPI and label information.
#' @param x Column containing the KPI values.
#' @param y Column containing the labels.
#' @param id ID of the team to be highlighted (default: NA, no team highlighted).
#' @param KPI The selected KPI (from Shiny input) - default: "Name of KPI".
#' @param negative Set to TRUE if lower values are better for the KPI (default: FALSE).
#' @param median Use median instead of mean for the reference line (default: FALSE).
#' @param fillCol Color of the bars for non-highlighted IDs (default: NA).
#' @param highCol Color for the bars of the highlighted ID (default: NA).
#' @param refCol Color of the reference line (default: NA).
#'
#' @return A bar plot where the chosen ID is highlighted, and the mean (or median) is indicated with a red line.
#' The ordering of bars is determined by the "negative" parameter.
#'
#' @import ggplot2
#' @import grDevices
#' @import dplyr
#' @importFrom stats reorder
#'
#' @export
#'
bar_chart <- function(df, x, y, id = NA, KPI = "Name of KPI", negative = FALSE, median = FALSE, fillCol = NA, highCol = NA, refCol = NA) {
  # Throw error message if negative is not TRUE/FALSE
  if (negative %nin% c(TRUE, FALSE)) {
    stop("Negative argument needs to be either TRUE or FALSE")
  }

  # Throw error message if median is not TRUE/FALSE
  if (median %nin% c(TRUE, FALSE)) {
    stop("Median argument needs to be either TRUE or FALSE")
  }

  # If id is NA highlight top/bottom depending on negative
  if (is.na(id)) {
    if (negative == T) {
      id <- as.character((df %>% filter(.data[[y]] == min(.data[[y]])) %>% reframe(.data[[x]]))[1])
      print("No id given - defaulting to lowest value")
    } else {
      id <- as.character((df %>% filter(.data[[y]] == max(.data[[y]])) %>% reframe(.data[[x]]))[1])
      print("No id given - defaulting to highest value")
    }
  }

  # Calculate the reference value based on mean or median
  ref <- ifelse(median == TRUE,
    as.numeric(df %>% mutate(median = median(.data[[y]])) %>% summarise(median = max(median))),
    as.numeric(df %>% mutate(mean = mean(.data[[y]])) %>% summarise(mean = max(mean)))
  )

  # Create ggplot from df, using x, and y. Reorder if needed
  ggplot(data = df, aes(x = stats::reorder(.data[[x]], .data[[y]], decreasing = negative), y = .data[[y]])) +
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
    geom_hline(aes(yintercept = ref, color = "League average"), linewidth = 2, alpha = 0.8) +
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
