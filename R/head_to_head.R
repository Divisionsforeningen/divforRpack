#' Head-to-Head Chart
#'
#' Creates a head-to-head chart comparing the given data for two teams in a match.
#'
#' @param df Data frame containing the data for the head-to-head chart.
#' @param x Column with variables (e.g., match statistics or metrics).
#' @param y Column with shares (e.g., share of possession or goals).
#' @param value Column with numeric values (e.g., actual values of the metrics).
#' @param names Column with team names.
#' @param home Name of the home team (used for orientation).
#' @param textCol Color used for text labels (optional).
#' @param provider Data provider (default: "OPTA").
#' @param title Title of the chart (default: "HEAD TO HEAD").
#'
#' @return A head-to-head chart comparing the given data for two teams in a match.
#' @export
#' @import ggplot2
#'
head_to_head <- function(df, x = "variable", y = "share", value = "value", names = "OfficialName", home = NA, textCol = NA, provider = "Opta", title = "HEAD TO HEAD") {
  # Check if the 'home' parameter is provided.
  if (is.na(home)) {
    stop("No home team defined")
  }

  # Check if the 'names' parameter is provided.
  if (is.na(names)) {
    stop("No column of team names given")
  }

  # Calculate the maximum value for 'y' to set the y-axis limits.
  high <- as.numeric(df %>% mutate(max = max(.data[[y]])) %>% summarise(max = max(max)))

  # Create the ggplot chart.
  ggplot(df, aes(x = .data[[x]], y = ifelse(.data[[names]] %in% home, -.data[[y]], .data[[y]]), fill = .data[[names]])) +
    # Add bar plot for head-to-head comparison.
    geom_bar(stat = "identity", position = "identity", width = 0.7) +
    # Add labels to the bars.
    geom_label(aes(label = round(value, digits = 1)),
      size = 7,
      # Set text color for the labels.
      color = ifelse(is.na(textCol), div_col(type = "w_text"), div_col(color = textCol)), show.legend = FALSE
    ) +
    # Set y-axis limits to ensure bars are centered.
    scale_y_continuous(limits = c(-high, high)) +
    # Set chart title and caption.
    labs(
      title = title,
      caption = paste0("Data from ", provider)
    ) +
    # Customize chart theme.
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 15),
      plot.title = element_text(hjust = 0.5, size = 15),
      legend.position = "left",
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 12)
    ) +
    # Rotate the chart to display horizontally.
    coord_flip() +
    # Remove axis labels.
    labs(
      x = "",
      y = ""
    ) +
    # Use minimal theme for the plot.
    theme_minimal()
}
