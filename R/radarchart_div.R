#' radarchart_div
#'
#' Plots a radar chart, up to 4 groups.
#'
#' @param df Dataframe containing the data for the radar chart.
#' @param x Variable names (e.g., categories or attributes).
#' @param y Percentile values for each category.
#' @param group Grouping variable.
#' @param lab Data labels to display on the chart - only writes the first group label.
#' @param col Series of colors - up to 4 - c("blue","black","orange","red") etc.
#' @param title Title for the radar chart.
#' @param subTitle Subtitle for the radar chart.
#' @param provider Data provider for the data source.
#'
#' @return Returns a radar plot for a single team/player.
#' @export
#'
radarchart_div <- function(df = NA, x = NA, y = NA, lab = NA, group = NA, col = NA, title = "Put title here", subTitle = "Put subtitle here", provider = "Put provider here") {
  p <- ggplot() +
    # Add radial lines
    geom_hline(yintercept = seq(0, 1.1, by = 0.10), colour = divforRpack::div_col(type = "fill"), linewidth = 0.2, alpha = 0.8)

  # Get groups to loop over
  groups <- unique(df %>% reframe(.data[[group]]))

  # Checks if there is more than 4 groups - to keep it "pretty"
  if (nrow(groups) > 4) {
    stop("More than 4 groups!")
  }
  # If no colors give, then use standards
  if (length(col) < 1) {
    cols <- c("#007bff", "lightgrey", "orange", "red")
  }

  # If colors given then use them - if not enough provided add some unlikely colors
  cols <- col
  if (length(cols) < nrow(groups)) {
    cols <- c(cols, c("black", "purple", "limegreen", "yellow"))
    print("Weird colors added - give more colors as argument to avoid these")
  }


  # Loop over groups
  for (i in 1:nrow(groups)) {
    # For each group add bars to radar plot
    p <- p + geom_bar(
      data = df %>% dplyr::filter(.data[[group]] == groups[i, 1]),
      aes(x = .data[[x]], y = as.numeric(.data[[y]])),
      stat = "identity", show.legend = FALSE, alpha = 0.5,
      color = "black", fill = divforRpack::div_col(color = cols[i])
    )
    if (i == 1) {
      p <- p +
        # Add data labels to the bars.
        geom_text(
          data = df %>% dplyr::filter(.data[[group]] == groups[i, 1]),
          aes(x = .data[[x]], y = as.numeric(.data[[y]]), label = round(as.numeric(.data[[lab]]), digits = 1)), size = 5
        )
    }
  }

  # Add the rest of gglayers
  p <- p +
    # Remove legends.
    guides(fill = "none", alpha = "none") +
    # Add annotations for the radial lines.
    annotate("text", x = 0, y = 0.1, label = "0") +
    annotate("text", x = 0, y = 0.2, label = "10") +
    annotate("text", x = 0, y = 0.3, label = "20") +
    annotate("text", x = 0, y = 0.4, label = "30") +
    annotate("text", x = 0, y = 0.5, label = "40") +
    annotate("text", x = 0, y = 0.6, label = "50") +
    annotate("text", x = 0, y = 0.7, label = "60") +
    annotate("text", x = 0, y = 0.8, label = "70") +
    annotate("text", x = 0, y = 0.9, label = "80") +
    annotate("text", x = 0, y = 1.0, label = "90") +
    annotate("text", x = 0, y = 1.1, label = "100") +
    # Set chart labels and captions.
    labs(
      x = "",
      y = "",
      title = title,
      subtitle = subTitle,
      caption = paste0("Stats are normalized from 0 to 100. Data from ", provider)
    ) +
    # Set polar coordinates for a radial chart.
    coord_polar() +
    # Set y-axis limits and breaks for percentile values.
    scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1.1, by = 0.10)) +
    # Customize theme settings.
    theme(
      axis.text.y.left = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 12),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = "NA"),
      panel.background = element_rect(fill = "NA")
    )

  # Return plot
  p
}
