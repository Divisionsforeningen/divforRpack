#' Point Chart
#'
#' Creates a point chart for comparing players' values using facet wrap for visualization.
#'
#' @param df Data frame containing players, the value to compare, and the facet variable.
#' @param name Column with player names. (Default: "FullName")
#' @param value The value to compare in percentile format. (Default: "value")
#' @param facet Variable to use for facet wrap. (Default: "variable")
#' @param playerCol Highlighted player color.
#' @param otherCol Color for other players.
#' @param replaced Value in "name" column to highlight as replaced player. (Default: "TEST")
#' @param repCol Color for player to replace.
#' @param debug Debug mode. (Default: 0)
#'
#' @return Point chart used in the scouting module - depends on row_click() being defined.
#' @export
#'
point_chart <- function(df, name = "FullName", value = "value", facet = "variable", replaced = "TEST", playerCol = NA, repCol = NA, otherCol = NA, debug = 0) {
  # TODO Write test battery
  highlight <- ifelse(debug == 1, "Bjorn Kopplin", row_click())

  # Start ggplot with data and value
  ggplot(df, aes(x = (.data[[value]] - 0.1) * 100, y = 1, group = 1)) +
    # Set color of all players
    geom_point(aes(fill = "All players with similar position"), pch = 21, alpha = 0.1, size = 5) +
    # Highlight chosen player - requires row_click() to be defined
    geom_point(data = df %>%
      dplyr::filter(.data[[name]] == highlight), aes(fill = "Scouted player"), pch = 21, size = 10) +
    # Highlight replaced player
    geom_point(data = df %>%
      dplyr::filter(.data[[name]] == replaced), aes(fill = "Player to replace"), pch = 21, size = 10) +
    # Set colors
    scale_fill_manual(NULL, values = c(
      "All players with similar position" = ifelse(is.na(otherCol), div_col(type = "others"), otherCol),
      "Scouted player" = ifelse(is.na(playerCol), div_col(type = "chosen"), playerCol),
      "Player to replace" = ifelse(is.na(repCol), div_col(type = "reference"), repCol)
    )) +
    # Facet wrap by chosen variable
    facet_wrap(~ .data[[facet]], ncol = 1) +
    # Set up x axis
    scale_x_continuous(name = "Percentile", breaks = seq(0, 100, by = 10)) +
    # Remove label from y and set x label
    labs(
      x = "Percentile",
      y = ""
    ) +
    # Add theme
    theme(
      strip.text = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
}
