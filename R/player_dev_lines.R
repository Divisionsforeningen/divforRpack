#' Player Development Lines
#'
#' Creates a plot of a specific KPI by season, facet-wrapped by competition, and ordered by age and level.
#'
#' @param df Data frame with player data.
#' @param competitionId Column name of Wyscout competitionId. (Default: "competitionId")
#' @param season Column name of x axis. (Default: "season")
#' @param value Column name of y axis. (Default: "value")
#' @param comp Column name of competition name for facet_wrap. (Default: "comp")
#' @param KPI Name of y axis KPI. (Default: "Put KPI here")
#' @param provider Name of data provider. (Default: "Put provider here")
#'
#' @return Plot of KPI by season - facet_wrap by competition - ordered by age and level.
#' @export
#'
#'
player_dev_lines <- function(df, competitionId = "competitionId", season = "season", value = "value", comp = "comp", KPI = "Put KPI here", provider = "Put provider here") {
  # TODO Write test battery

  # Get ordering from divforRpack::comp_order()
  df <- df %>% mutate(order = divforRpack::comp_order(.data[[competitionId]]))

  # Create values for factoring
  labels <- as.list(unique(df %>% dplyr::arrange(order) %>% dplyr::select(.data[[comp]]))) %>% unlist()
  levels <- as.list(unique(df %>% dplyr::arrange(order) %>% select(order))) %>% unlist()

  # Convert to factor for plotting - with names and levels
  df$order <- factor(df$order, levels = levels, labels = labels)

  # Create plot with x and y axis
  ggplot(df, aes(x = as.factor(.data[[season]]), y = .data[[value]], group = 1)) +
    # Add points to graph
    geom_point(aes(), size = 2, alpha = 0.3) +
    # Add data points to graph
    geom_text_repel(data = df, aes(label = .data[[value]]), size = 5, nudge_x = 0.1) +
    # Connect points in the same competition over time
    geom_path() +
    # Facet the plot by competition, ordered by divforRpack::comp_order() and named by competition coloum
    facet_wrap(~order, ncol = 1, labeller = labeller(order = comp.labs)) +
    # Change to x axis labels to "Seasons" and add explanation to y axis - also add provider
    labs(
      x = "Seasons",
      y = paste0("Average ", KPI, " per 90 minutes"),
      caption = paste0("Data from ", provider)
    ) +
    # Theme
    theme(
      strip.text = element_text(size = 15),
      axis.text = element_text(size = 10)
    )
}
