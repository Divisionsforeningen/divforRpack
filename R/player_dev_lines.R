#' player_dev_lines
#'
#' @param player_df Data frame with player data
#' @param competitionId Coloum name of Wyscout competitionId
#' @param season Coloum name of x axis
#' @param value Coloum name of y axis
#' @param comp Coloum name of competition name for facet_wrap
#' @param KPI Name of y axis KPI
#' @param provider Name of data provider
#'
#' @return Plot of KPI by season - facet_wrap by competition - order by age and level
#' @export
#'
player_dev_lines <- function(player_df, competitionId = "competitionId", season = "season", value = "value", comp = "comp", KPI = "Put KPI here", provider = "Put provider here") {
  # Get ordering from divforRpack::comp_order()
  player_df <- player_df %>% mutate(order = divforRpack::comp_order(.data[[competitionId]]))

  # Create values for factoring
  labels <- as.list(unique(player_df %>% dplyr::arrange(order) %>% dplyr::select(.data[[comp]]))) %>% unlist()
  levels <- as.list(unique(player_df %>% dplyr::arrange(order) %>% select(order))) %>% unlist()

  # Convert to factor for plotting - with names and levels
  player_df$order <- factor(player_df$order, levels = levels, labels = labels)

  # Create plot with x and y axis
  ggplot(player_df, aes(x = as.factor(.data[[season]]), y = .data[[value]], group = 1)) +
    # Add points to graph
    geom_point(aes(), size = 2, alpha = 0.3) +
    # Add data points to graph
    geom_text_repel(data = player_df, aes(label = .data[[value]]), size = 5, nudge_x = 0.1) +
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
