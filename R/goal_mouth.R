#' goal_mouth
#'
#' @param team Selected team - from input
#' @param season Selected season - from input
#' @param start Start of period - from input
#' @param end End of period - from input
#' @param df Data with shots
#' @param z Column with goal mouth z coordinate
#' @param y Column with goal mouth y coordinate
#' @param type Column containing event types
#' @param goalCol Point color for goals
#' @param saveCol Point color for saves
#' @param provider Provider for caption
#' @param keeperDetails Binary - if true adds keeper name to plot
#' @param team Column containing team names
#' @param name Column containing keeper names
#'
#' @return Adds shots to the goal_frame plot - highlighting goals
#' @export
#' @import ggplot2
#'
goal_mouth <- function(team = input$sel_team_opp, season = input$sel_season, start = input$gw_prep_def_slider[1], end = input$gw_prep_def_slider[2], df, z, y,type, goalCol = NA, saveCol = NA, provider = "OPTA", keeperDetails = c(TRUE, FALSE), team, name) {
  # TODO Write test battery

  # Add shots to goal_frame plot
  p <- divforRpack::goal_frame() +
    # Add points to goalmouth
    geom_point(data = df, aes(
      x = z, y = 100 - y,
      color = ifelse((type == "Goal"), "Goal", "Save"),
      alpha = ifelse(type == "Goal", 1, 0.9)
    ), size = 7) +
    # Defines colors
    scale_color_manual("", values = c(
      "Goal" = ifelse(is.na(goalCol), div_col(type = "goal"), div_col(color = goalCol)),
      "Save" = ifelse(is.na(saveCol), div_col(type = "fill"), div_col(color = saveCol))
    )) +
    # Add labels and captions
    labs(
      x = "",
      y = "",
      caption = paste0("Data from ", provider)
    ) +
    # Removes legend for alpha
    scale_alpha(guide = "none") +
    # Set theme
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 12)
    ) +
    # Adds text annotations
    annotate("text", x = 45, y = 50, label = paste0(team, " | ", season, " | Gameweek ", start, " to ", end), size = 8)
  # If keeper details is true add name
  if (keeperDetails == TRUE) {
    # FIXME only first keeper? What if multiple?
    keeper <- df %>%
      filter(team == selectedTeam) %>%
      select(name) %>%
      head(1)

    p <- p +
      annotate("text", x = 50, y = 50, label = paste0(keeper), size = 10)
  }

  print(p)
}
