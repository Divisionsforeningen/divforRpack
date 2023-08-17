#' goal_mouth
#'
#' @param team Selected team - from input variable as default
#' @param season Selected season - from from input variable as default
#' @param start Start of period - from from input variable as default
#' @param end End of period - from from input variable as default
#' @param df Data with shots on target
#' @param z Column with goal mouth z coordinate
#' @param y Column with goal mouth y coordinate
#' @param type Column containing event types ("Goal" is used, other types are discounted)
#' @param goalCol Point color for goals
#' @param saveCol Point color for saves
#' @param provider Provider for caption
#' @param keeperDetails Binary - if true adds keeper name to plot
#' @param teams Column containing team names
#' @param name Column containing keeper names
#'
#' @return Adds shots to the goal_frame plot - highlighting goals
#' @export
#' @import ggplot2
#'
goal_mouth <- function(team = input$sel_team_opp, season = input$sel_season, start = input$gw_prep_def_slider[1], end = input$gw_prep_def_slider[2], df, z = "gk_z", y = "gk_y", type = "Event_type", goalCol = NA, saveCol = NA, provider = "OPTA", keeperDetails = TRUE, teams = "Conceding team", name = "Conceding keeper") {
  # TODO Write test battery

  # Add shots to goal_frame plot
  p <- divforRpack::goal_frame() +
    # Add points to goalmouth
    geom_point(data = df, aes(
      x = .data[[z]], y = 100 - .data[[y]],
      fill = ifelse((.data[[type]] == "Goal"), "Goal", "Save"),
      alpha = ifelse(.data[[type]] == "Goal", 1, 0.9)
    ), pch = 21, size = 7) +
    # Defines colors
    scale_fill_manual("", values = c(
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
      filter(.data[[teams]] == team) %>%
      select(.data[[name]]) %>%
      head(1)

    p <- p +
      annotate("text", x = 50, y = 50, label = paste0(keeper), size = 10)
  }

  print(p)
}
