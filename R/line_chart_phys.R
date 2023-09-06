#' Line Chart for Physical Performance
#'
#' Generate a line chart to visualize physical performance data.
#'
#' @param df A data frame containing the physical performance data.
#' @param date Column name for the date variable (default: "Date").
#' @param parameter Selected physical parameter (default: input$sel_phys_par).
#' @param name Column name for team names (default: "TeamName").
#' @param team Selected team (default: NA).
#' @param match Selected match (default: NA).
#' @param compare Selected teams for comparison (default: input$sel_phys_overlay).
#' @param debug Debugging flag (0 or 1, default: 0).
#'
#' @return A line chart for visualizing physical performance data.
#' @export
#'
#' @examples
#' line_chart_phys(df = phys_data, parameter = "Total Distance (km)", team = "Vejle Boldklub", compare = "FC Midtjylland")
line_chart_phys <- function(df, date = "Date", parameter = input$sel_phys_par, name = "TeamName", team = NA, match = NA, compare = input$sel_phys_overlay, debug = 0) {
  # TODO Build function & documentation

  if (debug == 1) {
    parameter <- "Total Distance (km)"
    compare <- "FC Midtjylland"
    team <- "Vejle Boldklub"
    match <- "2021-07-18"
  }

  if (is.na(team)) {
    stop("No team chosen!")
  }

  if (is.na(match)) {
    stop("No match chosen!")
  }

  # Create ggplot with date and selected variable as x and y
  ggplot(df, aes(.data[[date]], .data[[parameter]], group = 1)) +
    # Add points for selected team
    geom_point(data = df %>%
      filter(.data[[name]] == team), size = 2, alpha = 0.5, color = divforRpack::div_col(type = "fill")) +
    # Add line between points for selected teams
    geom_line(data = df %>%
      filter(.data[[name]] == team), linewidth = 1, alpha = 0.5, aes(color = "Game by game")) +
    # Add trend for selected teams
    geom_smooth(data = df %>%
      filter(.data[[name]] == team), aes(color = "Team progression"), linewidth = 2, se = FALSE) +
    # Add trend for league
    stat_smooth(method = "lm", formula = y ~ x, level = 0.95, linewidth = 2, aes(color = "League progression", fill = "95% confidence interval")) +
    # Add trend for team to compare with
    geom_smooth(data = df %>%
      filter(.data[[name]] == compare), aes(color = "Overlayed team"), linewidth = 2, se = FALSE) +
    # Set colors
    scale_color_manual(NULL, values = c(
      "Overlayed team" = divforRpack::div_col(type = "chosen"),
      "League progression" = divforRpack::div_col(type = "highlight"),
      "Team progression" = divforRpack::div_col(type = "reference"),
      "Game by game" = divforRpack::div_col(type = "fill")
    )) +
    # Set fill color
    scale_fill_manual(NULL, values = ("95% confidence interval" <- divforRpack::div_col(type = "fill"))) +
    # Break by month
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%B"
    ) +
    # Define guides
    guides(
      color = guide_legend(override.aes = list(fill = NA), order = 1),
      fill = guide_legend(override.aes = list(color = NA), order = 2)
    ) +
    # Add point for selected game
    geom_point(data = df %>%
      filter(.data[[name]] == team & Date == as.Date(match)), size = 8, alpha = 0.4, color = divforRpack::div_col(type = "chosen")) +
    # Add label for selected game
    geom_text_repel(data = df %>%
      filter(.data[[name]] == team & Date == as.Date(match)), size = 5, nudge_y = -1, aes(label = paste0(round(.data[[parameter]], digits = 1)))) +
    # Define labels and caption
    labs(
      title = "Progression during season",
      subtitle = team,
      y = parameter,
      x = "",
      caption = "Data from Second Spectrum"
    ) +
    # Theme
    theme(
      axis.text.y = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 12),
      legend.position = "bottom"
    )
}
