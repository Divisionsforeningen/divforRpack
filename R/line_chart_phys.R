line_chart_phys <- function() {
  # TODO Build function & documentation
  # Create ggplot with date and selected variable as x and y
  ggplot(ss_summary, aes(Date, get(input$sel_phys_par), group = 1)) +
    # Add points for selected team
    geom_point(data = ss_summary %>%
      filter(`Team Name` == as.name(id_name)), size = 2, alpha = 0.5, color = "lightgrey") +
    # Add line between points for selected teams
    geom_line(data = ss_summary %>%
      filter(`Team Name` == as.name(id_name)), size = 1, alpha = 0.5, aes(color = "Game by game")) +
    # Add trend for selected teams
    geom_smooth(data = ss_summary %>%
      filter(`Team Name` == as.name(id_name)), aes(color = "Team progression"), size = 2, se = FALSE) +
    # Add trend for league
    stat_smooth(method = "lm", formula = y ~ x, level = 0.95, size = 2, aes(color = "League progression", fill = "95% confidence interval")) +
    # Add trend for team to compare with
    geom_smooth(data = ss_summary %>%
      filter(`Team Name` == as.name(input$sel_phys_overlay)), aes(color = "Overlayed team"), size = 2, se = FALSE) +
    # Set colors
    scale_color_manual(NULL, values = c("Overlayed team" = "yellow", "League progression" = "blue", "Team progression" = "red", "Game by game" = "lightgrey")) +
    # Set fill color
    scale_fill_manual(NULL, values = ("95% confidence interval" <- "lightgrey")) +
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
    geom_point(data = ss_summary %>%
      filter(`Team Name` == id_name & Date == as.Date(d_id)), size = 8, alpha = 0.4, color = "yellow") +
    # Add label for selected game
    geom_text_repel(data = ss_summary %>%
      filter(`Team Name` == id_name & Date == as.Date(d_id)), size = 5, nudge_y = -1, aes(label = paste0(round(get(input$sel_phys_par), digits = 1)))) +
    # Define labels and caption
    labs(
      title = "Progression during season",
      subtitle = as.name(id_name),
      y = input$sel_phys_par,
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
