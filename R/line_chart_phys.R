line_chart_phys <- function() {
  # TODO Build function & documentation


  ggplot(ss_summary, aes(Date, get(input$sel_phys_par), group = 1)) +
    geom_point(data = ss_summary %>%
      filter(`Team Name` == as.name(id_name)), size = 2, alpha = 0.5, color = "lightgrey") +
    geom_line(data = ss_summary %>%
      filter(`Team Name` == as.name(id_name)), size = 1, alpha = 0.5, aes(color = "Game by game")) +
    geom_smooth(data = ss_summary %>%
      filter(`Team Name` == as.name(id_name)), aes(color = "Team progression"), size = 2, se = FALSE) +
    stat_smooth(method = "lm", formula = y ~ x, level = 0.95, size = 2, aes(color = "League progression", fill = "95% confidence interval")) +
    geom_smooth(data = ss_summary %>%
      filter(`Team Name` == as.name(input$sel_phys_overlay)), aes(color = "Overlayed team"), size = 2, se = FALSE) +
    scale_color_manual(NULL, values = c("Overlayed team" = "yellow", "League progression" = "blue", "Team progression" = "red", "Game by game" = "lightgrey")) +
    scale_fill_manual(NULL, values = ("95% confidence interval" <- "lightgrey")) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%B"
    ) +
    guides(
      color = guide_legend(override.aes = list(fill = NA), order = 1),
      fill = guide_legend(override.aes = list(color = NA), order = 2)
    ) +
    geom_point(data = ss_summary %>%
      filter(`Team Name` == id_name & Date == as.Date(d_id)), size = 8, alpha = 0.4, color = "yellow") +
    geom_text_repel(data = ss_summary %>%
      filter(`Team Name` == id_name & Date == as.Date(d_id)), size = 5, nudge_y = -1, aes(label = paste0(round(get(input$sel_phys_par), digits = 1)))) +
    labs(
      title = "Progression during season",
      subtitle = as.name(id_name),
      y = input$sel_phys_par,
      x = "",
      caption = "Data from Second Spectrum"
    ) +
    theme(
      axis.text.y = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 12),
      legend.position = "bottom"
    )
}
