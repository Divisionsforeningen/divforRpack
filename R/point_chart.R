point_chart <- function() {
  # TODO Build function & documentation

  ggplot(df_par, aes(x = (value - 0.1) * 100, y = 1, group = 1)) +
    geom_point(aes(color = "All players with similar position"), alpha = 0.1, size = 5) +
    geom_point(data = df_par %>%
      filter(FullName == row_click()), aes(color = "Scouted player"), size = 10) +
    scale_color_manual(NULL, values = c("Scouted player" = "yellow", "All players with similar position" = "white")) +
    facet_wrap(~variable, ncol = 1) +
    scale_x_continuous(name = "Percentile", breaks = seq(0, 100, by = 10)) +
    labs(
      x = "Percentile",
      y = ""
    ) +
    theme(
      strip.text = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
}
