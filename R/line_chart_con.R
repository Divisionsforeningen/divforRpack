#' line_chart_con
#'
#' Line chart - not sure what it does...
#'
#' @param df Data frame
#' @param x Column with data for x axis
#' @param y Column with data for y axis
#' @param idCl Column with data containing ids
#' @param id Id to show
#' @param ref Reference value on y axis
#' @param KPI Y axis label
#' @param Periods X axis label
#' @param Title Title of plot
#' @param Subtitle Subtitle of plot
#' @param Caption Caption for data provider
#' @param refCol Color of reference line
#'
#' @return Line chart on continuous x axis
#' @export
#' @import ggplot2
#' @import dplyr
#' @import ggrepel
#'
line_chart_con <- function(df, x, y, idCl, id, ref = NA, KPI, Periods, Title = NA, Subtitle = NA, Caption = "Data by OPTA", refCol = NA) {
  # TODO Test if it works!
  # TODO Change description!
  # Start ggplot with data, filtering(?) - and setting x, y
  ggplot(
    data = df %>% filter(idCl %in% id),
    aes(x = x, y = y, group = 1)
  ) +
    # Add line
    geom_path(aes(), alpha = 0.0) +
    # Add smoothed line for tendency
    geom_smooth(aes(), se = FALSE) +
    # Add points
    geom_point(aes(), size = 4, alpha = 0.1) +
    # Add labels
    ggrepel::geom_text_repel(aes(label = id)) +
    # Add reference line
    geom_hline(aes(yintercept = ref), color = div_col("reference", ifelse(is.na(refCol), NA, refCol)), linetype = "dashed") +
    # Scale x axis
    scale_x_continuous(breaks = seq(from = min(df$x), to = max(df$x), by = 1)) +
    # Add labels, title and subtitle - caption with data provider
    labs(
      y = KPI,
      x = Periods,
      title = Title,
      subtitle = Subtitle,
      caption = Caption
    ) +
    # Control theme
    theme(
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 15),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 12)
    )
}
