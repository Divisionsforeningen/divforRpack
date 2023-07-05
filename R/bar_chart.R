#' bar_chart
#'
#' @param df Data frame containing KPI and Label, x and y
#' @param x Column containing KPI
#' @param y Label
#' @param id Id to highlight
#' @param z Selected KPI - from shiny input
#' @param positive True if higher is better for KPI
#' @param median Use median instead of mean
#' @param fillCol Color of non-highlighted ids
#' @param highCol Highlighted id color
#' @param refCol Color of reference line
#'
#' @return A bar plot, chosen id is highlighted, and mean is added as a red line - ordering is done by "positive"
#' @export
#'
#' @examples bar_chart(df, x, y, id, z, TRUE, FALSE, NA, NA, NA)
bar_chart <- function(df, x, y, id, z, positive = c(TRUE, FALSE), median = c(TRUE, FALSE), fillCol = NA, highCol = NA, refCol = NA) {
  ggplot(df, aes(x = x, y = reorder(x, mean, decreasing = positive))) +
    geom_bar(stat = "identity", width = 0.3, fill = div_col("fill", ifelse(is.na(fillCol), NA, fillCol))) +
    geom_bar(data = df %>%
      filter(y == id), aes(), stat = "identity", fill = div_col("highlight", ifelse(is.na(highCol), NA, highCol)), width = 0.3) +
    geom_point(data = df %>%
      filter(y != id), aes(), size = 6, color = div_col("fill", ifelse(is.na(fillCol), NA, fillCol))) +
    geom_point(data = df %>%
      filter(y == id), size = 6, color = div_col("highlight", ifelse(is.na(highCol), NA, highCol))) +
    geom_vline(aes(xintercept = ifelse(median == TRUE, median(df$x), mean(df$x)), color = "League average"), size = 2, alpha = 0.8) +
    scale_color_manual("", values = c("League average" = div_col("reference", ifelse(is.na(refCol), NA, refCol)))) +
    labs(
      x = paste0(z),
      y = "",
      title = "Your rank compared to the chosen teams"
    ) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12)
    )
}
