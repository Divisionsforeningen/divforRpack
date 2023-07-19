#' head_to_head
#'
#' @param df Data frame
#' @param home Name of home team
#' @param textCol Color used for text
#' @param provider Data provider
#' @param title Title - defaults to HEAD TO HEAD
#'
#' @return A head-to-head chart with data from a match
#' @export
#' @import ggplot2
#'
head_to_head <- function(df, home = NA, textCol = NA, provider = "OPTA", title = "HEAD TO HEAD") {
  # TODO Maybe it works?

  if (is.na(home)) {
    stop("No home team defined")
  }
  if (is.na(names)) {
    stop("No colum of team names given")
  }

  high <- max(df$share)

  ggplot(df, aes(x = variable, y = ifelse(OfficialName %in% home, -share, share), fill = OfficialName)) +
    geom_bar(stat = "identity", position = "identity", width = 0.7) +
    geom_label(aes(label = round(value, digits = 1)),
      size = 7,
      color = ifelse(is.na(textCol), div_col(type = "w_text"), div_col(color = textCol)), show.legend = FALSE
    ) +
    scale_y_continuous(limits = c(-high, high)) +
    labs(
      title = title,
      caption = paste0("Data from ", provider)
    ) +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 15),
      plot.title = element_text(hjust = 0.5, size = 15),
      legend.position = "left",
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 12)
    ) +
    coord_flip() +
    labs(
      x = "",
      y = ""
    ) +
    theme_minimal()
}
