#' goal_frame
#'
#' Defines a goal frame in ggplot for further use, needs to be printed for the plot to show
#'
#' @return A ggplot with a goal frame
#' @export
#' @import ggplot2
#'
#' @examples print(goal_frame())
goal_frame <- function(lowR = NA, lowM = NA, lowL = NA, upR = NA, upM = NA, upL = NA) {
  # TODO Write test battery

  # Define goalframe
  goalframe <- ggplot() +
    # Defines size of plot
    geom_rect(aes(xmin = -1, xmax = 65, ymin = 54.3, ymax = 45.7), alpha = 0.0) +
    # Flip coordinates
    coord_flip() +
    # Define posts and crossbar
    geom_rect(aes(xmin = 0, xmax = 38, ymin = 54.8, ymax = 55.3)) +
    geom_rect(aes(xmin = 0, xmax = 38, ymin = 45, ymax = 45.5)) +
    geom_rect(aes(xmin = 38, xmax = 42, ymin = 45, ymax = 55.3)) +
    # Defines 6 areas in goal - probably from wyscout
    # Lower right
    geom_rect(aes(xmin = 0, xmax = 20, ymin = 51.8, ymax = 54.8), alpha = 0.25, fill = ifelse(is.na(lowR), div_col("fill"), div_col(color = lowR))) +
    # Lower middle
    geom_rect(aes(xmin = 0, xmax = 20, ymin = 48.2, ymax = 51.8), alpha = 0.25, fill = ifelse(is.na(lowM), div_col("fill"), div_col(color = lowM))) +
    # Lower left
    geom_rect(aes(xmin = 0, xmax = 20, ymin = 45.5, ymax = 48.2), alpha = 0.25, fill = ifelse(is.na(lowL), div_col("fill"), div_col(color = lowL))) +
    # Upper right
    geom_rect(aes(xmin = 20, xmax = 38, ymin = 51.8, ymax = 54.8), alpha = 0.25, fill = ifelse(is.na(upR), div_col("fill"), div_col(color = upR))) +
    # Upper middle
    geom_rect(aes(xmin = 20, xmax = 38, ymin = 48.2, ymax = 51.8), alpha = 0.25, fill = ifelse(is.na(upM), div_col("fill"), div_col(color = upM))) +
    # Upper left
    geom_rect(aes(xmin = 20, xmax = 38, ymin = 45.5, ymax = 48.2), alpha = 0.25, fill = ifelse(is.na(upL), div_col("fill"), div_col(color = upL))) +
    # Add theme
    theme(
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(face = "bold", size = 15),
      axis.text = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = 15),
      plot.caption = element_text(size = 12),
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent")
    )

  return(goalframe)
}
