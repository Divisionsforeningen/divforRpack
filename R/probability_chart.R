#' Probability Chart
#'
#' Creates a win probability chart using data from the win_prob reactive.
#'
#' @param wp Data from win_prob reactive.
#' @param barCol Color of the bars. (Default: NA)
#' @param textCol Color of the text. (Default: NA)
#' @param accuracy Number of decimals for label values. (Default: 1)
#' @param provider Data provider for the caption. (Default: "Opta")
#'
#' @return Win probability chart.
#' @export
#' @import ggplot2
#'
probability_chart <- function(wp, barCol = NA, textCol = NA, accuracy = 1, provider = "Opta") {
  # Check if provider is a string
  if (!is.character(provider)) {
    stop("Provider is not a string")
  }

  # Rename columns
  colnames(wp) <- "Probability"

  # Define outcomes
  Outcome <- c("Home", "Draw", "Away")

  # Merge data and outcomes
  prob <- cbind(Outcome, wp)

  # Convert outcomes to factors for sorting
  prob$Outcome <- factor(prob$Outcome, levels = c("Home", "Draw", "Away"))

  # Throw warning message if probability is above 100%
  if (sum(prob$Probability) != 1) {
    warning("Probability does not sum to 1")
  }

  # Start ggplot with data, outcome on x and probability on y
  ggplot(prob, aes(x = Outcome, y = Probability)) +
    # Add columns
    geom_col(aes(), alpha = 0.7, fill = ifelse(is.na(barCol), div_col(type = "fill"), div_col(color = barCol))) +
    # Add labels with probability rounded to accuracy
    geom_label(aes(label = paste0(round(Probability * 100, digits = accuracy), " %")),
      size = 7,
      color = ifelse(is.na(textCol), div_col(type = "w_text"), div_col(color = textCol))
    ) +
    # Add tile, subtitle and caption
    labs(
      title = "WIN PROBABILITY",
      caption = paste0("The probability of every outcome based on xG. Data from ", provider, ". Model by Divisionsforeningen.")
    ) +
    # Add theme
    theme(
      plot.title = element_text(size = 15, hjust = 0.5),
      plot.subtitle = element_text(face = "bold", size = 15),
      axis.text = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = 15),
      plot.caption = element_text(size = 12),
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent")
    )
}
