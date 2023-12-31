#' Simulate Win Probabilities
#'
#' Calculate win probabilities for home, draw, and away outcomes of a match
#' using a Monte Carlo simulation approach. This model balances extreme cases
#' at the expense of some computation speed.
#'
#' @param home A numeric vector representing the home team's shots.
#' @param away A numeric vector representing the away team's shots.
#' @param m The number of simulations. (Default: 100000)
#'
#' @return A numeric vector containing probabilities of home, draw, and away outcomes.
#'
#' @export
#'
#' @examples
#' sim_win_prob(home = c(0.6, 0.8), away = c(0.45, 0.06), m = 50000)
sim_win_prob <- function(home, away, m = 100000) {
  # Attempt at a better simulation model - handles extremes better at the cost of some speed

  # Input validation: Ensure all elements are less than 1
  if (any(home >= 1) || any(away >= 1)) {
    stop("All elements in home and away vectors must be less than 1.")
  }

  # Calculate the number of home and away teams
  num_home <- length(home)
  num_away <- length(away)

  # Generate a matrix of random numbers for simulations
  random_matrix <- matrix(runif(m * max(num_home, num_away)), nrow = m)

  # Create matrices for element-wise comparisons with home and away strengths
  home_gt_matrix <- matrix(home > random_matrix[, 1:num_home], nrow = num_home, byrow = TRUE)
  away_gt_matrix <- matrix(away > random_matrix[, 1:num_away], nrow = num_away, byrow = TRUE)

  # Calculate scores for home and away teams
  home_scores <- colSums(home_gt_matrix)
  away_scores <- colSums(away_gt_matrix)

  # Create a matrix to store results
  results <- matrix(0, nrow = m, ncol = 3)
  results[, 1] <- home_scores
  results[, 2] <- away_scores
  results[, 3] <- sign(home_scores - away_scores)

  # Add column names to the results matrix
  colnames(results) <- c("HomeGoals", "AwayGoals", "Result")

  # Convert the results matrix to a data frame
  results <- data.frame(results)

  # Calculate win, draw, and lose probabilities
  r <- sum(results$Result == 1) / m
  d <- sum(results$Result == 0) / m
  a <- sum(results$Result == -1) / m

  # Return a numeric vector with probabilities
  return(c(home = r, draw = d, away = a))
}
