#' Poisson Win Probability
#'
#' Calculates win probability between home and away teams using the stats::dpois() function.
#'
#' @param home_xg Expected goals (xG) of the home team.
#' @param away_xg Expected goals (xG) of the away team.
#'
#' @return A set of probabilities: HomeWin %, Draw %, AwayWin %.
#' @export
#' @importFrom stats dpois
#'
#' @examples
#' poisson_win_prob(home_xg = 1.2, away_xg = 1)
#'
poisson_win_prob <- function(home_xg, away_xg) {
  # Original simulation model - useful but not the best in extreme cases

  # Throw error message if input is not numeric
  if(!is.numeric(home_xg) || !is.numeric(away_xg)){
    stop("Input needs to be numeric!")
  }

  # Initial results set to 0
  matchResult <- c(0, 0, 0)

  # Calculate chance of scoring between 0 and 15 goals
  for (home_score in 0:15) {
    for (away_score in 0:15) {
      scoreProbability <- stats::dpois(home_score, home_xg) * stats::dpois(away_score, away_xg)
      # Compare chances of winning and write to results
      if (home_score > away_score) {
        matchResult[1] <- matchResult[1] + scoreProbability
      } else if (home_score == away_score) {
        matchResult[2] <- matchResult[2] + scoreProbability
      } else {
        matchResult[3] <- matchResult[3] + scoreProbability
      }
    }
  }

  # Throw warning if probability is above 100%
  if(sum(matchResult)>1){
    warning("Something might be wrong...")
  }

  # Return results
  return(matchResult)
}
