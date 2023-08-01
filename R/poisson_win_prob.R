#' poisson_win_prob
#'
#' Calculates win probability between home and away using stats::dpois()
#'
#' @param home_xg xG of home team
#' @param away_xg xG of away team
#'
#' @return A set of probabilities HomeWin %,Draw %, AwayWin %
#' @export
#' @importFrom stats dpois
#'
#' @examples poisson_win_prob(home_xg = 1.2, away_xg = 1)
poisson_win_prob <- function(home_xg, away_xg) {
  matchResult <- c(0, 0, 0)

  for (home_score in 0:15) {
    for (Game_away_score in 0:15) {
      scoreProbability <- stats::dpois(home_score, home_xg) * stats::dpois(Game_away_score, away_xg)

      if (home_score > Game_away_score) {
        matchResult[1] <- matchResult[1] + scoreProbability
      } else if (home_score == Game_away_score) {
        matchResult[2] <- matchResult[2] + scoreProbability
      } else {
        matchResult[3] <- matchResult[3] + scoreProbability
      }
    }
  }

  return(matchResult)
}
