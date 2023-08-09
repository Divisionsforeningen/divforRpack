test_sim_win_prob <- function() {
  # Test case 1
  home_team1 <- c(0.2, 0.1)
  away_team1 <- c(0.1, 0.2)
  expected_probs1 <- c(home = 0.09, draw = 0.8, away = 0.09)

  result_probs1 <- sim_win_prob(home_team1, away_team1)

  expect_equal(result_probs1, expected_probs1, tolerance = 0.05)

  # Test case 2
  home_team2 <- c(0.2, 0.3, 0.3)
  away_team2 <- c(0.1, 0.3, 0.4)
  expected_probs2 <- c(home = 0.08, draw = 0.81, away = 0.09)

  result_probs2 <- sim_win_prob(home_team2, away_team2)

  expect_equal(result_probs2, expected_probs2, tolerance = 0.05)

  # Test case 3 (Invalid input)
  invalid_home <- c(1.2, 0.8)
  invalid_away <- c(0.9, 0.7)

  expect_error(sim_win_prob(invalid_home, invalid_away), "All elements in home and away vectors must be less than 1.")
}


test_that("Simulate win probabilities working as intended", {
  test_sim_win_prob()
})
