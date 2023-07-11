sim_win_prob <- function(home, away) {
  m <- 10000

  r <- data.frame(match = c(1:m), homeG = 0, awayG = 0)
  for (j in 1:m) {
    hg <- data.frame(a = c(1:length(home)), b = 0)
    for (i in home) {
      hg <- sum(i > runif(1))
      # ifelse(i>runif(1),hg+1,hg)
    }
    r[j, 2] <- hg
    ag <- data.frame(a = c(1:length(away)), b = 0)
    for (i in 1:length(away)) {
      ag[i, 2] <- sum(away[i] > runif(1))
    }
    r[j, 3] <- sum(ag$b)
  }
  home <- sum(ifelse(r$homeG > r$awayG, 1, 0)) / m
  draw <- sum(ifelse(r$homeG == r$awayG, 1, 0)) / m
  away <- sum(ifelse(r$homeG < r$awayG, 1, 0)) / m

  return(c(home, draw, away))
}

sim_win_prob_dat <- function(home, away) {
  m <- 10000

  r <- data.frame(match = c(1:m), homeG = 0, awayG = 0)
  for (j in 1:m) {
    hg <- data.frame(a = c(1:length(home)), b = 0)
    for (i in home) {
      hg <- sum(i > runif(1))
      # ifelse(i>runif(1),hg+1,hg)
    }
    r[j, 2] <- hg
    ag <- data.frame(a = c(1:length(away)), b = 0)
    for (i in 1:length(away)) {
      ag[i, 2] <- sum(away[i] > runif(1))
    }
    r[j, 3] <- sum(ag$b)
  }
  home <- sum(ifelse(r$homeG > r$awayG, 1, 0)) / m
  draw <- sum(ifelse(r$homeG == r$awayG, 1, 0)) / m
  away <- sum(ifelse(r$homeG < r$awayG, 1, 0)) / m

  return(r)
}
