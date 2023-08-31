#' Competition Order
#'
#' Orders divisions by age and division level.
#' This function is designed for Wyscout competition IDs.
#'
#' @param competitionId Wyscout competition ID.
#'
#' @return An integer rank indicating the order.
#' @export
#'
#' @examples
#'
#' A <- data.frame(competitionId = c(355, 328, 100))
#' A$Order <- comp_order(A$competitionId)
#'
comp_order <- function(competitionId) {
  if (!is.numeric(competitionId)) {
    stop("Inputs are not numeric")
  }

  # Return integer rank based on age and level - if not in danish male league system return 999
  y <- dplyr::case_when(
    # Senior
    competitionId == 355 ~ 1, # Superliga
    competitionId == 328 ~ 2, # 1. Division
    competitionId == 329 ~ 3, # 2. Division
    competitionId == 43319 ~ 4, # 3. Division

    # Youth
    competitionId == 1305 ~ 11, # U19 Liga
    competitionId == 3134 ~ 12, # U19 Division

    competitionId == 1570 ~ 21, # U17 Liga
    competitionId == 3135 ~ 22, # U17 Division

    competitionId == 43149 ~ 31, # U15 Liga

    # Other
    T ~ 999
  )

  # Return order as vector
  return(y)
}
