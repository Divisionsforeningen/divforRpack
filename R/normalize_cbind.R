#' normalize
#'
#' Normalizes a series of values to be between 0 and 1
#'
#' @param df Data frame to work on and return
#' @param x Column to use for normalization
#' @param type Choose between Z-score ("Z") or 0 to 1 normalization ("1")
#'
#' @return Series of values scaled between 0 and 1
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'   OfficialName = c("A", "B", "C", "D"),
#'   mean = runif(4)
#' )
#'
#' df <- normalize_cbind(df, x = df$mean, type = "1")
#'
#' df <- normalize_cbind(df, x = df$mean, type = "Z")
normalize_cbind <- function(df, x, type = NA) {
  if (!is.numeric(x)) {
    stop("Inputs are not numeric")
  }

  if (type %nin% c("Z", "1")) {
    stop("Selected type is not usable")
  }

  # TODO Write test battery
  if (type == "1") {
    # Takes a series of values and returns the 0-1 scaled version
    norm1 <- (x - min(x)) / (max(x) - min(x))
    df <- cbind(df, norm1)

    return(df)
  } else if (type == "Z") {
    # Takes a series of values and returns the z-score
    normZ <- (x - mean(x)) / sd(x)
    df <- cbind(df, normZ)

    return(df)
  } else {
    (
      "Something is not right!"
    )
  }
}
