% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_pitch.R
\name{theme_pitch}
\alias{theme_pitch}
\title{Removes background and axes details from a ggplot plot.}
\usage{
theme_pitch(aspect_ratio = 68/105)
}
\arguments{
\item{aspect_ratio}{Aspect ratio (\code{y / x}) for the plot.
Use \code{NULL} to let the plot take any aspect ratio.}
}
\value{
list of ggplot themes to be added to a ggplot plot
}
\description{
Functionally very similar to \code{ggplot2::theme_void}.
}
\examples{
library(ggplot2)

shots_data <- data.frame(
  x = c(90, 85, 82, 78, 83),
  y = c(43, 40, 52, 56, 44)
)

p <- ggplot(shots_data, aes(x = x, y = y)) +
  annotate_pitch() +
  geom_point()

# Pitch fixed to 68/105 by default
p + theme_pitch()

# Free aspect
p + theme_pitch(aspect_ratio = NULL)

}
