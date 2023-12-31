% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicator_volunteers.R
\name{edible_volunteers}
\alias{edible_volunteers}
\title{The number of volunteers involved in urban agriculture in your city}
\usage{
edible_volunteers(
  x,
  volunteers = c(0.00163, 0.22),
  edible = NULL,
  area_col = "edible_area",
  interval = 0.95,
  verbose = FALSE
)
}
\arguments{
\item{x}{An 'sf' object with the urban model of your city and a 'land_use' column with categories of urban features.}

\item{volunteers}{A vector of length 2 with the range of volunteers involved by square meter of edible gardens.}

\item{edible}{The categories in 'land_uses' that represent community edible gardens. If NULL, land_uses
from 'city_land_uses' dataset area used where volunteers is TRUE.}

\item{area_col}{The column to be used as the area of each feature. If NULL, the area is calculated with
sf::st_area().}

\item{interval}{A numeric value with the confidence interval returned by the function.}

\item{verbose}{If TRUE, the indicators returns a vector (N=1000) with all simulated values.}
}
\value{
If verbose is FALSE, it returns a named vector with the median and the low and high confidence intervals.
Otherwise, it returns a vector of length 1000 with all simulated values.
}
\description{
This indicator estimates the number of volunteers potentially involved in community urban agriculture
initiatives in your city. It uses a range of volunteers per square meter to create the median and the
confidence interval of the number of volunteers by simulating a random uniform distribution of 1000 values
within the provided range. The default range came from required work hours in urban agriculture assessed
in scientific literature, assuming that a volunteers dedicates a 10% of a full-time job.
}
\examples{
# Get the 95\% confidence interval
edible_volunteers(city_example, interval = 0.95)

# Get the raw values from the Monte Carlo simulation
# and adjust the number of volunteers by squared meter.
result <- edible_volunteers(city_example, volunteers = c(0.1, 0.2), verbose = TRUE)
result[1:10]
}
\author{
Josep Pueyo-Ros
}
