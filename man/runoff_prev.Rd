% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicator_runoff.R
\name{runoff_prev}
\alias{runoff_prev}
\title{Runoff prevention}
\usage{
runoff_prev(
  x,
  runoff_df = NULL,
  rain = 85,
  floors_field = "floors",
  harvest_dist = 10,
  tank_size = c(0, 45)
)
}
\arguments{
\item{x}{An 'sf' object with the urban model of your city and a 'land_use' column with categories
of urban features.}

\item{runoff_df}{A dataframe of categories that are considered impervious area with three columns.
\enumerate{
    \item 'land_uses' with the names of 'land_use' in 'x' to be considered as impervious.
    \item Curve numbers columns 'CN1' and 'CN2' the range of curve number of that function.
    \item 'water_storage', a boolean column indicating whether the land_uses is potentially harvesting
    and storing rainwater using a tank.
}
If NULL, categories and values of 'city_land_uses' are considered.}

\item{rain}{The amount of 24h-rain to be simulated, default is 85 mm.}

\item{floors_field}{The column in 'x' containing the number of floors of each building. Zero is considered
unbuilt areas like gardens or streets. It is used to calculate rainwater harvesting area, since only
upper surface are considered. Missing values are considered as zero.}

\item{harvest_dist}{Maximum distance (in meters) of buildings where to harvest rainwater}

\item{tank_size}{A two-length vector with the range of tank size possibilities, proportional
to the surface of each element where the tank size is located (in l/m2).}
}
\value{
It returns a named vector with values of runoff in mm, total rainfall
and harvested rainwater in cubic meters.
}
\description{
The indicator calculates the runoff prevention considering a rain event, the infiltration
capacity and the rain harvesting and storage capacity.
government.
}
\examples{
# Get the total values of runoff, rainfall and rain harvested
runoff_prev(city_example)

# Adjust the parameters for rain, maximum distance to harvest rainwater and tank size
runoff_prev(city_example, rain = 160, harvest_dist = 5, tank_size = c(20,30))
}
\author{
Josep Pueyo-Ros
}
