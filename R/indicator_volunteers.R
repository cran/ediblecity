#' @title The number of volunteers involved in urban agriculture in your city
#' @description This indicator estimates the number of volunteers potentially involved in community urban agriculture
#' initiatives in your city. It uses a range of volunteers per square meter to create the median and the
#' confidence interval of the number of volunteers by simulating a random uniform distribution of 1000 values
#' within the provided range. The default range came from required work hours in urban agriculture assessed
#' in scientific literature, assuming that a volunteers dedicates a 10% of a full-time job.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'land_use' column with categories of urban features.
#' @param volunteers A vector of length 2 with the range of volunteers involved by square meter of edible gardens.
#' @param edible The categories in 'land_uses' that represent community edible gardens. If NULL, land_uses
#' from 'city_land_uses' dataset area used where volunteers is TRUE.
#' @param area_col The column to be used as the area of each feature. If NULL, the area is calculated with
#' sf::st_area().
#' @param interval A numeric value with the confidence interval returned by the function.
#' @param verbose If TRUE, the indicators returns a vector (N=1000) with all simulated values.
#' @return If verbose is FALSE, it returns a named vector with the median and the low and high confidence intervals.
#' Otherwise, it returns a vector of length 1000 with all simulated values.
#' @examples
#' # Get the 95% confidence interval
#' edible_volunteers(city_example, interval = 0.95)
#'
#' # Get the raw values from the Monte Carlo simulation
#' # and adjust the number of volunteers by squared meter.
#' result <- edible_volunteers(city_example, volunteers = c(0.1, 0.2), verbose = TRUE)
#' result[1:10]
#' @export


edible_volunteers <- function(x,
                        volunteers = c(0.00163, 0.22),
                        edible = NULL,
                        area_col = 'edible_area',
                        interval = 0.95,
                        verbose = FALSE){

  #to avoid notes in R CMD check
  city_land_uses <- ediblecity::city_land_uses
  land_use <- NULL

  #get categories
  if (is.null(edible)){
    edible <- city_land_uses$land_uses[city_land_uses$volunteers]
  }

  #filter x based on edible
  filtered <- x %>%
    dplyr::filter(land_use %in% edible)

  #calculates area of filtered based on st_area() or in area_col
  area <- ifelse(is.null(area_col),
                 sum(sf::st_area(filtered)),
                 sf::st_drop_geometry(filtered) %>%
                   dplyr::select(matches(area_col)) %>%
                   dplyr::summarise(sum(!!sym(area_col)))) |>
    unlist()

  #use the jobs range to create a random uniform distribution
  dist <- area * runif(1000, min = volunteers[1], max = volunteers[2])

  if(verbose) return(dist)

  #return median and confidence interval of dist
  return(c(quantile(dist,1-interval), "50%" = median(dist), quantile(dist,interval)))
}
