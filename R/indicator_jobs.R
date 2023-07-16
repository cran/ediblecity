#' @title The jobs created by urban agriculture in your city
#' @description This indicator estimates the number of full-time jobs created by commercial urban agriculture
#' initiatives in your city. It uses a range of jobs per square meter to create the median and the
#' confidence interval of the number of jobs by simulating a random uniform distribution of 1000 values
#' within the provided range.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'land_use' column with categories of urban features.
#' @param jobs A vector of length 2 with the range of jobs created by square meter of edible gardens.
#' @param edible The categories in 'land_uses' that represent commercial edible gardens. If NULL, the
#' land_uses from 'city_land_uses' dataset are used where jobs is TRUE.
#' @param area_col The column to be used as the area of each feature. If NULL, the area is calculated with
#' sf::st_area()
#' @param interval A numeric value with the confidence interval returned by the function.
#' @param verbose If TRUE, the indicators returns a vector (N=1000) with all simulated values.
#' @return If verbose is FALSE, it returns a named vector with the median and the low and high confidence intervals.
#' Otherwise, it returns a vector of length 1000 with all simulated values.
#' @examples
#' # First, we set a scenario with commercial gardens that create jobs
#' scenario <- set_scenario(city_example, pCommercial = 1, quiet = TRUE)
#' # Get the 95% confidence interval
#' edible_jobs(scenario, interval = 0.95)
#'
#' # Get the raw values from the Monte Carlo simulation and adjust the number of jobs by square meter.
#' result <- edible_jobs(scenario, jobs = c(0.02, 0.03), verbose = TRUE)
#' result[1:10]
#' @export


edible_jobs <- function(x,
                        jobs = c(0.000163, 0.022),
                        edible = NULL,
                        area_col = 'edible_area',
                        interval = 0.95,
                        verbose = FALSE){

    #top avoid notes on R CMD Check
    city_land_uses <- ediblecity::city_land_uses
    land_use <- NULL
    . <- NULL


    #get categories
    if (is.null(edible)){
      edible <- city_land_uses$land_uses[city_land_uses$jobs]
    }

    #filter x based on edible
    filtered <- x %>%
      dplyr::filter(land_use %in% edible)

    #calculates area of filtered based on st_area() or in area_col
    area <- ifelse(is.null(area_col),
                    sum(sf::st_area(filtered)),
                    as.data.frame(filtered) %>%
                      dplyr::select(matches(area_col)) %>%
                      sum(.[area_col]))

    #use the jobs range to create a random uniform distribution
    dist <- area * runif(1000, min = jobs[1], max = jobs[2])

    if(verbose) return(dist)

    #return median and confidence interval of dist
    return(c(quantile(dist,1-interval), "50%" = median(dist), quantile(dist,interval)))
}




