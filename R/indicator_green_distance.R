#' @title Distance to closest public green area
#' @description This indicator calculates the distance from each residence to its closest public green area
#' larger than 'min_area'. It can return the summary of distances or the percentage of residence buildings further
#' than a defined distance.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'land_use' column with categories of urban features.
#' @param green_cat A vector with the categories in 'land_use' that must be considered in the calculations. If
#' NULL (default), the 'city_land_uses' dataset is used where 'public' is TRUE.
#' @param min_area A numerical value (in meters). smaller green areas are not considered in the calculations.
#' @param residence_col The column 'x' where the residences are specified.
#' @param residences A vector with the categories that represent residences.
#' @param percent_out If TRUE, the function returns the percentage of residences further than 'max_dist'.
#' (default = FALSE)
#' @param max_dist A numeric value representing the maximum distance that a residence should be from a
#' public green area.
#' @param verbose If TRUE returns the vector of distances. Otherwise, it returns as specified in value section.
#' @return If 'percent_out' is FALSE, it returns a summary of statistics for distance. Otherwise, it
#' returns a numeric value with the percentage of residences further than 'max_dist' from its closest
#' public green area.
#' @examples
#' # Calculate a summary of the distances to closest public green area larger than 0.5 ha.
#' green_distance(city_example, min_area = 5000)
#'
#' # Get the distances from each residence to its closest public green area.
#' result <- green_distance(city_example, min_area = 0, verbose = TRUE)
#' result[1:10]
#'
#' # Get the percentage of residences further than 300 m. from a green area larger than 0.5 ha.
#' green_distance(city_example, percent_out = TRUE, max_dist = 300)
#' @export

green_distance <- function(x,
                           green_cat = NULL,
                           min_area = 5000,
                           residence_col = 'land_use_verbose',
                           residences = 'Residence',
                           percent_out = FALSE,
                           max_dist = 300,
                           verbose = FALSE
                          ){

  #to avoid notes on R CMD check
  city_land_uses <- ediblecity::city_land_uses
  land_use <- NULL
  area <- NULL

  check_sf(x)

  #get categories
  if (is.null(green_cat)){
    green_cat <- city_land_uses$land_uses[city_land_uses$public]
  }

  # get green areas layer
  green_areas <- x %>%
    dplyr::filter(land_use %in% green_cat)

  if(is.null(green_areas$area))
      green_areas$area <- as.numeric(sf::st_area(green_areas))

  green_areas <- green_areas %>%
    dplyr::filter(area >= min_area)

  if(nrow(green_areas) == 0) rlang::warn("No public green areas larger than 'min_area' in 'x'. Returning 'NAs'")

  houses <- x %>%
    dplyr::filter(!!as.symbol(residence_col) %in% residences)

  if(nrow(houses) == 0) rlang::abort("No residences found in 'x'")

  nearest <- sf::st_nearest_feature(houses,green_areas)
  distance <- sf::st_distance(houses, green_areas[nearest,], by_element = TRUE)

  if (verbose) return(distance)

  if (percent_out){
    return(sum(as.numeric(distance) > max_dist) / length(distance) * 100)
  } else {
    return(summary(distance))
  }

}
