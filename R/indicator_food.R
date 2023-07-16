#' @title The food produced by urban agriculture in your city
#' @description This indicator estimates the food (in kg/year) produced by urban agriculture
#' initiatives in your city. It uses a range of production for each type of initiative to create the median and the
#' confidence interval of the number of jobs by simulating a random uniform distribution of 1000 values
#' within the provided range.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'land_use' column with categories of
#' urban features.
#' @param edible_df A dataframe of categories that are considered as urban agriculture with three columns:
#' \enumerate{
#'   \item 'land_uses': Column with the land_use to be considered in the calculations corresponding
#'   to 'land_use' column in 'x'.
#'   \item 'food1': The low range of food production in each land_use (in kg/year/m2).
#'   \item 'food2': The high range of food production of each land_use (in kg/year/m2).
#' }
#' @param area_col The column to be used as the area of each feature. If NULL, the area is calculated with
#' sf::st_area()
#' @param interval A numeric value with the confidence interval returned by the land_use.
#' @param verbose If TRUE, the indicators returns a vector (N=1000) with all simulated values.
#' @return If verbose is FALSE, it returns a named vector with the median and the low and high confidence intervals
#' (in kg/year). Otherwise, it returns a vector of length 1000 with all simulated values (in kg/year)
#' @examples
#' # Estimate the food production within 95% confidence interval
#' food_production(city_example, interval = 0.95, verbose = FALSE)
#'
#' # Get the raw values instead of the confidence interval
#' result <- food_production(city_example, verbose = TRUE)
#' result[1:10]
#' @export


food_production <- function(x,
                            edible_df = NULL,
                            area_col = 'edible_area',
                            interval = 0.95,
                            verbose = FALSE){

  #to avoid notes on R CMD check
  city_land_uses <- ediblecity::city_land_uses
  edible <- NULL
  land_uses <- NULL
  food1 <- NULL
  food2 <- NULL
  land_use <- NULL
  . <- NULL

  check_sf(x)

  #get categories
  if(is.null(edible_df)){
    edible_df <- city_land_uses %>%
      dplyr::filter(edible == TRUE) %>%
      dplyr::select(land_uses, food1, food2)
  } else {
    if (!all(c("land_uses", "food1", "food2") %in% colnames(edible_df)))
      rlang::abort("edible_df must have the columns 'land_uses', 'food1' and 'food2'. See `?food_production` for details.")
  }

  #filter x based on edible
  filtered <- x %>%
    dplyr::filter(land_use %in% edible_df$land_uses)

  if(is.null(area_col)){
    filtered$new_area <- as.numeric(sf::st_area(filtered))
  } else {
    filtered$new_area <- as.data.frame(filtered) %>%  .[area_col]
  }

  production <- 0

  #calculates food production area for each land_use
  for (i in 1:nrow(edible_df)){
     f <- filtered %>%
       dplyr::filter(land_use == edible_df$land_uses[i])

     if(nrow(f) > 0){
       production <- production + sum(f$new_area) * runif(1000, edible_df$food1[i], edible_df$food2[i])
     }
  }

  if(verbose) return(production)

  #return median and confidence interval of dist
  return(c(quantile(production,1-interval), "50%" = median(production), quantile(production,interval)))
}






