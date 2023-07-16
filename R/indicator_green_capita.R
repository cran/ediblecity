#' @title Urban green per capita
#' @description This indicators calculates the amount of green per capita in the city. This may include private green
#' such as gardens and crops or exclude them.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'land_use' column with categories of urban features.
#' @param green_categories The categories that are considered as urban green. If NULL, categories of 'city_land_uses'
#' are considered.
#' @param inhabitants A value representing the inhabitants in the city.
#' @param neighbourhoods (optional) An 'sf' object with polygons representing the neighborhoods in the city.
#' @param inh_col (optional) The col in 'x' or in 'neighborhoods' indicating the inhabitants in each neighborhood.
#' @param name_col (optional) The col in 'x' or in 'neighborhoods' indicating the name of each neighborhood
#' @param private If FALSE (default), only public areas are considered in the indicator. If TRUE, elements in
#' 'city_land_uses' where 'private' is TRUE are considered. Alternatively, a vector with land_uses to be
#' considered.#'
#' @param verbose If FALSE (default), the indicator returns the proportion between the most and the least green neighbourhoods.
#' Otherwise, it will return a tibble with the green per capita in each neighborhood, provided that 'inh_col'
#' and 'name_col' are provided.
#' @param min_inh If neighbourhoods are used, those with less inhabitants than 'min-inh' will be discarded.
#' @details If 'inh_col' and 'name_col' are defined and  'neighbourhoods' is NULL, the function searches
#' the columns in 'x'. If 'neighbourhoods' is defined along with previous both, the columns are searched in
#' 'neighbourhoods' and spatially joined with 'x'. In both cases, 'inhabitants' is ignored.
#' @return A numeric value expressing the square meters of green per capita. Or a numeric value expressing
#' the proportion between the greenest and the least green neighbourhood. Or a tibble with the green area,
#' inhabitants and green per capita in each neighbourhood.
#' @examples
#' # Calculate total green per capita in the city
#' green_capita(city_example, inhabitants = 6000)
#'
#' # Calculate the differences between the greenest and the least green neighbourhoods
#' green_capita(city_example, neighbourhoods = neighbourhoods_example,
#'              inh_col = "inhabitants", name_col = "name")
#'
#' # Get the green per capita in each neighbourhood
#' green_capita(city_example, neighbourhoods = neighbourhoods_example,
#'              inh_col = "inhabitants", name_col = "name", verbose = TRUE)
#'
#' # Use a customized vector of land_uses to be considered private green
#' green_capita(city_example, inhabitants = 6000, private = c("Normal garden", "Commercial garden"))
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @importFrom stats runif
#' @importFrom stats quantile
#' @import dplyr
#' @import sf
#' @export
#'


green_capita <- function(
                        x,
                        green_categories = NULL,
                        inhabitants = NULL,
                        neighbourhoods = NULL,
                        inh_col = NULL,
                        name_col = NULL,
                        private = FALSE,
                        verbose = FALSE,
                        min_inh = 0
                        ){

  #to avoid notes on R CMD check
  city_land_uses <- ediblecity::city_land_uses
  land_use <- NULL
  pGreen <- NULL

  check_sf(x)

  if(all(is.null(inh_col), is.null(inhabitants))) rlang::abort("'inhabitants' or 'inh_col' must be provided.")
  if(!is.null(inh_col) && is.null(name_col)) rlang::abort("'name_col' must be provided along with 'inh_col'")

  if (is.null(green_categories)){

    green_categories <- city_land_uses$land_uses[city_land_uses$public]

    if (isTRUE(private)){
      green_categories <- c(green_categories, city_land_uses$land_uses[c(1,7)])
    } else if(is.character(private)) {
      green_categories <- c(green_categories, private)
    }
  }

  green_areas <- x %>%
    dplyr::filter(land_use %in% green_categories)

  if(is.null(green_areas$area))
    green_areas$area <- as.numeric(sf::st_area(green_areas))

  if (sum(green_areas$land_use %in% city_land_uses$land_uses) == nrow(green_areas)){
    green_areas <- dplyr::left_join(green_areas, city_land_uses, by = c("land_use" = "land_uses"))
  } else {
    green_areas$pGreen <- NA
    green_areas$location <- NA
  }



  if(!is.null(inh_col) && !is.null(name_col)){

    if(!is.null(neighbourhoods)){

      check_sf(neighbourhoods)

      if (sf::st_crs(green_areas) != sf::st_crs(neighbourhoods)){
        neighbourhoods <- sf::st_transform(neighbourhoods, sf::st_crs(green_areas))
      }

      green_areas <- suppressWarnings(sf::st_intersection(green_areas, neighbourhoods))

    }

     green_areas <- green_areas %>%
      dplyr::filter(!is.na(!!as.symbol(name_col)),
                    !!as.symbol(inh_col) > min_inh) %>%
      dplyr::mutate(area_ = ifelse(!is.na(pGreen),
                              area * pGreen,
                              ifelse(location == "rooftop",
                                  area * 0.61,
                                  area))) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(land_use,!!as.symbol(name_col), !!as.symbol(inh_col), area) %>%
      dplyr::group_by(!!as.symbol(name_col)) %>%
      dplyr::summarise(area = sum(as.numeric(area))) %>%
      dplyr::inner_join(neighbourhoods, by=name_col) %>%
      dplyr::select(!!as.symbol(name_col), area, !!as.symbol(inh_col)) %>%
      dplyr::mutate(green_capita = area/!!as.symbol(inh_col))

    if(verbose){
      result <- green_areas

    } else {

      result <- min(green_areas$green_capita) / max(green_areas$green_capita)
    }

  } else {

    area <-  green_areas %>%
      dplyr::mutate(area = ifelse(!is.na(pGreen),
                          (area * pGreen),
                          (ifelse(location == "rooftop",
                                 area * 0.61,
                                 area)))) %>%
      pull(area)

    result <- sum(as.numeric(area))/inhabitants

  }

  return(result)
}
