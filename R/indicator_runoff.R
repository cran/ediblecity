#' @title Runoff prevention
#' @description The indicator calculates the runoff prevention considering a rain event, the infiltration
#' capacity and the rain harvesting and storage capacity.
#' government.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'land_use' column with categories
#' of urban features.
#' @param runoff_df A dataframe of categories that are considered impervious area with three columns.
#' \enumerate{
#'     \item 'land_uses' with the names of 'land_use' in 'x' to be considered as impervious.
#'     \item Curve numbers columns 'CN1' and 'CN2' the range of curve number of that function.
#'     \item 'water_storage', a boolean column indicating whether the land_uses is potentially harvesting
#'     and storing rainwater using a tank.
#' }
#' If NULL, categories and values of 'city_land_uses' are considered.
#' @param rain The amount of 24h-rain to be simulated, default is 85 mm.
#' @param floors_field The column in 'x' containing the number of floors of each building. Zero is considered
#' unbuilt areas like gardens or streets. It is used to calculate rainwater harvesting area, since only
#' upper surface are considered. Missing values are considered as zero.
#' @param harvest_dist Maximum distance (in meters) of buildings where to harvest rainwater
#' @param tank_size A two-length vector with the range of tank size possibilities, proportional
#' to the surface of each element where the tank size is located (in l/m2).
#' @return It returns a named vector with values of runoff in mm, total rainfall
#' and harvested rainwater in cubic meters.
#' @examples
#' # Get the total values of runoff, rainfall and rain harvested
#' runoff_prev(city_example)
#'
#' # Adjust the parameters for rain, maximum distance to harvest rainwater and tank size
#' runoff_prev(city_example, rain = 160, harvest_dist = 5, tank_size = c(20,30))
#' @export

#CN https://www.hec.usace.army.mil/confluence/hmsdocs/hmstrm/cn-tables

runoff_prev <- function(
                        x,
                        runoff_df = NULL,
                        rain = 85,
                        floors_field = 'floors',
                        harvest_dist = 10,
                        tank_size = c(0,45)
                        ){

  #to avoid notes in R CMD check
  city_land_uses <- ediblecity::city_land_uses
  land_uses <- NULL
  water_storage <- NULL
  floors_ <- NULL
  CN1 <- NULL
  CN2 <- NULL
  land_use <- NULL
  area <- NULL
  id.x <- NULL
  id.y <- NULL
  floors_.x <- NULL
  area.x <- NULL
  . <- NULL

  check_sf(x)

  #if no runoff_df, use the city_land_uses values
  if(is.null(runoff_df)){
    runoff_df <- city_land_uses %>%
      dplyr::select(
        land_uses,
        water_storage,
        contains("CN")
      )
  }

  #create new field and equal NA floors to zero
  x <- x %>%
    dplyr::mutate(floors_ = !!as.symbol(floors_field),
                 floors_ = ifelse(is.na(floors_), 0, floors_))


  #calculate area
  if (is.null(x$area))
    x$area <- as.numeric(sf::st_area(x))

  #rainfall volume in the city
  rainfall <- rain * sum(x$area)

  #join runoff_df with x

  x <- x %>%
    dplyr::left_join(runoff_df, by = c("land_use" = "land_uses")) %>%
    dplyr::mutate(
      CN1 = ifelse(is.na(CN1), 98, CN1),
      CN2 = ifelse(is.na(CN2), 98, CN2)
    )

  areaCN <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      CN = runif(1, CN1, CN2),
      areaCN = case_when(
        land_use == "Rooftop garden" ~ CN * area * 0.60,
        TRUE ~ CN * area)
        ) %>%
    dplyr::pull(areaCN)

  weightedCN <- sum(areaCN)/sum(x$area)

  ##calculate rainwater harvesting area and tank size


  #create unique id
  x$id <- rownames(x)

  harvest_land_uses <- runoff_df$land_uses[runoff_df$water_storage]

  #buffer of land_uses with storage using harvest_dist
  buffer_harv <- x %>%
    dplyr::filter(land_use %in% harvest_land_uses) %>%
    dplyr::select(id, floors_, area) %>%
    sf::st_buffer(harvest_dist)

  #join rooftops as harvesting surfaces with buffer_harv
  rooftops <- x %>%
    dplyr::filter(floors_ > 0,
                  !(land_use %in% harvest_land_uses))

  rooftops <- suppressWarnings(sf::st_join(rooftops, buffer_harv, largest = FALSE)) %>%
    dplyr::select(id.x, id.y, floors_.x, area.x) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(!is.na(id.y))

  buffer_harv <- sf::st_drop_geometry(buffer_harv)

  storage_func <- function(u){
    collect <- rainfall * sum(rooftops$area.x[rooftops$id.y == u[[1]] & rooftops$floors_.x > u[[2]]], na.rm = TRUE)
    tank <-  as.numeric(u[[3]]) * runif(1, tank_size[1], tank_size[2])
    return(min(collect,tank))
  }

  #total rain harvested in litres
  total_RH <- sum(apply(buffer_harv, 1, "storage_func"))

  #rain harvested in mm
  RH <- total_RH/sum(x$area)

  #SCS model for runoff
  S <- 25.4*((1000/weightedCN)-10) #mm
  Ia <- (0.2 * S) + RH #mm
  Q <- ((rain-Ia)^2)/(rain-Ia+S) #mm


  return(
    c(
      runoff = Q, #mm
      rainfall = rainfall/1000, #cubic metres
      rainharvest = total_RH/1000 #cubic metres
      )
    )

}

