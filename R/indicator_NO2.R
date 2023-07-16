#' @title Sequestration of nitrogen dioxide
#' @description This indicator returns the amount of NO2 that is sequestered by urban green.
#' @author Josep Pueyo-Ros
#' @param x An 'sf' object with the urban model of your city and a 'land_use' column with categories of
#' urban features.
#' @param green_df A dataframe of categories that are considered as urban green with four columns:
#' \enumerate{
#'   \item 'land_uses': Column with the function to be considered in the calculations corresponding
#'   to 'land_use' column in 'x'.
#'   \item 'no2_seq1': The low range of NO2 sequestration of each function (in ug/s/m2).
#'   \item 'no2_seq2': The high range of NO2 sequestration of each function (in ug/s/m2).
#'   \item 'pGreen': The proportion of green surface in each function (0:1). This is overriden by 'edible_are' when
#'   land_uses are community garden, commercial garden, rooftop garden and hydroponic rooftop.
#' }
#' If NULL, the 'city_land_uses' dataset is used.
#' @return A numeric value with the total NO2 sequestration in the city (in grams/second).
#' @examples
#' # Get the total nitrogen dioxide sequestered by urban green
#' no2_seq(city_example)
#' @export
#'


no2_seq <- function(x,
                    green_df = NULL
                    ){

  #to avoid notes in R CMD check
  city_land_uses <- ediblecity::city_land_uses
  pGreen <- NULL
  land_uses <- NULL
  no2_seq1 <- NULL
  no2_seq2 <- NULL
  land_use <- NULL

  check_sf(x)

  if(is.null(green_df)){
    green_df <- city_land_uses %>%
      mutate(pGreen = ifelse(!is.na(pGreen),
                             pGreen,
                             ifelse(location == "rooftop",
                                    0.61,
                                    1))) %>%
      select(land_uses, no2_seq1, no2_seq2, pGreen)
  }

  if(!(all(c("land_uses", "no2_seq1", "no2_seq2") %in% colnames(green_df))))
    rlang::abort("green_df must contain the columns 'land_uses', 'no2_seq1' & 'no2_seq2'. See ?no2_seq for details")

  x_f <- x %>% filter(land_use %in% green_df$land_uses)


  x_f <- left_join(x_f,green_df, by=c("land_use" = "land_uses"))
  x_f$pGreen[is.na(x_f$pGreen)] <- 0

  x_f$green_area <- x_f$edible_area
  green_area_na <- is.na(x_f$green_area)

  x_f$green_area[green_area_na] <- as.numeric(sf::st_area(x_f[green_area_na,])) * x_f$pGreen[green_area_na]

  edible_gardens <- x_f$land_use == "Edible private garden"
  x_f$green_area[edible_gardens] <- as.numeric(sf::st_area(x_f[edible_gardens,])) * x_f$pGreen[edible_gardens]

  x_f$no2_seq <- 0

  for (i in 1:nrow(green_df)){
    f <- green_df$land_uses[i]

    if (green_df$no2_seq1[i] != green_df$no2_seq2[i]){
      x_f$no2_seq[x_f$land_use == f] <-  x_f$green_area[x_f$land_use == f] * runif(
                                                                        length(x_f$green_area[x_f$land_use == f]),
                                                                        green_df$no2_seq1[i],
                                                                        green_df$no2_seq2[i])

      } else {
      x_f$no2_seq[x_f$land_use == f] <-  x_f$green_area[x_f$land_use == f] * green_df$no2_seq1[i]
    }
  }


 return(c("gr/s" = sum(x_f$no2_seq)/1000))



}
