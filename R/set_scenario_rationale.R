set_scenario_rationale <- function(x,
                         pGardens = 1,
                         pVacant = 1,
                         pRooftop = 1,
                         perc_garden = c(0.02, 0.3),
                         perc_vacant = c(0.52, 0.75),
                         perc_rooftop = c(0.6,0.62),
                         min_area_garden = 10,
                         min_area_vacant = 100,
                         min_area_rooftop = 100,
                         private_gardens_from = "Normal garden",
                         vacant_from = "Vacant",
                         rooftop_from = "Rooftop",
                         pCommercial = 0,
                         area_field = 'flat_area'
                         ){

  #to avoid notes on R CMD check
  city_land_uses <- ediblecity::city_land_uses

  check_sf(x)

  #if area_field is null, calculates de area of each feature
  if (is.null(area_field)) {
    x$area <- as.numeric(sf::st_area(x))
  } else {
    x$area <- x[[area_field]]
  }

  #create edible area field
  x$edible_area <- 0

  #CONVERT PRIVATE GARDENS TO EDIBLE GARDENS
  if (pGardens > 0){
    gardens_index <- which(x$land_use %in% private_gardens_from & x$area >= min_area_garden)
    nGardens <- sum(x$land_use %in% private_gardens_from)

    if (pGardens == 1 || nGardens*pGardens >= length(gardens_index)){

      x$land_use[gardens_index] <- city_land_uses$land_uses[city_land_uses$location == "garden"]

      if (nGardens*pGardens >= length(gardens_index)){
        warning(paste("Only", length(gardens_index), "private gardens out of", nGardens*pGardens, "assumed satisfy the 'min_area_garden'\n"))
      }

    } else if (pGardens < 1){
      nGardens <- nGardens*pGardens
      gardens_index <- sample(gardens_index, nGardens)
      x$land_use[gardens_index] <- city_land_uses$land_uses[city_land_uses$location == "garden"]

    }

    x$edible_area[gardens_index] <- x$area[gardens_index]*runif(length(gardens_index), perc_garden[1], perc_garden[2])
  }


  #CONVERT VACANT PLOTS TO COMMUNITY OR COMMERCIAL GARDENS
  if (pVacant > 0){

    #locate and count vacant plots
    vacant_index <- which(x$land_use %in% vacant_from & x$area >= min_area_vacant)
    new_index <- vacant_index
    nVacant <- sum(x$land_use %in% vacant_from)

    #set transformed vacants, commercial and community proportions to zero
    total_vacant <- 0
    commercial <- 0

    #define categories in a vector
    commercial_garden <- city_land_uses$land_uses[city_land_uses$jobs & city_land_uses$location == "vacant"]
    community_garden <- city_land_uses$land_uses[city_land_uses$volunteers & city_land_uses$location == "vacant"]


    if (length(vacant_index) < nVacant*pVacant){
      warning(paste("Only", length(vacant_index), "vacant plots out of", nVacant*pVacant, "assumed satisfy the 'min_area_vacant'\n"))
      nVacant <- length(vacant_index)

    } else {
      nVacant <- nVacant*pVacant
    }

    while (total_vacant < nVacant){

      larger <- which(x$area == max(x$area[vacant_index]) & x$land_use %in% vacant_from)
      if (length(larger) > 1) larger <- larger[1]
      vacant_index <- vacant_index[vacant_index != larger]

      if (commercial < pCommercial){

        x$land_use[larger] <- commercial_garden
        commercial <- sum(x$land_use == commercial_garden)/nVacant

      } else {

        x$land_use[larger] <- community_garden
      }

      total_vacant <- total_vacant + 1
    }

    x$edible_area[new_index] <-
      x$area[new_index]*runif(length(new_index), perc_vacant[1], perc_vacant[2])

  }

  #CONVERT ROOFTOPS TO ROOFTOP GARDENS OR HYDROPONIC ROOFTOPS
  if (pRooftop > 0){

    #locate and count vacant plots
    rooftop_index <- which(x$land_use %in% rooftop_from & x$area >= min_area_rooftop)
    new_index <- rooftop_index
    nRooftop <- sum(x$land_use %in% rooftop_from)

    #set transformed vacants, commercial and community proportions to zero
    total_rooftop <- 0
    commercial <- 0

    #define categories
    rooftop_garden <- city_land_uses$land_uses[city_land_uses$volunteers & city_land_uses$location == 'rooftop']
    hydroponic_rooftop <- city_land_uses$land_uses[city_land_uses$jobs & city_land_uses$location == 'rooftop']

    if (length(rooftop_index) < nRooftop*pRooftop){
      warning(paste("Only", length(rooftop_index), "rooftops out of", nRooftop*pRooftop, "assumed satisfy the 'min_area_rooftop'\n"))
      nRooftop <- length(rooftop_index)

    } else {
      nRooftop <- nRooftop*pRooftop
    }

    while (total_rooftop < nRooftop){

      larger <- which(x$area == max(x$area[rooftop_index]) & x$land_use %in% rooftop_from)
      if (length(larger) > 1) larger <- larger[1]
      rooftop_index <- rooftop_index[rooftop_index != larger]

      if (commercial < pCommercial){

        x$land_use[larger] <- hydroponic_rooftop
        commercial <- sum(x$land_use == hydroponic_rooftop)/nRooftop

      } else {

        x$land_use[larger] <- rooftop_garden
      }

      total_rooftop <- total_rooftop + 1
    }

    x$edible_area[new_index] <- x$area[new_index]*runif(length(new_index), perc_rooftop[1], perc_rooftop[2])

  }


  return(x)

}
