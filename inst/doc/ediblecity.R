## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = TRUE
)

## ----setup--------------------------------------------------------------------
library(ediblecity)

## ----set_scenarios------------------------------------------------------------

scenario <- set_scenario(city_example,
                           pGardens = 0.5,
                           pVacant = 0.5,
                           pRooftop = 0.75,
                           private_gardens_from = "Normal garden",
                           vacant_from = c("Vacant", "Streets"),
                           rooftop_from = "Rooftop",
                           pCommercial = 0.5)


## -----------------------------------------------------------------------------
UHI(scenario, SVF)


## -----------------------------------------------------------------------------
plot(UHI(scenario, SVF, return_raster = TRUE))


## -----------------------------------------------------------------------------
runoff_prev(scenario)

## -----------------------------------------------------------------------------

green_distance(scenario)


## -----------------------------------------------------------------------------
green_distance(scenario, percent_out = TRUE)

## -----------------------------------------------------------------------------
green_capita(scenario, inhabitants = 6000)

green_capita(scenario, 
             neighbourhoods = neighbourhoods_example, 
             inh_col = 'inhabitants',
             name_col = 'name',
             verbose = TRUE)

green_capita(scenario, 
             neighbourhoods = neighbourhoods_example, 
             inh_col = 'inhabitants',
             name_col = 'name')


## -----------------------------------------------------------------------------

no2_seq(scenario)


## -----------------------------------------------------------------------------
edible_jobs(scenario)

## -----------------------------------------------------------------------------
edible_volunteers(scenario)

## -----------------------------------------------------------------------------
food_production(scenario)

