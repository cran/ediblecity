check_sf <- function(x){
  #check if x is a sf object
  if (all(class(x) != "sf")) rlang::abort("x must be an `sf` object")
}
