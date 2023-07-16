default <- runoff_prev(city_example)

test_that("rain is considered", {
  result <- runoff_prev(city_example, rain = 200)
  expect_lt(default[['runoff']], result[["runoff"]])
  expect_lt(default[['rainfall']], result[["rainfall"]])
})

test_that("harvest distance is working properly", {
  default_fixed_tank <- runoff_prev(city_example, tank_size = c(40,40))
  result_fixed_tank <- runoff_prev(city_example, tank_size = c(40,40), harvest_dist = 1)
  expect_gt(default_fixed_tank[["rainharvest"]],
            result_fixed_tank[["rainharvest"]])
})

test_that("tank size is working properly", {
  result <- runoff_prev(city_example, tank_size = c(45, 60))
  expect_gt(result[["rainharvest"]], default[["rainharvest"]])
})

test_that("curve number is working properly", {
  result <- runoff_prev(city_example, runoff_df = tibble(
    land_uses = city_land_uses$land_uses,
    CN1 = city_land_uses$CN2,
    CN2 = city_land_uses$CN2 * 1.5,
    water_storage = city_land_uses$water_storage
  ))
  expect_gt(result[["runoff"]], default[["runoff"]])
})
