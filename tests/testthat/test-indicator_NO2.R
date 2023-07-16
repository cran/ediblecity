test_that("indicator returns a named numeric scalar", {
  result <- no2_seq(city_example)
  expect_length(result, 1)
  expect_type(result, "double")
  expect_type(names(result), "character")
})

test_that("city_functions is not used when green_df is not NULL", {
  no2_seq(city_example, green_df = tibble(a = 1, b=2)) %>%
    expect_error()
})

test_that("sequestration increases if edible area increases", {
  scenario_1 <- set_scenario(city_example, pGarden = 0, pRooftop = 0,
                            vacant_from = "Streets", edible_area_vacant = c(0.1, 0.1), quiet = TRUE)

  scenario_2 <- set_scenario(city_example, pGarden = 0, pRooftop = 0,
                            vacant_from = "Streets", edible_area_vacant = c(0.8, 0.8), quiet = TRUE)
  expect_gt(no2_seq(scenario_2), no2_seq(scenario_1))
})


