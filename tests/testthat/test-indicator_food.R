
test_that("verbose false returns interval", {
  result <- food_production(city_example, verbose = FALSE)
  expect_equal(length(result), 3)
})

test_that("verbose true returns 1000 values", {
  result <- food_production(city_example, verbose = TRUE)
  expect_equal(length(result), 1000)
})

bau <- food_production(city_example)

test_that("private gardens produce food", {
  result <- set_scenario(city_example, pGardens = 1, pVacant = 0, pRooftop = 0, quiet = TRUE) %>%
    food_production()
  expect_gt(result[[2]], bau[[2]])
})

test_that("community gardens produce food", {
  result <- set_scenario(city_example, pGardens = 0, pVacant = 1, pRooftop = 0, quiet = TRUE) %>%
    food_production()
  expect_gt(result[[2]], 0)
})

test_that("rooftop gardens produce food", {
  result <- set_scenario(city_example, pGardens = 0, pVacant = 0, pRooftop = 1, quiet = TRUE) %>%
    food_production()
  expect_gt(result[[2]], bau[[2]])
})

test_that("commercial gardens produce food", {
  result <- set_scenario(city_example, pGardens = 0, pVacant = 1, pRooftop = 0,
                                          pCommercial = 1, quiet = TRUE) %>%
    food_production()
  expect_gt(result[[2]], bau[[2]])
})

test_that("hydroponic rooftop produce food", {
  result <- set_scenario(city_example, pGardens = 0, pVacant = 0, pRooftop = 1,
                                          pCommercial = 1, quiet = TRUE) %>%
    food_production()
  expect_gt(result[[2]], bau[[2]])
})

