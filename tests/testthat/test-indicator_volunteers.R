test_that("edible_area works properly", {
  edible_volunteers(city_example, area_col = "edible_area")[[2]] %>%
    expect_lt(edible_volunteers(city_example, area_col = "area")[[2]])
})

test_that("randomization is working", {
  result <- edible_volunteers(city_example)
  expect_lt(result[[1]], result[[2]])
  expect_lt(result[[2]], result[[3]])
})

test_that("verbose TRUE returns a vector of length 1000", {
  edible_volunteers(city_example, verbose = TRUE) %>%
    expect_length(1000)
})
