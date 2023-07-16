test_that("No larger areas than min_area present", {
  city_example %>%
    filter(area < 5000) %>%
    green_distance(verbose = TRUE) %>%
    expect_warning()
})

test_that("No residence", {
  city_example %>%
    filter(land_use_verbose != "Residence") %>%
    green_distance() %>%
    expect_error()
})

test_that("percent_out true returns number between 0 and 100", {
  bau <- green_distance(city_example, percent_out = TRUE)
  scenario <- set_scenario(city_example, quiet = TRUE) %>%
    green_distance(percent_out = TRUE)

  expect_length(bau, 1)
  expect_length(scenario, 1)
  expect_true(all(bau >= 0, bau <= 100, scenario >= 0, scenario <= 100))
})


test_that("verbose true returns as value as residences", {
  n_residences <- length(with(city_example, land_use_verbose[land_use_verbose == "Residence"]))
  green_distance(city_example, verbose = TRUE) %>%
    expect_length(n_residences)
})

test_that("max_dist works properly", {
  distances <- green_distance(city_example, verbose = TRUE)
  max_dist <- as.numeric(median(distances))
  green_distance(city_example, max_dist = max_dist, percent_out = TRUE) %>%
    expect_equal(50, tolerance = 1)
})
