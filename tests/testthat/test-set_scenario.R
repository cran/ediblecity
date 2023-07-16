test_that("Previous edible areas are respected", {
  test <- set_scenario(city_example,
                       pGardens = 0.2,
                       pVacant = 0,
                       pRooftop = 0.3,
                       quiet = TRUE)

  after_area <- test$edible_area[test$land_use == "Community garden"]
  before_area <- city_example$edible_area[city_example$land_use == "Community garden"]
  expect_equal(after_area, before_area)
})

test_that("Raise error if x is not an sf object", {
  expect_error(
    city_land_uses %>%
      set_scenario()
  )
})

test_that("Raise error if land_use col does not exist", {
  expect_error(
    city_example %>%
      select(-land_use) %>%
      set_scenario()
  )
})

test_that("Commercial are larger than community gardens", {
  scenario <- set_scenario(city_example, pGardens = 0, pVacant = 0.5, pRooftop = 0.5,
                           vacant_from = c("Vacant", "Streets"), pCommercial = 0.5, quiet = TRUE)
  community_surface <- scenario %>%
    filter(land_use %in% city_land_uses$land_uses[city_land_uses$volunteers]) %>%
    summarise(area = sum(area)) %>% pull(area)

  commercial_surface <- scenario %>%
    filter(land_use %in% city_land_uses$land_uses[city_land_uses$jobs]) %>%
    summarise(area = sum(area)) %>% pull(area)

  expect_gt(commercial_surface, community_surface)
})
