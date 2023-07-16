test_that("when return_raster an verbose are False, it returns a summary", {
  UHI(city_example, SVF) %>%
    expect_s3_class("summaryDefault")
})

test_that("when return_raster is TRUE it returns a stars object", {
  UHI(city_example, SVF, return_raster = TRUE, verbose = TRUE) %>%
    expect_s3_class("stars")
  UHI(city_example, SVF, return_raster = TRUE) %>%
    st_dimensions() %>%
    expect_equal(st_dimensions(SVF))
})

test_that("when return_raster is False and verbose is True returns vector", {
  UHI(city_example, SVF, verbose = TRUE) %>%
    expect_type("double")
})

