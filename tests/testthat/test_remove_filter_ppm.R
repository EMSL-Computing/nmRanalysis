library(nmRanalysis)

data("e_data")
data("f_data")

ppmData <- as.ppmData(e_data = e_data,
                      f_data = f_data,
                      edata_cname = "PPM", fdata_cname = "Sample",
                      instrument_strength = 600, ph = 7.33, solvent = "h2o")

filtered_data <- filter_ppm(ppmData, range = list(min = 4, max = 6))

test_that("errors thrown when incorrect parameters are supplied", {
  expect_error(remove_filter_ppm(filtered_data$e_data, filters = 1),
               "'ppmData' must be 'ppmData' object created using the function 'as.ppmData'")
  expect_error(remove_filter_ppm(ppmData = ppmData, filters = 1),
               "No PPM filter has been applied to ppmData object")
  expect_error(remove_filter_ppm(filtered_data, filters = c(1,2)),
               "Length of 'filters' parameter greater than the number of filters applied to 'ppmData'")
})

test_that("filters removed correctly", {
  # remove a single filter
  unfilt_data <- remove_filter_ppm(filtered_data, filters = 1)
  expect_equal(length(attr(unfilt_data, "filters")), 0)

  # remove multiple filters
  refilt_data <- filter_ppm(filtered_data, range = list(min = 2, max = 2.5))
  unrefilt_data <- remove_filter_ppm(refilt_data, filters = 2)
  unfilt_data2 <- remove_filter_ppm(refilt_data, filters = c(1,2))

  expect_equal(length(attr(unrefilt_data, "filters")), 1)
  expect_true(attr(unrefilt_data, "filters")[[1]]$range$min == 4)
  expect_equal(length(attr(unfilt_data2, "filters")), 0)
  })
