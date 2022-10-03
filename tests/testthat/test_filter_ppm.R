library(nmRanalysis)

data("e_data")
data("f_data")

ppmData <- as.ppmData(e_data = e_data,
                      f_data = f_data,
                      edata_cname = "PPM", fdata_cname = "Sample",
                      instrument_strength = 600, temperature = 298, solvent = "h2o")

test_that("error thrown when incorrect data is supplied", {
  expect_error(filter_ppm(ppmData = ppmData$e_data, range = list(min = 2, max = 5)),
               "'ppmData' must be object created using the function 'as.ppmData'")
})

test_that("filter_ppm breaks when an incorrect range is supplied",{
  expect_error(filter_ppm(ppmData = ppmData, range = c(2,3)),
               "Range must be a list containing the minimum and maximum value in the range to be filtered")
  expect_error(filter_ppm(ppmData = ppmData, range = 5),
               "Range must be a list containing the minimum and maximum value in the range to be filtered")
  expect_error(filter_ppm(ppmData = ppmData, range = list(max = c(2, 4))))
  expect_error(filter_ppm(ppmData = ppmData, range = list(min = c(2, 4))))
  expect_error(filter_ppm(ppmData = ppmData, range = list(min = c(2, 4), max = 5)),
               "Number of range minima must equal number of range maxima")
})


filtered_data <- filter_ppm(ppmData = ppmData,
                            range = list(min = 4.6, max = 5.0))

test_that("filter_ppm removes ranges as expected",{
  expect_true(nrow(ppmData$e_data) == nrow(filtered_data$e_data) + nrow(attr(filtered_data, "filters")[[1]]$filtered_data))
  expect_equal(filtered_data$e_data %>% dplyr::filter(PPM >= 4.6 & PPM <= 5.0) %>% nrow(), 0)
})

test_that("subsequent filters correctly added to attributes", {
  refiltered_data <- filter_ppm(ppmData = filtered_data,
                                range = list(min = 6, max = 6.5))
  expect_equal(length(attr(refiltered_data, "filters")), 2)
  expect_true(attr(refiltered_data, "filters")[[2]]$range$max == 6.5)
})

