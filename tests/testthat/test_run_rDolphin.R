library(nmRanalysis)

data("e_data")
data("f_data")
data("example_metabs")

ppmData <- as.ppmData(e_data = e_data,
                      f_data = f_data,
                      edata_cname = "PPM", fdata_cname = "Sample",
                      instrument_strength = 600, ph = 7.33, solvent = "h2o")

imported_data <- ppmData_to_rDolphin(ppmData = ppmData,
                                     metabs = example_metabs)

test_that("errors thrown when incorrect input provided", {
  expect_error(run_rDolphin(imported_data$dataset),
               "'imported_data' must be 'rDolphin' object, created using `ppmData_to_rDolphin`")
  expect_error(run_rDolphin(imported_data, dir = "this_is_not/a_directory"))
  expect_error(run_rDolphin(imported_data, optimization = 8),
               "'optimization' must be logical value")
  expect_error(run_rDolphin(imported_data, spectra_to_profile = c("one", "two")))
})

test_that("correct output dimensions",{

  profiling_data <- run_rDolphin(imported_data = imported_data)

  expect_true(length(profiling_data$final_output) == 6) # expect 6 pieces of output
  expect_true(nrow(profiling_data$final_output$quantification) == length(imported_data$Experiments)) # expect rows of output equal to number of samples
  expect_true(ncol(profiling_data$final_output$quantification) == nrow(imported_data$ROI_data)) # expect columns of output equal to number of metabolites
  expect_true(sum(sapply(profiling_data$reproducibility_data, length)) == (nrow(imported_data$ROI_data) * nrow(imported_data$Metadata))) # reproducibility metrics for every ROI for every sample
})

# test_that("output saved to specified directory", {
#   profiling_data <- run_rDolphin(imported_data = imported_data,
#                                  dir = ".")
#
  # expect_true(file.exists("tests/testthat/ROI_profiles_used.csv"))
  # expect_true(file.exists("tests/testthat/chemical_shift.csv"))
  # expect_true(file.exists("tests/testthat/fitting_error.csv"))
  # expect_true(file.exists("tests/testthat/half_bandwidth.csv"))
  # expect_true(file.exists("tests/testthat/intensity.csv"))
  # expect_true(file.exists("tests/testthat/quantification.csv"))
  # expect_true(file.exists("tests/testthat/signal_area_ratio.csv"))
  #
  # file.remove("tests/testthat/ROI_profiles_used.csv")
  # file.remove("tests/testthat/chemical_shift.csv")
  # file.remove("tests/testthat/fitting_error.csv")
  # file.remove("tests/testthat/half_bandwidth.csv")
  # file.remove("tests/testthat/intensity.csv")
  # file.remove("tests/testthat/quantification.csv")
  # file.remove("tests/testthat/signal_area_ratio.csv")
# })


