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

profiling_data <- run_rDolphin(imported_data = imported_data)

test_that("format_plotting works as expected", {

  plot.data <- format_plotting(profiling_data = profiling_data)
  expect_equal(nrow(plot.data), 120940)

  plot.data2 <- format_plotting(profiling_data = profiling_data,
                                signals_to_plot = c("L.Arginine_3" = 2, "L.Arginine_2" = 4))
  expect_equal(nrow(plot.data2), 25068)

  # test output
  expect_equal(ncol(plot.data), 12)
})
