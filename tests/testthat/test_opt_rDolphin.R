library(nmRanalysis)

data("e_data")
data("f_data")
data("example_metabs")

ppmData <- as.ppmData(e_data = e_data,
                      f_data = f_data,
                      edata_cname = "PPM", fdata_cname = "Sample",
                      instrument_strength = 600, temperature = 298, solvent = "h2o")

imported_data <- ppmData_to_rDolphin(ppmData = ppmData,
                                     metabs = example_metabs)

test_that("opt_rDolphin works as desired",{

  expect_error(opt_rDolphin(imported_data$dataset),
               "'imported_data' must be 'rDolphin' object, created using `ppmData_to_rDolphin`")

  # test opt_rDolphin
  mydolphin <- opt_rDolphin(imported_data = imported_data)

  # Expect output dimensions
  expect_true(nrow(mydolphin$total_signals_parameters) == nrow(imported_data$ROI_data))
  expect_true(all.equal(mydolphin$ROI_data, imported_data$ROI_data))
})

