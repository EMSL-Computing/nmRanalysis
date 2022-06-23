library(nmRanalysisApp)

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
plot.data <- format_plotting(profiling_data = profiling_data)

# outfile <- paste0("results_", Sys.Date())
# rm("_snaps")
# rm(outfile)

test_that("trel_plot creates properly formatted data.frame", {
  p <- trel_plot(plot.data = plot.data)
  expect_equal(nrow(p), 40)
  expect_equal(ncol(p), 10)
})

