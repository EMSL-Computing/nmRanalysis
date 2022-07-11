library(nmRanalysis)

data("e_data")
data("f_data")

ppmData <- as.ppmData(e_data = e_data,
                      f_data = f_data,
                      edata_cname = "PPM", fdata_cname = "Sample",
                      instrument_strength = 600, ph = 7.33, solvent = "h2o")

test_that("ppmData_to_rDolphin breaks if normalization or alignment are wrong",{

  # Expect an error for incorrect normalization input
  expect_error(ppmData_to_rDolphin(ppmData, normalization = 7, alignment = 0))

  # Expect an error for incorrect alignment input
  expect_error(ppmData_to_rDolphin(ppmData, normalization = 0, alignment = 7))
})


test_that("ppmData_to_rDolphin function returns expected",{

  # Create rDolphin object
  imported_data <- ppmData_to_rDolphin(ppmData, metabs = "",
                                       normalization = 0, alignment = 0,
                                       nmr_folder_path = "",
                                       data_index_1d = "",
                                       proc_no = "",
                                       default_suppressions = "",
                                       bucket_resolution = "",
                                       #data_path_2d = "",
                                       specific_params = "")

  # Expect attributes
  expect_true(length(imported_data) == 13)
  expect_true(inherits(imported_data, "list"))

  # Expect dimensions
  expect_true(ncol(imported_data$dataset) == length(imported_data$ppm)) # same ppm in dataset and ppm
  expect_true(nrow(imported_data$dataset) == length(imported_data$Experiments)) # same samples in dataset and experiments
  expect_true(nrow(imported_data$dataset) == nrow(imported_data$Metadata)) # same samples in dataset and metadata
  expect_true(length(imported_data$program_parameters) == 24) # 24 rDolphin operating parameters
  expect_true(all(imported_data$norm_factor == 1)) # no normalization applied, so all normalization factors should be 1
  expect_true(ncol(imported_data$final_output$quantification) == nrow(imported_data$ROI_data)) # output column for each ROI
  expect_true(sum(sapply(imported_data$reproducibility_data, length)) == (nrow(imported_data$ROI_data) * nrow(imported_data$Metadata))) # reproducibility metrics for every ROI for every sample
})



