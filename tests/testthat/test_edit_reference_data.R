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

roi_df <- imported_data$ROI_data

test_that("search_edit_params works as expected", {
  expect_error(search_edit_params(roi_df, ppm = "3.75"),
               "'ppm' must be numeric")

  # function returns data.frame
  res <- search_edit_params(roi_df, ppm = 3.75)
  expect_s3_class(res, "data.frame")

  # test metabolite filter
  res <- search_edit_params(roi_df, metabolite_name = "L-Alanine", ppm = 3.75)
  expect_true(unique(res$Metabolite) == "L-Alanine")
})

test_that("edit_reference_data works as expected", {
  # single value edit
  res <- edit_reference_data(roi_df,
                             metabolite_name = "L-Alanine",
                             ppm = 3.75,
                             parameter_name = "Quantification.Mode",
                             parameter_value = "Baseline Sum")

  expect_equal(attr(res, "edits"), "row: 10, column: Quantification.Mode, value: Baseline Sum")

  # multiple value edits
  expect_error(edit_reference_data(roi_df,
                                   metabolite_name = c("L-Arginine","L-Alanine"),
                                   ppm = c(3.75),
                                   parameter_name = c("Quantification.Mode", "Roof.effect"),
                                   parameter_value = c("Baseline Sum", "0.35")),
               "Input parameters not of equal length")

  expect_message(edit_reference_data(roi_df,
                                     metabolite_name = c("L-Arginine","L-Alanine"),
                                     ppm = c(3.75, 3.75),
                                     parameter_name = c("Quantification.Mode", "Roof.effect"),
                                     parameter_value = c("Baseline Sum", "0.35")),
                 "Multiple changes were made to input data frame.")

  res <- edit_reference_data(roi_df,
                             metabolite_name = c("L-Arginine","L-Alanine"),
                             ppm = c(3.75, 3.75),
                             parameter_name = c("Quantification.Mode", "Roof.effect"),
                             parameter_value = c("Baseline Sum", "0.35"))

  expect_equal(attr(res, "edits")[1], "row: 8, column: Quantification.Mode, value: Baseline Sum")
  expect_equal(attr(res, "edits")[2], "row: 10, column: Roof.effect, value: 0.35")
})
