library(nmRanalysis)

data("e_data")
data("f_data")

test_that("errors thrown when incorrect inputs supplied", {

  expect_error(as.ppmData(e_data = "edata",
                          f_data = f_data,
                          edata_cname = "PPM", fdata_cname = "Sample",
                          instrument_strength = 600, ph = 7.33, solvent = "h2o"),
               "e_data must be of the class 'data.frame'")

  expect_error(as.ppmData(e_data = e_data,
                          f_data = "fdata",
                          edata_cname = "PPM", fdata_cname = "Sample",
                          instrument_strength = 600, ph = 7.33, solvent = "h2o"),
               "f_data must be of the class 'data.frame'")

  expect_error(as.ppmData(e_data = e_data,
                          f_data = f_data,
                          edata_cname = "PPM", fdata_cname = "Sample",
                          instrument_strength = "A", ph = 7.33, solvent = "h2o"),
               "instrument_strength must be a numeric value of the MHz strength of the instrument")

  expect_error(as.ppmData(e_data = e_data,
                          f_data = f_data,
                          edata_cname = "PPM", fdata_cname = "Sample",
                          instrument_strength = 600, ph = "cat", solvent = "h2o"),
               "ph must be a numeric value indicating the pH of the sample")

  # modify f_data for testing
  f_data_1col <- f_data %>% dplyr::select(Sample)
  expect_error(as.ppmData(e_data = e_data,
                          f_data = f_data_1col,
                          edata_cname = "PPM", fdata_cname = "Sample",
                          instrument_strength = 600, ph = 7.33, solvent = "h2o"),
               "f_data must contain at least 2 columns")
})

test_that("as.ppmData function fails with incorrect column names",{

  expect_error(as.ppmData(e_data = e_data,
                          f_data = f_data,
                          edata_cname = "XYZ", fdata_cname = "Sample",
                          instrument_strength = 600, ph = 7.33, solvent = "h2o"))

  expect_error(as.ppmData(e_data = e_data,
                          f_data = f_data,
                          edata_cname = "PPM", fdata_cname = "SampleName",
                          instrument_strength = 600, ph = 7.33, solvent = "h2o"))

  expect_error(as.ppmData(e_data = e_data,
                          f_data = f_data,
                          edata_cname = "PPM", fdata_cname = "Sample",
                          instrument_strength = 600, ph = 7.33, solvent = "milk"))

})

test_that("as.ppmData function fails with missing samples",{

  # Modify f_data for testing
  f_data_missing2 <- f_data[-2, ]

  expect_error(as.ppmData(e_data = e_data,
                          f_data = f_data_missing2,
                          edata_cname = "PPM", fdata_cname = "Sample",
                          instrument_strength = 600, ph = 7.33, solvent = "h2o"))

  # Modify e_data for testing
  e_data_missing <- e_data[,-4]
  expect_warning(as.ppmData(e_data = e_data_missing,
                            f_data = f_data,
                            edata_cname = "PPM", fdata_cname = "Sample",
                            instrument_strength = 600, ph = 7.33, solvent = "h2o"))
})

test_that("as.ppmData function returns expected",{

  mydata <- as.ppmData(e_data = e_data,
                        f_data = f_data,
                        edata_cname = "PPM", fdata_cname = "Sample",
                        instrument_strength = 600, ph = 7.33, solvent = "h2o")

  # Expect classes
  expect_true(class(mydata) == "ppmData")
  expect_true(class(mydata$e_data) == "data.frame")
  expect_true(class(mydata$f_data) == "data.frame")

  # Expect dimensions
  expect_true(ncol(mydata$e_data) == 5)
  expect_true(nrow(mydata$e_data) == 262144)
  expect_true(nrow(mydata$f_data) == 4)
  # not checking ncol of f_data because that might change currently

  # Expect attributes
  expect_equal(colnames(mydata$e_data[-which(colnames(mydata$e_data) == attr(mydata, "cnames")$edata_cname)]),
               as.character(mydata$f_data[,attr(mydata, "cnames")$fdata_cname]))
  expect_true(class(attr(mydata, "data_info")) == "list")
  expect_true(class(attr(mydata, "exp_info")) == "list")
})



