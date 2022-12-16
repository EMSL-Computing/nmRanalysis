library(nmRanalysis)

# data("bmse_associations")

test_that("as.bmseList returns correctly formatted output", {

  # test input checks
  expect_error(as.bmseList(c("56-41-7","2613-02-7"), solvent_type = 'D2O', ph = 7.4, instrument_strength = 400),
               "List of CAS numbers must be of the class 'list'.")
  expect_error(as.bmseList(list("56-41-7","2613-02-7"), return_metabs = "exact_match"),
               'Solvent type, temperature, and instrument strength must be specified if return_metabs = "exact_match"')
  expect_error(as.bmseList(list("56-41-7","2613-02-7"), return_metabs = "some"),
               'return_metabs must be one of "exact_match" or "all"')

  # test typical parameter values
  bmse_list <- as.bmseList(list("56-41-7","2613-02-7", "82016-55-5"), solvent_type = 'D2O', ph = 7.4, instrument_strength = 400)
  expect_true(class(bmse_list) == "list")
  expect_true(all(stringr::str_detect(bmse_list, pattern = "bmse")))
  expect_equal(length(bmse_list), bmse_associations %>%
                 dplyr::filter(CASno == "56-41-7" | CASno == "2613-02-7" | CASno == "82016-55-5") %>%
                 nrow())

  # test return_metabs = "all"
  bmse_list_all <- as.bmseList(list("56-41-7","2613-02-7", "82016-55-5"), return_metabs = "all")
  expect_equal(length(bmse_list_all), bmse_associations %>%
                 dplyr::filter(CASno == "56-41-7" | CASno == "2613-02-7" | CASno == "82016-55-5") %>%
                 nrow())

  # test for warnings/messages
  expect_warning(as.bmseList(list("56-41-7","2613-02-7", "82016-55-5", "80000-55-5"),
                             solvent_type = 'D2O', ph = 7.4, instrument_strength = 400),
                 "80000-55-5 is not a recognized CAS registry number")
  expect_message(suppressWarnings(as.bmseList(list("56-41-7","2613-02-7", "82016-55-5", "80000-55-5"),
                             solvent_type = 'D2O', ph = 7.4, instrument_strength = 400)),
                 "Data not available for all provided CAS numbers.")
  # expect_message(as.bmseList(list("56-41-7","2613-02-7", "82016-55-5"),
  #                            return_metabs = "exact_match",
  #                            solvent_type = 'D2O', temperature = 298, instrument_strength = 200),
  #                "Data not available for all provided CAS numbers at the specified conditions.")

})

test_that("as.bmseListFromName returns correctly formatted output", {

  # test input checks
  expect_error(as.bmseListFromName(c("ATP", "Maltose", "Creatinine"), solvent_type = 'D2O', ph = 7.4, instrument_strength = 400),
               "List of metabolite names must be of the class 'list'.")
  expect_error(as.bmseList(list("56-41-7","2613-02-7"), return_metabs = "some"),
               'return_metabs must be one of "exact_match" or "all"')
  expect_error(as.bmseListFromName(list("56-41-7","2613-02-7"), return_metabs = "exact_match"),
               'Solvent type, temperature, and instrument strength must be specified if return_metabs = "exact_match"')
  expect_warning(as.bmseListFromName(list("Chicken"), solvent_type = 'D2O', ph = 7.4, instrument_strength = 400),
               "Chicken is not a recognized metabolite name")

  # test typical parameter values
  bmse_list <- as.bmseListFromName(list("ATP", "Maltose", "Creatinine"),
                                   return_metabs = "exact_match",
                                   solvent_type = 'D2O',
                                   temperature = 298,
                                   ph = 7.4,
                                   instrument_strength = 500,
                                   concentration = 100)
  expect_true(class(bmse_list) == "list")
  expect_true(all(stringr::str_detect(bmse_list, pattern = "bmse")))
  expect_equal(length(bmse_list), bmse_associations %>%
                 dplyr::filter(Solute == "ATP" | Solute == "Maltose" | Solute == "Creatinine",
                               Temperature == 298,
                               Solvent == 'D2O',
                               Field_strength == 500,
                               Concentration == 100) %>%
                 dplyr::pull(Entry_ID) %>%
                 unique() %>% length())
})


# test_that("show_file", {
#   expect_error(show_file(ID = c("bmse000006", "bmse001001")),
#                "'show_file()' takes only one ID, not a list of IDs")
#
#   # setwd("..")
#   # spectra_file <- show_file(local_path = "inst/star_json/", ID = "bmse000006")
#   # expect_equal(spectra_file$entry_id, "bmse000006")
# })


# test_that("get_spectra_data", {
#   get_spectra_data(ID_list = c("bmse000006", "bmse001001", "bmse000946"))
# })

test_that("roi_ref_export", {
  mymetabs <- roi_ref_export(cas_list = list("56-41-7", "74-79-3", "75277-39-3", "82016-55-5"),
                            return_metabs = "exact_match",
                            instrument_strength = 600,
                            solvent_type = 'D2O',
                            temperature = 298,
                            concentration = 100)

  expect_equal(ncol(mymetabs), 18)
  expect_equal(nrow(mymetabs), 5)

  mymetabs_all <- roi_ref_export(cas_list = list("56-41-7", "74-79-3", "75277-39-3", "82016-55-5"),
                                 return_metabs = "all")

  expect_equal(ncol(mymetabs_all), 18)
  expect_equal(nrow(mymetabs_all), 22)

  mymetabs_names <- roi_ref_export(name_list = list("ATP", "Maltose", "Creatinine"),
                             instrument_strength = 400,
                             solvent_type = 'D2O',
                             half_bandwidth = 1.4,
                             roi_tol = 0.02,
                             ph =7.4)

  expect_equal(ncol(mymetabs_names), 18)
  expect_equal(nrow(mymetabs_names), 28)
})

# test_that("nearest_match_metabs returns expected", {
#   mymetabs <- roi_ref_export(cas_list = list("56-41-7", "74-79-3", "75277-39-3", "82016-55-5"))
#   nearest_match_metabs(mymetabs, temperature = 298, instrument_strength = 400)
# })

