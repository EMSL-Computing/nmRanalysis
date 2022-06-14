OOhlibrary(nmRanalysis)

data("bmse_associations")

test_that("as.bmseList returns correctly formatted output", {

  # test input checks
  expect_error(as.bmseList(c("56-41-7","2613-02-7"), solvent_type = 'D2O', ph = 7.4, instrument_strength = 400),
               "List of CAS numbers must be of the class 'list'.")
  expect_error(as.bmseList(list("56-41-7","2613-02-7")), "Solvent type and ph must be specified if return_all == FALSE")
  expect_snapshot_warning(as.bmseList(list("56-47-7"), solvent_type = 'D20', ph = 7.4, instrument_strength = 400))

  # test typical parameter values
  bmse_list <- as.bmseList(list("56-41-7","2613-02-7", "82016-55-5"), solvent_type = 'D2O', ph = 7.4, instrument_strength = 400)
  expect_true(class(bmse_list) == "list")
  expect_true(all(stringr::str_detect(bmse_list, pattern = "bmse")))
  expect_equal(length(bmse_list), bmse_associations %>%
                 dplyr::filter(CASno == "56-41-7" | CASno == "2613-02-7" | CASno == "82016-55-5") %>%
                 dplyr::filter(pH == 7.4) %>%
                 nrow())

  # test alternative solvent value
  bmse_list2 <- as.bmseList(list("56-41-7","2613-02-7", "82016-55-5"), solvent_type = 'H2O', ph = 7.4, instrument_strength = 400)
  expect_true(all(stringr::str_detect(bmse_list, pattern = "bmse")))
  expect_equal(length(bmse_list2), bmse_associations %>%
                 dplyr::filter(CASno == "56-41-7" | CASno == "2613-02-7" | CASno == "82016-55-5") %>%
                 dplyr::filter(pH == 7.4) %>%
                 nrow())

  # test return_all = TRUE
  bmse_list_all <- as.bmseList(list("56-41-7","2613-02-7", "82016-55-5"), return_all = TRUE)
  expect_equal(length(bmse_list_all), bmse_associations %>%
                 dplyr::filter(CASno == "56-41-7" | CASno == "2613-02-7" | CASno == "82016-55-5") %>%
                 nrow())
})

test_that("as.bmseListFromName returns correctly formatted output", {

  # test input checks
  expect_error(as.bmseListFromName(c("ATP", "Maltose", "Creatinine"), solvent_type = 'D2O', ph = 7.4, instrument_strength = 400),
               "List of metabolite names must be of the class 'list'.")
  # expect_error(as.bmseListFromName(list("56-41-7","2613-02-7")), "Solvent type and ph must be specified if return_all == FALSE")
  expect_warning(as.bmseListFromName(list("Chicken"), solvent_type = 'D20', ph = 7.4, instrument_strength = 400),
               "Chicken is not a recognized metabolite name")

  # test typical parameter values
  bmse_list <- as.bmseListFromName(list("ATP", "Maltose", "Creatinine"), solvent_type = 'D2O', ph = 7.4, instrument_strength = 400)
  expect_true(class(bmse_list) == "list")
  expect_true(all(stringr::str_detect(bmse_list, pattern = "bmse")))
  expect_equal(length(bmse_list), bmse_associations %>%
                 dplyr::filter(Solute == "ATP" | Solute == "Maltose" | Solute == "Creatinine") %>%
                 dplyr::filter(pH == 7.4) %>%
                 dplyr::pull(Entry_ID) %>%
                 unique() %>% length())

  # test alternative solvent value
  bmse_list2 <- as.bmseListFromName(list("ATP", "Maltose", "Creatinine"), solvent_type = 'H2O', ph = 7.4, instrument_strength = 400)
  expect_true(all(stringr::str_detect(bmse_list, pattern = "bmse")))
  expect_equal(length(bmse_list2), bmse_associations %>%
                 dplyr::filter(Solute == "ATP" | Solute == "Maltose" | Solute == "Creatinine") %>%
                 dplyr::filter(pH == 7.4) %>%
                 dplyr::pull(Entry_ID) %>%
                 unique() %>% length())

  # test return_all = TRUE
  bmse_list_all <- as.bmseListFromName(list("ATP", "Maltose", "Creatinine"), return_all = TRUE)
  expect_equal(length(bmse_list_all), bmse_associations %>%
                 dplyr::filter(Solute == "ATP" | Solute == "Maltose" | Solute == "Creatinine") %>%
                 dplyr::pull(Entry_ID) %>%
                 unique() %>% length())
})


test_that("show_file", {
  expect_error(show_file(local_path = "../../inst/star_json/", ID = c("bmse000006", "bmse001001")))

  # setwd("..")
  # spectra_file <- show_file(local_path = "inst/star_json/", ID = "bmse000006")
  # expect_equal(spectra_file$entry_id, "bmse000006")
})


# test_that("get_spectra_data", {
#
# })

test_that("roi_ref_export", {
  mymetabs <- roi_ref_export(cas_list = list("56-41-7", "74-79-3", "75277-39-3", "82016-55-5"),
                            instrument_strength = 400,
                            solvent_type = 'D20',
                            half_bandwidth = 1.4,
                            roi_tol = 0.02,
                            ph =7.4)

  expect_equal(ncol(mymetabs), 12)
  expect_equal(nrow(mymetabs), 17)

  mymetabs_all <- roi_ref_export(cas_list = list("56-41-7", "74-79-3", "75277-39-3", "82016-55-5"),
                                 return_all = TRUE)

  expect_equal(ncol(mymetabs_all), 12)
  expect_equal(nrow(mymetabs_all), 17)

  mymetabs_names <- roi_ref_export(name_list = list("ATP", "Maltose", "Creatinine"),
                             instrument_strength = 400,
                             solvent_type = 'D20',
                             half_bandwidth = 1.4,
                             roi_tol = 0.02,
                             ph =7.4)

  expect_equal(ncol(mymetabs_names), 12)
  expect_equal(nrow(mymetabs_names), 15)

  mymetabs_names_all <- roi_ref_export(name_list = list("ATP", "Maltose", "Creatinine"),
                                       return_all = TRUE)

  expect_equal(ncol(mymetabs_names_all), 12)
  expect_equal(nrow(mymetabs_names_all), 15)
})

