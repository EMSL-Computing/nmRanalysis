#' Module: UI elements specific to reference metabolite data uploading
#'
#' @description Copyright (C) 2022 Battelle Memorial Institute
#'
#'  This program is free software; you can redistribute it and/or modify
#'  it under the terms of the GNU General Public License as published by
#'  the Free Software Foundation; either version 2 of the License, or
#'  (at your option) any later version.
#'
#'  This program is distributed in the hope that it will be useful,
#'  but WITHOUT ANY WARRANTY; without even the implied warranty of
#'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'  GNU General Public License for more details.
#'
#'  You should have received a copy of the GNU General Public License along
#'  with this program; if not, write to the Free Software Foundation, Inc.,
#'  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#'
#' @param id A string denoting the namespace id.
#' @param ref_db Dataframe of internal database of reference metabolite data.
#'
#' @details This is the UI component for the module created to handle all uploading/specification of reference (target) metabolite data.
#' The value provided for 'id' should be identical across ref_data_uploadUI() and ref_data_uploadServer().
#'
#' This module component provides the UI elements that allow users to:
#' 1) Select an import method for the target metabolite data. Users may choose to either upload a .xlsx file
#' containing a column of CAS numbers or users may choose to manually specify metabolites by name. Note that
#' autocompletion functionality is available for manual specification.
#' 2) Import target (reference) metabolite data via the selected import method.
#'
#' @import shiny
#'
ref_data_uploadUI <- function(id, ref_db){
  ns <- NS(id)
  tagList(
    h4("Reference Metabolite Data"),
    selectInput(ns("ref_upload_method"), "Select an import method for reference metabolite(s):",
                c("Upload a file" = "file",
                  "Specify from list" = "list")),
    tabsetPanel(
      id = ns("refmet_upload"),
      type = "hidden",
      tabPanelBody(
        value = "file",
        fileInput(ns("uploaded_refmet_file"),
                  label = "Choose a file for upload:"),
        uiOutput(ns("ui_refmetfile_whichcol"))
      ),
      tabPanelBody(
        value = "list",
        # Note: may want to later update to make choices only the set of metabolites that we
        # have data for at the supplied experimental conditions.
        selectizeInput(ns("user_refmets"), label = "List reference metabolite(s) of interest:",
                       choices = unique(ref_db$Solute), multiple = TRUE)
      )
    ),
    # clickable button
    shinyWidgets::actionBttn(inputId = ns("process_ref_inputs"),
                             label = "Query Reference Database",
                             style = "unite",
                             color = "primary",
                             size = "sm")
  )
}

#' Module: Server functions specific to target metabolite data uploading
#'
#' @description Copyright (C) 2022 Battelle Memorial Institute
#'
#'  This program is free software; you can redistribute it and/or modify
#'  it under the terms of the GNU General Public License as published by
#'  the Free Software Foundation; either version 2 of the License, or
#'  (at your option) any later version.
#'
#'  This program is distributed in the hope that it will be useful,
#'  but WITHOUT ANY WARRANTY; without even the implied warranty of
#'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'  GNU General Public License for more details.
#'
#'  You should have received a copy of the GNU General Public License along
#'  with this program; if not, write to the Free Software Foundation, Inc.,
#'  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#'
#' @param id A string denoting the namespace id.
#' @param xpmt_data A reactive object containing experimental NMR data and associated metadata.
#' @param ref_db Dataframe of internal database of reference metabolite data.
#'
#' @details This is the server component for the module created to handle all uploading/specification of reference (target) metabolite data.
#' The value provided for 'id' should be identical across ref_data_uploadUI() and ref_data_uploadServer().
#'
#' This module component provides the back-end code that:
#' 1) Dynamically toggles between the option sets for file upload or manual specification of reference metabolites
#' 2) Reads in uploaded files and searches for columns containing CAS numbers based on suppplied column names
#' 3) Processes and formats uploaded or specified data via nmRanalysis
#' 4) Outputs a warning message if uploaded data are not matched to internal reference data
#'
#' @return A reactive object with two elements. The first, $ref_data, is a dataframe containing the uploaded or manually specified
#' reference data, formatted as per roi_ref_export(). The second, $casno_not_in_db, is a list containing any CAS number(s)
#' and corresponding uploaded data entries that were not matched to entries within our internal database. This element will be null
#' if reference metabolites were manually specified or all uploaded reference metabolites were matched to internal database entries.
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
ref_data_uploadServer <- function(id, xpmt_data, ref_db, connec){
  stopifnot(!is.reactive(ref_db))
  stopifnot(is.reactive(xpmt_data))
  stopifnot(is.reactive(connec))

  moduleServer(id, function(input, output, session){

    # Initialize reactiveValues needed by this module
    rv <- reactiveValues(casno_not_in_db = NULL)

    # Observer to control which set of options for refmet upload are displayed: file upload or manual specification
    observeEvent(c(input$ref_upload_method),
                 {
                   req(xpmt_data())

                   updateTabsetPanel(inputId = "refmet_upload", selected = input$ref_upload_method)
                 })

    # reactive to read in refmet file (when supplied)
    refmet_file <- reactive({
      req(xpmt_data())
      req(input$uploaded_refmet_file)

      ext  <- tools::file_ext(input$uploaded_refmet_file$datapath)
      if(ext != "xlsx"){
        shinyWidgets::show_alert(
          title = "Incorrect file format.",
          text = "The uploaded file should be a .xlsx file and of the same format as the typical Chenomx output file. Specifically,
          column headers should be entered in the third row, and data in rows six and beyond. A column containing
          the CAS numbers of target metabolites is required.",
          type = "error"
        )
      }

      shinyFeedback::feedbackDanger("uploaded_refmet_file",
                                    !(ext == "xlsx"),
                                    "Please upload a .xlsx file.")
      req(ext == "xlsx")
      metab_names_table <- load_file(path    = input$uploaded_refmet_file$datapath,
                                     dataset = "metabolites")

      fmtcheck1 <- !any(!is.na(suppressWarnings(as.numeric(colnames(metab_names_table)))))

      if(!fmtcheck1){
        shinyWidgets::show_alert(
          title = "Incorrect file format.",
          text = "The uploaded file should be a .xlsx file and of the same format as the typical Chenomx output file. Specifically,
          column headers should be entered in the third row, and data in rows six and beyond. A column containing
          the CAS numbers of target metabolites is required.",
          type = "error"
        )
      }

      shinyFeedback::feedbackDanger("uploaded_refmet_file",
                                    !fmtcheck1,
                                    "Please upload a correctly formatted file.")

      req(fmtcheck1)

      return(metab_names_table)
    })

    # Dynamic UI element that asks for column specification after user specifies a file path for the reference
    # metabolite file.
    output$ui_refmetfile_whichcol <- renderUI({
      req(xpmt_data())
      req(input$ref_upload_method == 'file')
      req(refmet_file())

      metab_names_table           <- refmet_file()
      vars                        <- names(metab_names_table)
      isCASinvars <- "CAS Registry" %in% vars

      if(isCASinvars){
        # Update select input based on the variable names found in uploaded reference metabolite file
        selectInput(NS(id, "columns"), "Select column containing CAS registry number",
                    choices  = vars,
                    selected = "CAS Registry")
      } else {
        # Update select input based on the variable names found in uploaded reference metabolite file
        selectInput(NS(id, "columns"), "Select column containing CAS registry number",
                    choices = vars)
      }
    })

    # eventReactive() to fully process the uploading of reference metabolite data, whether via file upload
    # or manual specification
    uploaded_ref_data <- eventReactive(c(input$process_ref_inputs),

                                       {
                                         req(xpmt_data())

                                         if (input$ref_upload_method == 'file') {

                                           req(input$process_ref_inputs > 0)

                                           shinyFeedback::feedbackDanger("uploaded_refmet_file",
                                                                         is.null(input$uploaded_refmet_file$datapath),
                                                                         "Please upload a .xlsx file.")

                                           req(refmet_file())
                                           req(input$columns)

                                           metab_names_table <- refmet_file()
                                           metab_names_table <- metab_names_table[!is.na(metab_names_table[[input$columns]]),]

                                           # Pulls reference metabolites of interest from uploaded file based on column header selected
                                           # that corresponds to the CAS number
                                           user.refchoices <- as.list(metab_names_table[[input$columns]])

                                           if(length(user.refchoices) == 0 |
                                              all(grepl("^[A-Za-z]+$",user.refchoices))){
                                             shinyWidgets::show_alert(
                                               title = "No CAS numbers found.",
                                               text = "No CAS numbers were detected in the selected column. Data were either improperly formatted,
                                               or the selected column does not contain CAS numbers.",
                                               type = "error"
                                             )
                                           }

                                           shinyFeedback::feedbackDanger("columns",
                                                                         length(user.refchoices) == 0 |
                                                                           all(grepl("^[A-Za-z]+$",user.refchoices)),
                                                                         "No CAS numbers detected in selected column.")

                                           req(length(user.refchoices) != 0 & !all(grepl("^[A-Za-z]+$",user.refchoices)))


                                           # Checks whether all metabolites in list are contained in app database
                                           # If there are, saves appropriate reactive value.
                                           rv$casno_not_in_db <- list(CASno = metab_names_table[[input$columns]][metab_names_table[[input$columns]] %ni% ref_db$CASno],
                                                                      table = data.frame(metab_names_table[metab_names_table[[input$columns]] %ni% ref_db$CASno, ,drop = FALSE]))

                                           # Feeds in above to nmRanalysis function that generates formatted dataframe of reference metabolite info
                                           user_reference_data <- roi_ref_export(cas_list            = user.refchoices,
                                                                                 solvent_type        = attr(xpmt_data(), "exp_info")$solvent,
                                                                                 ph                  = attr(xpmt_data(), "exp_info")$ph,
                                                                                 instrument_strength = attr(xpmt_data(), "exp_info")$instrument_strength,
                                                                                 temperature         = attr(xpmt_data(), "exp_info")$temperature,
                                                                                 concentration       = attr(xpmt_data(), "exp_info")$concentration,
                                                                                 connec = connec())

                                           shinyFeedback::feedbackDanger("uploaded_refmet_file",
                                                                         nrow(user_reference_data) == 0,
                                                                         "No ROI data available.")

                                           req(nrow(user_reference_data) > 0)

                                           # Filter to get exact or best match
                                           xpmt_conds <- data.frame(`Frequency (MHz)`    = attr(xpmt_data(), "exp_info")$instrument_strength,
                                                                    `pH`                 = attr(xpmt_data(), "exp_info")$ph,
                                                                    `Concentration (mM)` = attr(xpmt_data(), "exp_info")$concentration,
                                                                    `Temperature (K)`    = attr(xpmt_data(), "exp_info")$temperature,
                                                                    `Solvent`            = attr(xpmt_data(), "exp_info")$solvent,
                                                                    check.names = FALSE)
                                           xpmt_conds <- xpmt_conds[, colSums(is.na(xpmt_conds)) == 0]
                                           cols_to_match <- colnames(xpmt_conds)

                                           unq_metabs <- unique(user_reference_data$Metabolite)
                                           bestmatches <- vector("list")
                                           for(i in 1:length(unq_metabs)){

                                             temp <- user_reference_data %>% dplyr::filter(.data$Metabolite == unq_metabs[i])

                                             matchsum <- rep(0, times = nrow(temp))
                                             if("pH" %in% cols_to_match){
                                               # Note that we are building in a tolerance of 0.1 for pH matching - this may be too generous
                                               matchvec <- ifelse(temp$pH < xpmt_conds$pH + 0.1 & temp$pH > xpmt_conds$pH - 0.1, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             if("Frequency (MHz)" %in% cols_to_match){
                                               matchvec <- ifelse(temp$`Frequency (MHz)` == xpmt_conds$`Frequency (MHz)`, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             if("Concentration (mM)" %in% cols_to_match){
                                               matchvec <- ifelse(temp$`Concentration (mM)` == xpmt_conds$`Concentration (mM)`, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             if("Temperature (K)" %in% cols_to_match){
                                               matchvec <- ifelse(temp$`Temperature (K)` == xpmt_conds$`Temperature (K)`, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             if("Solvent" %in% cols_to_match){
                                               matchvec <- ifelse(temp$`Solvent` == xpmt_conds$`Solvent`, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             temp$Matchsum <- matchsum

                                             bestmatches[[unq_metabs[i]]] <- temp %>% dplyr::group_by(.data$`Quantification Signal`) %>%
                                               dplyr::arrange(desc(.data$`Matchsum`)) %>%
                                               dplyr::slice_head()

                                             rm(temp, matchvec, matchsum)
                                           }

                                           user_reference_data_bestmatch <- Reduce("rbind", bestmatches) %>%
                                             dplyr::arrange(.data$`ROI left edge (ppm)`) %>%
                                             dplyr::group_by(.data$Metabolite) %>%
                                             dplyr::mutate(Quantify = ifelse(.data$`Multiplicity` %in% c("1", "2", "3", "4",
                                                                                                         "s", "d", "t", "q",
                                                                                                         "dd"), 1, 0),
                                                           rowid    = paste0(.data$Metabolite, dplyr::row_number())) %>%
                                             dplyr::select(.data$`ROI left edge (ppm)`, .data$`ROI right edge (ppm)`,
                                                           .data$`Quantification Mode`, .data$`Metabolite`, .data$`Quantification Signal`,
                                                           .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                                                           .data$`Half bandwidth (Hz)`, .data$`Multiplicity`, .data$`J coupling (Hz)`,
                                                           .data$`Roof effect`, .data$`J coupling 2 (Hz)`, .data$`Roof effect 2`,
                                                           .data$`Quantify`, .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`,
                                                           .data$`Temperature (K)`, .data$`Solvent`, .data$`rowid`)


                                           user_reference_data <- user_reference_data %>%
                                             dplyr::group_by(.data$Metabolite) %>%
                                             dplyr::select(.data$`ROI left edge (ppm)`, .data$`ROI right edge (ppm)`,
                                                           .data$`Quantification Mode`, .data$`Metabolite`, .data$`Quantification Signal`,
                                                           .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                                                           .data$`Half bandwidth (Hz)`, .data$`Multiplicity`, .data$`J coupling (Hz)`,
                                                           .data$`Roof effect`, .data$`J coupling 2 (Hz)`, .data$`Roof effect 2`,
                                                           .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`,
                                                           .data$`Temperature (K)`, .data$`Solvent`)
                                           # Note that the ordering of variables above *DOES* matter for
                                           # run_rDolphin()

                                           return(list(bestmatch_data = user_reference_data_bestmatch,
                                                       full_data      = user_reference_data))

                                         } else {
                                           # When using req() within an observer, does not halt computations beyond those in
                                           # the observer it is used.
                                           shinyFeedback::feedbackDanger("user_refmets",
                                                                         !all(input$user_refmets %in% ref_db$Solute),
                                                                         "Invalid choice(s).")

                                           req(all(input$user_refmets %in% ref_db$Solute))

                                           # specify list object on the user provided metabolite names
                                           user.refchoices <- as.list(input$user_refmets)

                                           rv$casno_not_in_db <- NULL

                                           # create an ROI reference object using nmRanalysis to be rendered as a table in the UI
                                           user_reference_data <- roi_ref_export(name_list           = user.refchoices,
                                                                                 solvent_type        = attr(xpmt_data(), "exp_info")$solvent,
                                                                                 ph                  = attr(xpmt_data(), "exp_info")$ph,
                                                                                 instrument_strength = attr(xpmt_data(), "exp_info")$instrument_strength,
                                                                                 temperature         = attr(xpmt_data(), "exp_info")$temperature,
                                                                                 concentration       = attr(xpmt_data(), "exp_info")$concentration,
                                                                                 connec = connec())

                                           shinyFeedback::feedbackDanger("user_refmets",
                                                                         nrow(user_reference_data) == 0,
                                                                         "No ROI data available.")

                                           req(nrow(user_reference_data) > 0)


                                           # Filter to get exact or best match
                                           xpmt_conds <- data.frame(`Frequency (MHz)`    = attr(xpmt_data(), "exp_info")$instrument_strength,
                                                                    `pH`                 = attr(xpmt_data(), "exp_info")$ph,
                                                                    `Concentration (mM)` = attr(xpmt_data(), "exp_info")$concentration,
                                                                    `Temperature (K)`    = attr(xpmt_data(), "exp_info")$temperature,
                                                                    `Solvent`            = attr(xpmt_data(), "exp_info")$solvent,
                                                                    check.names = FALSE)
                                           xpmt_conds <- xpmt_conds[, colSums(is.na(xpmt_conds)) == 0]
                                           cols_to_match <- colnames(xpmt_conds)

                                           unq_metabs <- unique(user_reference_data$Metabolite)
                                           bestmatches <- vector("list")
                                           for(i in 1:length(unq_metabs)){

                                             temp <- user_reference_data %>% dplyr::filter(.data$Metabolite == unq_metabs[i])

                                             matchsum <- rep(0, times = nrow(temp))
                                             if("pH" %in% cols_to_match){
                                               # Note that we are building in a tolerance of 0.1 for pH matching - this may be too generous
                                               matchvec <- ifelse(temp$pH < xpmt_conds$pH + 0.1 & temp$pH > xpmt_conds$pH - 0.1, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             if("Frequency (MHz)" %in% cols_to_match){
                                               matchvec <- ifelse(temp$`Frequency (MHz)` == xpmt_conds$`Frequency (MHz)`, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             if("Concentration (mM)" %in% cols_to_match){
                                               matchvec <- ifelse(temp$`Concentration (mM)` == xpmt_conds$`Concentration (mM)`, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             if("Temperature (K)" %in% cols_to_match){
                                               matchvec <- ifelse(temp$`Temperature (K)` == xpmt_conds$`Temperature (K)`, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             if("Solvent" %in% cols_to_match){
                                               matchvec <- ifelse(temp$`Solvent` == xpmt_conds$`Solvent`, 1, 0)
                                               matchsum <- matchsum + matchvec
                                             }

                                             temp$Matchsum <- matchsum

                                             bestmatches[[unq_metabs[i]]] <- temp %>% dplyr::group_by(.data$`Quantification Signal`) %>%
                                               dplyr::arrange(desc(.data$`Matchsum`)) %>%
                                               dplyr::slice_head()

                                             rm(temp, matchvec, matchsum)
                                           }

                                           user_reference_data_bestmatch <- Reduce("rbind", bestmatches) %>%
                                             dplyr::arrange(.data$`ROI left edge (ppm)`) %>%
                                             dplyr::group_by(.data$Metabolite) %>%
                                             dplyr::mutate(Quantify = ifelse(.data$`Multiplicity` %in% c("1", "2", "3", "4",
                                                                                                         "s", "d", "t", "q",
                                                                                                         "dd"), 1, 0),
                                                           rowid    = paste0(.data$Metabolite, dplyr::row_number())) %>%
                                             dplyr::select(.data$`ROI left edge (ppm)`, .data$`ROI right edge (ppm)`,
                                                           .data$`Quantification Mode`, .data$`Metabolite`, .data$`Quantification Signal`,
                                                           .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                                                           .data$`Half bandwidth (Hz)`, .data$`Multiplicity`, .data$`J coupling (Hz)`,
                                                           .data$`Roof effect`, .data$`J coupling 2 (Hz)`, .data$`Roof effect 2`,
                                                           .data$`Quantify`, .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`,
                                                           .data$`Temperature (K)`, .data$`Solvent`, .data$`rowid`)


                                           user_reference_data <- user_reference_data %>%
                                             dplyr::group_by(.data$Metabolite) %>%
                                             dplyr::select(.data$`ROI left edge (ppm)`, .data$`ROI right edge (ppm)`,
                                                           .data$`Quantification Mode`, .data$`Metabolite`, .data$`Quantification Signal`,
                                                           .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                                                           .data$`Half bandwidth (Hz)`, .data$`Multiplicity`, .data$`J coupling (Hz)`,
                                                           .data$`Roof effect`, .data$`J coupling 2 (Hz)`, .data$`Roof effect 2`,
                                                           .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`,
                                                           .data$`Temperature (K)`, .data$`Solvent`)
                                           # Note that the ordering of variables above *DOES* matter for
                                           # run_rDolphin()

                                           return(list(bestmatch_data = user_reference_data_bestmatch,
                                                       full_data      = user_reference_data))

                                         }
                                       })

    # Observer to produce warning alert only if there is a supplied CAS no that is not contained within the database.
    observeEvent(c(uploaded_ref_data()),
                 {
                   req(xpmt_data())
                   req(uploaded_ref_data())
                   req(rv$casno_not_in_db$CASno)


                   shinyWidgets::show_alert(
                     title = "CAS number(s) not found.",
                     text = paste0("The following were not found in our database: \n",
                                   paste(rv$casno_not_in_db$CASno, collapse = "\n")),
                     type = "warning"
                   )
                 })

    # Module output
    reactive({
      req(xpmt_data())
      req(uploaded_ref_data())

      list(bestmatch_ref_data = uploaded_ref_data()$bestmatch_data,
           full_ref_data      = uploaded_ref_data()$full_data,
           casno_not_in_db    = rv$casno_not_in_db)
    })
  })
}
