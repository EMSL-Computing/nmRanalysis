#' Module: UI elements specific to reference data ROI editing
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
#'
#' @details This is one of the UI components for the module created to handle all editing of reference (target) metabolite data.
#' The value provided for 'id' should be identical across the following: ref_data_ROIeditingUI(), ref_data_add_delUI(),
#' ref_data_quantTab(), ref_data_profileUI(), and ref_data_editingServer().
#'
#' This module component provides the UI elements that allow users to:
#' 1) Select from a dropdown of several options that relate to the main plot. These options include the choice of
#'    sample spectrum to plot, the choice of target metabolites that they wish to edit and display over the sample spectrum,
#'    and whether subplots should be displayed on region select. The dropdown also indicates
#'    what filters (if any) have been applied to the experimental spectral data.
#' 2) Save changes, revert the last saved changes, and revert all saved changes made to a given target
#'    metabolite's data (i.e. fitting parameters)
#' 3) Check the fit of metabolites and make changes to global fitting parameters
#'
#' This module component also displays target metabolite data within an interactive plot and table.
#'
#' @import shiny
#' @importFrom magrittr %>%
#'
ref_data_ROIeditingUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("vizoptions_ui")),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns('refmet_dspedt_selected_plot'))),
    fluidRow(
      column(
        width = 1,
        uiOutput(ns("ui_refmet_dspedt_savechanges"))
      ),
      column(
        width = 2,
        uiOutput(ns("ui_refmet_dspedt_revert_savechanges"))
      ),
      column(
        width = 2,
        uiOutput(ns("ui_refmet_dspedt_revert_all_savechanges"))
      ),
      column(
        width = 4, offset = 3,
        htmlOutput(ns("distance_text"))
      )
    ),
    h5(tags$b("Select a Signal:")),
    uiOutput(ns("fitcheck")),
    fluidRow(
      column(
        width = 2,
        shinyWidgets::switchInput(
          inputId = ns("auto_optim"),
          label = "Auto-Optimize",
          size = "small")
        ),
      column(width = 9,
             htmlOutput(ns("autoptim_note"))
             )
    ),
    h5(""),
    uiOutput(ns("ui_global_profiling_parameters")),
    uiOutput(ns("ui_metabolite_signal_options")),
    DT::dataTableOutput(ns("refmet_dspedt_table"))
  )
}

#' Module: UI elements specific to adding/removing target metabolites
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
#'
#' @details This is one of the UI components for the module created to handle all editing of reference (target) metabolite data.
#' The value provided for 'id' should be identical across the following: ref_data_ROIeditingUI(), ref_data_add_delUI(),
#' ref_data_quantTab(), ref_data_profileUI(), and ref_data_editingServer().
#'
#' This module component provides the UI elements that allow users to:
#' 1) Specify metabolite(s) to add or remove
#' 2) Add/remove specified metabolite(s)
#'
#' This module component also displays our internal reference database within a searchable, filterable table.
#'
#' @import shiny
#'
ref_data_add_delUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        uiOutput(ns("ui_refmet_add_options"))
      ),
      column(
        width = 1,
        h4("or", style="text-align: center;")
      ),
      column(
        width = 3,
        uiOutput(ns("ui_refmet_add_new_entry"))
      )
    ),
    actionButton(ns("refmet_add"), "Add"),
    h4(""),

    fluidRow(
      column(
        width = 4,
        uiOutput(ns("ui_refmet_remove_options"))
      )
    ),
    actionButton(ns("refmet_remove"), "Remove"),
    h4(""),

    DT::dataTableOutput(ns("refmet_database"))
  )
}


#' Module: Server functions specific to target metabolite data modification
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
#' @param ref_data A reactive object containing target metabolite data.
#' @param ref_db Dataframe of internal database of reference metabolite data.
#'
#' @details This is the server component for the module created to handle all editing of reference (target) metabolite data.
#' The value provided for 'id' should be identical across the following: ref_data_ROIeditingUI(), ref_data_add_delUI(),
#' ref_data_quantTab(), ref_data_profileUI(), and ref_data_editingServer().
#'
#' This module component provides the back-end code that:
#' 1) Populates the list of editable target metabolites that users may select from
#' 2) Adds and/or removes target metabolites
#' 3) Creates/formats reference (target) metabolite data for display and editing
#' 4) Creates the dropdown containing several plot options.
#' 5) Saves/reverts changes made to target metabolite data
#' 6) Allows for plot-table interactivity such that changes made to certain plot elements reflect in the accompanying table and vice-versa
#' 7) Initiates profiling of target metabolites
#'
#' @return A reactive object containing the edited target (reference) metabolite data, with attributes describing the history of
#' changes made to the initial data; a logical value (TRUE/FALSE) indicating whether the user confirmed proceeding with profiling;
#' and a list of global profiling parameters.
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
ref_data_editingServer <- function(id, xpmt_data, ref_data, ref_db, connec){
  stopifnot(is.reactive(xpmt_data))
  stopifnot(is.reactive(ref_data))
  stopifnot(!is.reactive(ref_db))
  stopifnot(is.reactive(connec))
  moduleServer(id, function(input, output, session){

    # Initialize reactiveValues needed by this module:
    # initialize with change_counter that indexes made to refmet data
    # initialize with refchanges to store all changes made as a list, with one element per change set.
    # initialize with unsaved_change to temporarily store any unsaved change.
    rv <- reactiveValues(change_counter = list(),
                         refchanges     = list(),
                         unsaved_change = list(),
                         obs_show_subplot_suspend = TRUE,
                         obs_annot_update_suspend = TRUE,
                         obs_annot_update_subplot_suspend = TRUE,
                         subplot_dat = NULL)

    observe(priority = 2, {
      req(ref_data())

      rv$unedited_bestmatch_ref_data <- ref_data()$bestmatch_ref_data # Needed for 'revert all changes'
      rv$user_reference_data <- ref_data()$bestmatch_ref_data
      rv$full_reference_data <- ref_data()$full_ref_data

    })

    # Observer to populate refmet choices after reference metabolites have been uploaded/specified/added/removed
    observeEvent(c(input$refmet_add, input$refmet_remove, ref_data()$bestmatch_ref_data),
                 {
                   req(rv$user_reference_data)

                   updateSelectizeInput(inputId = "which_refmet_dspedt",
                                        choices = unique(rv$user_reference_data$Metabolite))

                   # Reset field for specifying new entries (under add/remove metabolites)
                   updateSelectizeInput(inputId = "newentry_toadd",
                                        choices = c(""),
                                        selected = c(""))
                 })

    # Add/Delete Reference Metabolites ---------------------------------

    # Observer to add specified reference metabolite(s) to the set of reference metabolites already under consideration
    observeEvent(c(input$refmet_add),
                 {
                   req(ref_data())
                   req(xpmt_data())

                   # Check if any existing metabolites are being added
                   if(!is.null(input$refmet_toadd)){

                     # specify list object on the user provided metabolite names
                     added.refchoices <- as.list(input$refmet_toadd)

                     # create an ROI reference object using nmRanalysis to be rendered as a table in the UI
                     added_reference_data <- suppressMessages(roi_ref_export(name_list           = added.refchoices,
                                                                             solvent_type        = attr(xpmt_data(), "exp_info")$solvent,
                                                                             ph                  = attr(xpmt_data(), "exp_info")$ph,
                                                                             instrument_strength = attr(xpmt_data(), "exp_info")$instrument_strength,
                                                                             temperature         = attr(xpmt_data(), "exp_info")$temperature,
                                                                             concentration       = attr(xpmt_data(), "exp_info")$concentration))

                     if(is.null(added_reference_data)){
                       shinyFeedback::feedbackDanger("refmet_toadd",
                                                     is.null(added_reference_data),
                                                     "No ROI data available.")
                     } else{
                       shinyFeedback::feedbackDanger("refmet_toadd",
                                                     nrow(added_reference_data) == 0 | is.null(added_reference_data),
                                                     "No ROI data available.")
                     }

                     req(added_reference_data)
                     req(nrow(added_reference_data) > 0)

                     # Filter to get exact or best match
                     xpmt_conds <- data.frame(`Frequency (MHz)`    = ifelse(is.null(attr(xpmt_data(), "exp_info")$instrument_strength),
                                                                            NA, attr(xpmt_data(), "exp_info")$instrument_strength),
                                              `pH`                 = ifelse(is.null(attr(xpmt_data(), "exp_info")$ph),
                                                                            NA, attr(xpmt_data(), "exp_info")$ph),
                                              `Concentration (mM)` = ifelse(is.null(attr(xpmt_data(), "exp_info")$concentration),
                                                                            NA, attr(xpmt_data(), "exp_info")$concentration),
                                              `Temperature (K)`    = ifelse(is.null(attr(xpmt_data(), "exp_info")$temperature),
                                                                            NA, attr(xpmt_data(), "exp_info")$temperature),
                                              `Solvent`            = ifelse(is.null(attr(xpmt_data(), "exp_info")$solvent),
                                                                            NA, attr(xpmt_data(), "exp_info")$solvent),
                                              check.names = FALSE)
                     xpmt_conds <- xpmt_conds[, colSums(is.na(xpmt_conds)) == 0]
                     cols_to_match <- colnames(xpmt_conds)

                     unq_metabs <- unique(added_reference_data$Metabolite)
                     bestmatches <- vector("list")
                     for(i in 1:length(unq_metabs)){

                       temp <- added_reference_data %>% dplyr::filter(.data$Metabolite == unq_metabs[i])

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
                         dplyr::arrange(dplyr::desc(.data$`Matchsum`)) %>%
                         dplyr::slice_head()

                       rm(temp, matchvec, matchsum)
                     }

                     added_reference_data_bestmatch <- Reduce("rbind", bestmatches) %>%
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


                     added_reference_data <- added_reference_data %>%
                       dplyr::group_by(.data$Metabolite) %>%
                       dplyr::select(.data$`ROI left edge (ppm)`, .data$`ROI right edge (ppm)`,
                                     .data$`Quantification Mode`, .data$`Metabolite`, .data$`Quantification Signal`,
                                     .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                                     .data$`Half bandwidth (Hz)`, .data$`Multiplicity`, .data$`J coupling (Hz)`,
                                     .data$`Roof effect`, .data$`J coupling 2 (Hz)`, .data$`Roof effect 2`,
                                     .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`,
                                     .data$`Temperature (K)`, .data$`Solvent`)




                     rv$user_reference_data <- dplyr::bind_rows(rv$user_reference_data, added_reference_data_bestmatch)


                     rv$full_reference_data <- dplyr::bind_rows(rv$full_reference_data, added_reference_data)
                     rv$unedited_bestmatch_ref_data <- dplyr::bind_rows(rv$unedited_bestmatch_ref_data, added_reference_data_bestmatch)
                   }

                   # Check if any new metabolites are being added
                   if(!is.null(input$newentry_toadd)){

                     check1 <- grepl("\\[", input$newentry_toadd) | grepl("\\]", input$newentry_toadd)
                     valid_newentries <- input$newentry_toadd[!check1]
                     if(length(valid_newentries) == 0){
                       shinyWidgets::show_alert(
                         title = "Metabolite Naming error.",
                         text = "The name of the profiled metabolite should not contain the following characters: '[', ']'",
                         type = "error"
                       )
                     }
                     req(length(valid_newentries) > 0)

                     if(length(valid_newentries) != length(input$newentry_toadd)){
                       shinyWidgets::show_alert(
                         title = "Some metabolites were not added.",
                         text = "One or more specified metabolite names contained invalid characters and were therefore not added.",
                         type = "warning"
                       )
                     }

                     added_entry_data <- data.frame(`ROI left edge (ppm)` = rep(0.02, length(valid_newentries)),
                                                    `ROI right edge (ppm)` = rep(-0.02, length(valid_newentries)),
                                                    `Quantification Mode` = rep("Baseline Fitting", length(valid_newentries)),
                                                    `Metabolite` = valid_newentries,
                                                    `Quantification Signal` = rep(1, length(valid_newentries)),
                                                    `Chemical shift(ppm)` = rep(0, length(valid_newentries)),
                                                    `Chemical shift tolerance (ppm)` = rep(0.005, length(valid_newentries)),
                                                    `Half bandwidth (Hz)` = rep(1, length(valid_newentries)),
                                                    `Multiplicity` = rep("1", length(valid_newentries)),
                                                    `J coupling (Hz)` = rep(0, length(valid_newentries)),
                                                    `Roof effect` = rep(0, length(valid_newentries)),
                                                    `J coupling 2 (Hz)` = rep(0, length(valid_newentries)),
                                                    `Roof effect 2` = rep(0, length(valid_newentries)),
                                                    `Quantify` = rep(1, length(valid_newentries)),
                                                    `Frequency (MHz)` = ifelse(is.null(attr(xpmt_data(), "exp_info")$instrument_strength),
                                                                               rep(NA, length(valid_newentries)),
                                                                               rep(attr(xpmt_data(), "exp_info")$instrument_strength, length(valid_newentries))),
                                                    `pH` = ifelse(is.null(attr(xpmt_data(), "exp_info")$ph),
                                                                  rep(NA, length(valid_newentries)),
                                                                  rep(attr(xpmt_data(), "exp_info")$ph, length(valid_newentries))),
                                                    `Concentration (mM)` = ifelse(is.null(attr(xpmt_data(), "exp_info")$concentration),
                                                                                  rep(NA, length(valid_newentries)),
                                                                                  rep(attr(xpmt_data(), "exp_info")$concentration, length(valid_newentries))),
                                                    `Temperature (K)` = ifelse(is.null(attr(xpmt_data(), "exp_info")$temperature),
                                                                               rep(NA, length(valid_newentries)),
                                                                               rep(attr(xpmt_data(), "exp_info")$temperature, length(valid_newentries))),
                                                    `Solvent` = ifelse(is.null(attr(xpmt_data(), "exp_info")$solvent),
                                                                       rep(NA, length(valid_newentries)),
                                                                       rep(attr(xpmt_data(), "exp_info")$solvent, length(valid_newentries))),
                                                    `rowid` = paste0(valid_newentries, "1"),
                                                    check.names = FALSE)

                     rv$user_reference_data <- dplyr::bind_rows(rv$user_reference_data, added_entry_data)

                     rv$full_reference_data <- dplyr::bind_rows(rv$full_reference_data, added_entry_data)
                     rv$unedited_bestmatch_ref_data <- dplyr::bind_rows(rv$unedited_bestmatch_ref_data, added_entry_data)
                   }



                   updateSelectizeInput(session,
                                        "which_refmet_dspedt",
                                        "Select Reference Metabolite(s) to Display/Edit:",
                                        choices = unique(rv$user_reference_data$Metabolite))

                   updateSelectizeInput(session,
                                        "refmet_toadd",
                                        "Select from Existing:",
                                        choices = setdiff(unique(ref_db$Solute), unique(rv$user_reference_data$Metabolite)))

                   updateSelectizeInput(session,
                                        "refmet_toremove",
                                        "Select Metabolite(s) to Remove:",
                                        choices = unique(rv$user_reference_data$Metabolite))
                 })

    # Observer to remove specified reference metabolite(s) from the set of reference metabolites already under consideration
    observeEvent(c(input$refmet_remove),
                 {
                   req(ref_data())
                   req(xpmt_data())


                   temp <- rv$user_reference_data %>% dplyr::filter(.data$Metabolite %ni% input$refmet_toremove)
                   shinyFeedback::feedbackDanger("refmet_toremove",
                                                 nrow(temp) == 0,
                                                 "At least one metabolite must remain.")
                   req(nrow(temp) > 0)

                   rv$user_reference_data <- temp

                   rv$full_reference_data <- rv$full_reference_data %>% dplyr::filter(.data$Metabolite %ni% input$refmet_toremove)
                   rv$unedited_bestmatch_ref_data <- rv$unedited_bestmatch_ref_data %>% dplyr::filter(.data$Metabolite %ni% input$refmet_toremove)

                   # Also, remove stored changes and counter (from refchanges, change_counter) corresponding to removed metabolite
                   rv$refchanges <- rv$refchanges[names(rv$refchanges) %ni% input$refmet_toremove]
                   rv$change_counter <- rv$change_counter[names(rv$change_counter) %ni% input$refmet_toremove]

                   updateSelectizeInput(session,
                                        "which_refmet_dspedt",
                                        "Select Reference Metabolite(s) to Display/Edit:",
                                        choices = unique(rv$user_reference_data$Metabolite))

                   updateSelectizeInput(session,
                                        "refmet_toadd",
                                        "Select from Existing:",
                                        choices = setdiff(unique(ref_db$Solute), unique(rv$user_reference_data$Metabolite)))

                   updateSelectizeInput(session,
                                        "refmet_toremove",
                                        "Select Metabolite(s) to Remove:",
                                        choices = unique(rv$user_reference_data$Metabolite))
                 })

    # UI element for the options of adding of reference metabolites
    output$ui_refmet_add_options <- renderUI({

      req(ref_data())

      addchoices <- setdiff(unique(ref_db$Solute), unique(ref_data()$bestmatch_ref_data$Metabolite))
      selectizeInput(NS(id, "refmet_toadd"),
                     label = "Select from Existing:",
                     choices = addchoices, multiple = TRUE)
    })

    # UI element for the option of adding a completely new entry (not contained in the database)
    output$ui_refmet_add_new_entry <- renderUI({

      req(ref_data())

      selectizeInput(NS(id, "newentry_toadd"),
                     label = "Specify New:",
                     choices  = c(""),
                     multiple = TRUE,
                     options  = list(create = TRUE))
    })

    # UI element for the options of removal of reference metabolites
    output$ui_refmet_remove_options <- renderUI({

      req(ref_data())

      selectizeInput(NS(id, "refmet_toremove"),
                     label = "Select Metabolite(s) to Remove:",
                     choices = unique(ref_data()$bestmatch_ref_data$Metabolite), multiple = TRUE)
    })

    #----------------------------------------------------------------------------------------------------------

    # Reference metabolite-specific dataset creation and visualization ----------------------------------------

    # observer to create the copy (dspedt_user_reference_data) that corresponds to the
    # reference dataset filtered according to choice of reference metabolites to display/edit
    # Also resets rv$unsaved_change and updates annotations on main plot to reflect the selected
    # reference metabolite.
    observeEvent(c(input$which_refmet_dspedt), ignoreNULL = TRUE, ignoreInit = TRUE,
                 {
                   req(ref_data())

                   # reference metabolite copy
                   rv$dspedt_user_reference_data <- rv$user_reference_data %>%
                     dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt)

                   # clear any unsaved changes
                   rv$unsaved_change <- list()

                   ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                              pltproxy = refmet_dspedt_plot_proxy,
                                              newdat = rv$dspedt_user_reference_data)

                 })

    # This datatable corresponds to the selected reference metabolite data to display/edit
    output$refmet_dspedt_table <- DT::renderDT({

      req(ref_data())

      isolate({
        # Note: Remove quantification mode column, but allow users to specify quantification mode on a per-ROI basis on the
        # separate subtab (Reference Data for Quantification) where reference data are grouped by ROIs
        rv$user_reference_data %>% dplyr::ungroup() %>%
          dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
          dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"),
                        `Signal left edge (ppm)` = .data$`ROI left edge (ppm)`,
                        `Signal right edge (ppm)` = .data$`ROI right edge (ppm)`) %>%
          dplyr::select(.data$Signal, .data$Quantify, .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                        .data$`Signal left edge (ppm)`,
                        .data$`Signal right edge (ppm)`, .data$`Half bandwidth (Hz)`, .data$Multiplicity, .data$`J coupling (Hz)`,
                        .data$`J coupling 2 (Hz)`, .data$`Roof effect`, .data$`Roof effect 2`,
                        .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`,
                        .data$`Solvent`) %>%
          DT::datatable(rownames   = FALSE,
                        editable   = TRUE,
                        selection = "none",
                        options = list(scrollX = TRUE)) %>%
          DT::formatRound(columns = c("Chemical shift(ppm)", "Chemical shift tolerance (ppm)",
                                      "Signal left edge (ppm)", "Signal right edge (ppm)",
                                      "Half bandwidth (Hz)", "J coupling (Hz)", "J coupling 2 (Hz)", "Roof effect",
                                      "Roof effect 2", "Frequency (MHz)", "pH", "Concentration (mM)", "Temperature (K)"),
                          digits = 3)

      })
    })

    # Create proxy for the refmet datatable to more fluidly interact with refmet plot and table edits.
    refmet_dspedt_table_proxy <- DT::dataTableProxy('refmet_dspedt_table')

    # Plotly plot
    output$refmet_dspedt_selected_plot <- plotly::renderPlotly({

      isolate({
        req(ref_data())
        req(xpmt_data())

        # Code to resume the observer that was started in a suspended state. We also update the value of
        # rv$obs_show_subplot_suspend so that $resume() is not called every time this plot is rendered,
        # but only after the first rendering of the plot.
        if(rv$obs_show_subplot_suspend){
          obs_show_subplot$resume()
          rv$obs_show_subplot_suspend <- FALSE
        }
        if(rv$obs_annot_update_suspend){
          obs_annot_update$resume()
          rv$obs_annot_update_suspend <- FALSE
        }
        plotly::plot_ly(source = "id_refmet_dspedt_selected_plot", type = "scatter", mode = "lines") %>%
          plotly::event_register("plotly_relayout") %>%
          plotly::config(displaylogo = FALSE,
                         modeBarButtons = list(list("select2d"), list("zoom2d"), list("zoomIn2d"),
                                               list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                               list("resetScale2d"), list("toImage"))) %>%
          plotly::layout(title = paste("Experimental Data:"),
                         xaxis = list(title     = "PPM",
                                      autorange = "reversed"),
                         yaxis = list(title     = "Intensity"),
                         dragmode = "zoom2d") %>%
          plotly::config(edits = list(annotationTail     = TRUE,
                                      annotationText     = FALSE,
                                      annotationPosition = FALSE,
                                      shapePosition      = TRUE))
      })

    })

    # Create proxy for the above plotly plot to more fluidly interact with refmet plot and table edits.
    refmet_dspedt_plot_proxy <- plotly::plotlyProxy("refmet_dspedt_selected_plot")

    # This observer is responsible for plotting the trace (i.e. line) corresponding to a selected
    # experimental spectrum. This is implemented through proxy updates for the sake of efficiency.
    observeEvent(c(input$sample_to_plot, xpmt_data()), priority = -1, {
      req(input$sample_to_plot)
      req(input$sample_to_plot %in% names(xpmt_data()$e_data))
      req(input$which_refmet_dspedt)

      xpmt_data_sample <- xpmt_data()$e_data %>% dplyr::select(.data$PPM, .data[[input$sample_to_plot]])
      df_long <- xpmt_data_sample %>%
        tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")

      # Clear shapes and annotations
      plotly::plotlyProxyInvoke(refmet_dspedt_plot_proxy, "relayout",
                                list(annotations = NULL,
                                     shapes = NULL))
      # Clear plots
      plotly::plotlyProxyInvoke(refmet_dspedt_plot_proxy, "deleteTraces", as.list(as.integer(0)))

      # Line shape update
      ROI_lines <- ROI_line_gen(data = rv$dspedt_user_reference_data)

      # Annotation update
      ROI_annots <- ROI_annot_gen(data = rv$dspedt_user_reference_data)

      plotly::plotlyProxyInvoke(refmet_dspedt_plot_proxy, "addTraces",
                                list(x    = df_long$PPM,
                                     y    = df_long$Intensity,
                                     type = 'scatter',
                                     mode = 'lines',
                                     line = list(width = 1),
                                     hoverinfo = "text",
                                     text = paste0("PPM: ", round(df_long$PPM, 4), "<br>",
                                                   "Intensity: ", round(df_long$Intensity, 4))))
      plotly::plotlyProxyInvoke(refmet_dspedt_plot_proxy, "relayout",
                                list(title = paste("Experimental Data:", input$sample_to_plot, "<br>", "<sup>",
                                                   input$which_refmet_dspedt, "Peak Location(s) displayed", "</sup>"),
                                     annotations = ROI_annots,
                                     shapes = ROI_lines))
    })

    output$vizoptions_ui <- renderUI({

      req(xpmt_data())
      req(ref_data())

      shinyWidgets::dropdownButton(
        # Allows users to select which sample spectrum to display.
        selectInput(inputId = NS(id, "sample_to_plot"),
                    label   = "Choose a spectrum to plot",
                    choices = colnames(xpmt_data()$e_data)[-1]),

        selectizeInput(NS(id, "which_refmet_dspedt"),
                       "Select a metabolite to display and edit:",
                       choices = unique(ref_data()$bestmatch_ref_data$Metabolite)),

        # Toggle for subplot display
        shinyWidgets::materialSwitch(inputId = NS(id, "show_subplot"),
                                     label   = "Show subplot on box select",
                                     value   = FALSE,
                                     status  = "primary",
                                     right   = TRUE),


        # HTML output to display the filters currently applied
        htmlOutput(NS(id,"applied_filters_text")),

        circle = TRUE, status = "info",
        icon = icon("cog"), width = "300px",

        tooltip = shinyWidgets::tooltipOptions(title = "Plot Options")
      )
    })

    # Output (in HTML format) to display the filters that are currently applied to the data.
    output$applied_filters_text <- renderUI({

      if(length(attr(xpmt_data(), "filters")) == 0){
        htmltools::HTML("<strong>Currently applied filters:</strong><br/>None")

      } else{
        allfilts <- rlist::list.ungroup(rlist::list.select(attr(xpmt_data(), "filters"), range))
        allfilts <- Reduce("c", lapply(allfilts, function(x){paste0("(", x$min, ", ", x$max, ")")}))
        htmltools::HTML(paste0("<strong>Currently applied filters:</strong><br/>", paste(allfilts, collapse = "<br/>")))

      }
    })

    # Plotting of the subplot of ppm data across all sample spectra at the selected region
    output$refmet_dspedt_selected_subplot <- plotly::renderPlotly({
      req(ref_data())
      req(input$which_refmet_dspedt)
      req(input$sample_to_plot)
      req(input$show_subplot)

      brushedData <- plotly::event_data("plotly_brushed", source = "id_refmet_dspedt_selected_plot")


      if(is.null(brushedData)){
        return(NULL)
      }

      if(rv$obs_annot_update_subplot_suspend){
        obs_annot_update_subplot$resume()
        rv$obs_annot_update_subplot_suspend <- FALSE
      }

      isolate({
        ROI_lines <- ROI_line_gen(data = rv$dspedt_user_reference_data)
        ROI_annots <- ROI_annot_gen(data = rv$dspedt_user_reference_data)

        subplot_dat <- subplot_refmet_data(xpmt_data      = xpmt_data()$e_data,
                                           brushed_data   = brushedData,
                                           sample_to_plot = input$sample_to_plot,
                                           ROI_lines      = ROI_lines,
                                           ROI_annots     = ROI_annots,
                                           sourceid       = "id_refmet_dspedt_selected_subplot")
        rv$subplot_idx <- subplot_dat$ROIidx
        subplot_dat$plot
      })

    })

    # Create proxy for the above plotly subplot to more fluidly interact with refmet plot and table edits.
    refmet_dspedt_subplot_proxy <- plotly::plotlyProxy("refmet_dspedt_selected_subplot")

    # Observer to control pop-up (i.e. modal) containing the subplot of spectral data at a selected region.
    # Note: This works fine, but the only thing that I would like to change is
    # loading of subsequent plots generated by different brush events.
    # On initial plot, the loading spinner shows, but on subsequent plots, it does not.
    # Not sure how to fix this yet, but I suspect the issue lies in the execution order.
    # This observer triggers before "e_data_subplot" invalidates.
    obs_show_subplot <- observeEvent(plotly::event_data("plotly_brushed", source = "id_refmet_dspedt_selected_plot"), suspended = TRUE, {
      req(input$show_subplot)


      brushedData <- plotly::event_data("plotly_brushed", source = "id_refmet_dspedt_selected_plot")

      req(!identical(brushedData, rv$subplot_dat))

      removeModal()
      showModal(
        modalDialog(
          shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, 'refmet_dspedt_selected_subplot'))),
          title = paste0("All Sample Spectra: ", round(min(brushedData$x),3)," PPM to ", round(max(brushedData$x),3), " PPM"),
          size = "xl",
          easyClose = TRUE,
          fade = FALSE
        ))
      rv$subplot_dat <- brushedData
    })

    #----------------------------------------------------------------------------------------------------------

    # Save/Revert Changes Functionality -----------------------------------------------------------------------

    # Save changes made to refmet data based on plot changes (specific to actively edited metabolite)
    observeEvent(c(input$save_refmet_plot_changes),
                 {
                   req(ref_data())
                   req(input$which_refmet_dspedt)
                   req(input$save_refmet_plot_changes > 0)


                   rv <- refmet_save_update(updated_refmet = input$which_refmet_dspedt,
                                            rvlist = rv)

                 })

    # Revert last saved change to refmet data (specific to actively edited metabolite)
    observeEvent(c(input$revert_last_refmet_save_changes),
                 {
                   req(ref_data())
                   req(input$which_refmet_dspedt)
                   req(input$revert_last_refmet_save_changes > 0)

                   rv <- refmet_revert_update(updated_refmet = input$which_refmet_dspedt,
                                              rvlist = rv)

                   ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                              pltproxy = refmet_dspedt_plot_proxy,
                                              newdat = rv$dspedt_user_reference_data)
                 })

    # Revert all saved change to refmet data (specific to actively edited metabolite)
    observeEvent(c(input$revert_all_refmet_save_changes),
                 {
                   req(ref_data())
                   req(input$which_refmet_dspedt)
                   req(input$revert_all_refmet_save_changes > 0)

                   rv <- refmet_revert_update(updated_refmet = input$which_refmet_dspedt,
                                              rvlist = rv,
                                              all = TRUE)
                   ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                              pltproxy = refmet_dspedt_plot_proxy,
                                              newdat = rv$dspedt_user_reference_data)
                 })

    # Dynamic action button to save changes made to plot of reference metabolite
    output$ui_refmet_dspedt_savechanges <- renderUI({

      req(ref_data())
      req(input$which_refmet_dspedt)
      req(rv$unsaved_change[[input$which_refmet_dspedt]])

      actionButton(NS(id, "save_refmet_plot_changes"),
                   label = "Save")
    })

    # Dynamic action button to revert save changes made to plot of reference metabolite
    output$ui_refmet_dspedt_revert_savechanges <- renderUI({

      req(ref_data())
      req(input$which_refmet_dspedt)
      req(rv$change_counter[[input$which_refmet_dspedt]] > 0)

      actionButton(NS(id, "revert_last_refmet_save_changes"),
                   label = "Revert Last Save")
    })

    # Dynamic action button to revert all save changes made to plot of reference metabolite
    output$ui_refmet_dspedt_revert_all_savechanges <- renderUI({

      req(ref_data())
      req(input$which_refmet_dspedt)

      og_version <- rv$unedited_bestmatch_ref_data %>%
        dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
        dplyr::arrange(.data$`Quantification Signal`)
      curr_version <- rv$user_reference_data %>%
        dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
        dplyr::arrange(.data$`Quantification Signal`)

      req(!identical(og_version, curr_version))

      actionButton(NS(id, "revert_all_refmet_save_changes"),
                   label = "Revert All Saves")
    })

    #----------------------------------------------------------------------------------------------------------

    # Interactive reference data editing ----------------------------------------------------------------------

    # Observer to update dspedt_user_reference_data with changes made to
    # annotations in plotly plot.
    obs_annot_update <- observeEvent(plotly::event_data("plotly_relayout", source = "id_refmet_dspedt_selected_plot"), suspended = TRUE,
                                     {
                                       req(ref_data())

                                       # Store event data
                                       dat_allchanges <- plotly::event_data("plotly_relayout", source = "id_refmet_dspedt_selected_plot")

                                       # Extract any changes to x0 position
                                       x0_fields <- ifelse(length(grep("*.\\.x0", names(dat_allchanges), value = TRUE)) == 0,
                                                           NA, grep("*.\\.x0", names(dat_allchanges), value = TRUE))

                                       # Extract any changes to x1 position
                                       x1_fields <- ifelse(length(grep("*.\\.x1", names(dat_allchanges), value = TRUE)) == 0,
                                                           NA, grep("*.\\.x1", names(dat_allchanges), value = TRUE))

                                       req(!all(is.na(c(x0_fields, x1_fields))))


                                       if(!is.na(x0_fields)){
                                         changed_row <-
                                           as.numeric(gsub("[^0-9.-]", "",
                                                           regmatches(x0_fields,
                                                                      gregexpr("\\[.*?\\]", x0_fields))[[1]])) + 1
                                       } else if(!is.na(x1_fields)){
                                         changed_row <-
                                           as.numeric(gsub("[^0-9.-]", "",
                                                           regmatches(x1_fields,
                                                                      gregexpr("\\[.*?\\]", x1_fields))[[1]])) + 1
                                       }

                                       req(changed_row <= nrow(rv$dspedt_user_reference_data))


                                       rv$dspedt_user_reference_data <-
                                         refmet_data_change_fromplot(dspedt_refmet_data = rv$dspedt_user_reference_data,
                                                                     event_data = dat_allchanges,
                                                                     x0_fields  = x0_fields,
                                                                     x1_fields  = x1_fields)

                                       ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                                  pltproxy = refmet_dspedt_plot_proxy,
                                                                  newdat = rv$dspedt_user_reference_data)

                                       # Store the unsaved changes
                                       rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data
                                     })

    # Observer to update dspedt_user_reference_data with changes made to
    # annotations in plotly plot.
    obs_annot_update_subplot <- observeEvent(plotly::event_data("plotly_relayout", source = "id_refmet_dspedt_selected_subplot"), suspended = TRUE,
                                             {
                                               req(ref_data())

                                               # Store event data
                                               dat_allchanges <- plotly::event_data("plotly_relayout", source = "id_refmet_dspedt_selected_subplot")

                                               # Extract any changes to x0 position
                                               x0_fields <- ifelse(length(grep("*.\\.x0", names(dat_allchanges), value = TRUE)) == 0,
                                                                   NA, grep("*.\\.x0", names(dat_allchanges), value = TRUE))

                                               # Extract any changes to x1 position
                                               x1_fields <- ifelse(length(grep("*.\\.x1", names(dat_allchanges), value = TRUE)) == 0,
                                                                   NA, grep("*.\\.x1", names(dat_allchanges), value = TRUE))

                                               req(!all(is.na(c(x0_fields, x1_fields))))


                                               if(!is.na(x0_fields)){
                                                 changed_row <-
                                                   as.numeric(gsub("[^0-9.-]", "",
                                                                   regmatches(x0_fields,
                                                                              gregexpr("\\[.*?\\]", x0_fields))[[1]])) + min(which(rv$subplot_idx))
                                               } else if(!is.na(x1_fields)){
                                                 changed_row <-
                                                   as.numeric(gsub("[^0-9.-]", "",
                                                                   regmatches(x1_fields,
                                                                              gregexpr("\\[.*?\\]", x1_fields))[[1]])) + min(which(rv$subplot_idx))
                                               }

                                               req(changed_row <= nrow(rv$dspedt_user_reference_data))



                                               rv$dspedt_user_reference_data <-
                                                 refmet_data_change_fromplot(dspedt_refmet_data = rv$dspedt_user_reference_data,
                                                                             event_data         = dat_allchanges,
                                                                             x0_fields          = x0_fields,
                                                                             x1_fields          = x1_fields,
                                                                             rv                 = rv)

                                               ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                                          pltproxy = refmet_dspedt_plot_proxy,
                                                                          newdat   = rv$dspedt_user_reference_data)

                                               # Line shape update
                                               ROI_lines <- ROI_line_gen(data = rv$dspedt_user_reference_data)

                                               # Annotation update
                                               ROI_annots <- ROI_annot_gen(data = rv$dspedt_user_reference_data)

                                               # Update plot
                                               plotly::plotlyProxyInvoke(refmet_dspedt_subplot_proxy, "relayout",
                                                                         list(annotations = ROI_annots[rv$subplot_idx],
                                                                              shapes = ROI_lines[rv$subplot_idx]))

                                               # Store the unsaved changes
                                               rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data
                                             })

    # Observer to update dspedt_user_reference_data with changes made directly to
    # the datatable.
    observeEvent(input$refmet_dspedt_table_cell_edit,
                 {
                   req(ref_data())

                   info <- input$refmet_dspedt_table_cell_edit
                   changed_row <- info$row
                   changed_col <- info$col + 1 # column index starts at 0
                   v <- info$value

                   # Modify rv$dspedt_user_reference_data so that it is formatted the same way as the displayed table
                   # This is only done to extract the correct column name
                   temp <- rv$dspedt_user_reference_data %>% dplyr::ungroup() %>% dplyr::select(-.data$rowid) %>%
                     dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"),
                                   `Signal left edge (ppm)` = .data$`ROI left edge (ppm)`,
                                   `Signal right edge (ppm)` = .data$`ROI right edge (ppm)`) %>%
                     dplyr::select(.data$Signal, .data$Quantify, .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                                   .data$`Signal left edge (ppm)`,
                                   .data$`Signal right edge (ppm)`, .data$`Half bandwidth (Hz)`, .data$Multiplicity, .data$`J coupling (Hz)`,
                                   .data$`J coupling 2 (Hz)`, .data$`Roof effect`, .data$`Roof effect 2`,
                                   .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`,
                                   .data$`Solvent`)


                   edtd_colname <- names(temp)[changed_col]

                   req(changed_row <= nrow(rv$dspedt_user_reference_data))

                   if(edtd_colname %in% c("Chemical shift(ppm)", "Signal left edge (ppm)", "Signal right edge (ppm)")) {

                     if(edtd_colname %in% c("Signal left edge (ppm)")){
                       if(as.numeric(v) < rv$dspedt_user_reference_data$"ROI right edge (ppm)"[changed_row]){

                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Invalid entry.",
                           text = "The signal left edge should be larger than the signal right edge.",
                           type = "error"
                         )

                         req(as.numeric(v) > rv$dspedt_user_reference_data$"ROI right edge (ppm)"[changed_row])
                       }
                     }

                     if(edtd_colname %in% c("Signal right edge (ppm)")){
                       if(as.numeric(v) > rv$dspedt_user_reference_data$"ROI left edge (ppm)"[changed_row]){

                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Invalid entry.",
                           text = "The signal right edge should be less than the signal left edge.",
                           type = "error"
                         )

                         req(as.numeric(v) < rv$dspedt_user_reference_data$"ROI left edge (ppm)"[changed_row])
                       }
                     }

                     rv$dspedt_user_reference_data <-
                       refmet_data_change_fromtab(dspedt_refmet_data = rv$dspedt_user_reference_data,
                                                  changed_row  = changed_row,
                                                  change       = as.numeric(v),
                                                  edtd_colname = edtd_colname,
                                                  round_num    = 3)

                     ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                pltproxy = refmet_dspedt_plot_proxy,
                                                newdat = rv$dspedt_user_reference_data)

                     # Store the unsaved changes
                     rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data

                   } else if(edtd_colname %in% c("Half bandwidth (Hz)", "J coupling (Hz)", "Chemical shift tolerance (ppm)",
                                                 "J coupling 2 (Hz)", "Roof effect", "Roof effect 2")) {

                     if(edtd_colname %in% c("Half bandwidth (Hz)")){

                       if(as.numeric(v) <= 0){

                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Invalid entry.",
                           text = "The specified value must be larger than 0.",
                           type = "error"
                         )
                       }

                       req(as.numeric(v) > 0)

                       if(as.numeric(v) - input$gpp_widthtolerance <= 0){
                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Invalid entry.",
                           text = "The lower bandwidth bound based on the specified bandwidth and tolerance is less than or equal to 0.",
                           type = "error"
                         )
                       }

                       req(as.numeric(v) - input$gpp_widthtolerance > 0)
                     }

                     if(edtd_colname %in% c("Chemical shift tolerance (ppm)")){

                       if(as.numeric(v) < 0){

                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Invalid entry.",
                           text = "The specified value must be larger than or equal to 0.",
                           type = "error"
                         )
                       }

                       req(as.numeric(v) >= 0)

                       dist <- (rv$dspedt_user_reference_data$"ROI left edge (ppm)"[changed_row] -
                                  rv$dspedt_user_reference_data$"ROI right edge (ppm)"[changed_row])/2

                       if(as.numeric(v) > dist){
                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Invalid entry.",
                           text = "The specified value must be less than or equal to half the signal interval width",
                           type = "error"
                         )
                       }

                       req(as.numeric(v) <= dist)
                     }


                     if(edtd_colname %in% c("J coupling (Hz)", "J coupling 2 (Hz)")){

                       if(as.numeric(v) < 0){

                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Invalid entry.",
                           text = "The specified value must be larger than or equal to 0.",
                           type = "error"
                         )
                       }

                       req(as.numeric(v) >= 0)

                       if(rv$dspedt_user_reference_data$"Multiplicity"[changed_row] %ni% c("1", "s")){

                         if(edtd_colname %in% c("J coupling (Hz)")){
                           if(as.numeric(v) - input$gpp_j_coupling_variation < 0){
                             ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                        pltproxy = refmet_dspedt_plot_proxy,
                                                        newdat = rv$dspedt_user_reference_data)

                             shinyWidgets::show_alert(
                               title = "Invalid entry.",
                               text = "The lower J-coupling bound based on the specified J-coupling and tolerance is less than 0.",
                               type = "error"
                             )
                           }

                           req(as.numeric(v) - input$gpp_j_coupling_variation >= 0)
                         }

                       }

                     }


                     if(edtd_colname %in% c("Roof effect", "Roof effect 2")){

                       if(rv$dspedt_user_reference_data$"Multiplicity"[changed_row] %ni%
                          c("1", "s", "2", "d", "3", "t", "dd")){
                         if(as.numeric(v) != 0){

                           ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                      pltproxy = refmet_dspedt_plot_proxy,
                                                      newdat = rv$dspedt_user_reference_data)

                           shinyWidgets::show_alert(
                             title = "Not currently supported.",
                             text = "Profiling is currently only supported for s, d, t, and dd splitting patterns (multiplicities), when non-zero roof effect(s) are specified.",
                             type = "error"
                           )
                         }

                         req(as.numeric(v) == 0)
                       }
                     }

                     rv$dspedt_user_reference_data[[edtd_colname]][changed_row] <- as.numeric(v)

                     # Store the unsaved changes
                     rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data

                   } else if(edtd_colname %in% c("Multiplicity")){

                     if(!is.na(suppressWarnings(as.numeric(v)))){
                       if(as.numeric(v) <= 0){
                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Invalid entry.",
                           text = "Numeric multiplicity values must be larger than 0.",
                           type = "error"
                         )
                       }

                       req(as.numeric(v) > 0)
                     }

                     if(v %ni% c("1", "s", "2", "d", "3", "t", "4", "q", "dd")){
                       ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                  pltproxy = refmet_dspedt_plot_proxy,
                                                  newdat = rv$dspedt_user_reference_data)

                       shinyWidgets::show_alert(
                         title = "Not currently supported.",
                         text = "Profiling is currently only supported for s, d, t, q, and dd multiplets.",
                         type = "error"
                       )
                     }

                     req(v %in% c("1", "s", "2", "d", "3", "t", "4", "q", "dd"))


                     if(rv$dspedt_user_reference_data$"Roof effect"[changed_row] != 0 |
                        rv$dspedt_user_reference_data$"Roof effect 2"[changed_row] != 0){
                       if(v %ni% c("1", "s", "2", "d", "3", "t", "dd")){
                         ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                    pltproxy = refmet_dspedt_plot_proxy,
                                                    newdat = rv$dspedt_user_reference_data)

                         shinyWidgets::show_alert(
                           title = "Not currently supported.",
                           text = "Profiling is currently only supported for s, d, t, and dd splitting patterns (multiplicities), when non-zero roof effect(s) are specified.",
                           type = "error"
                         )
                       }

                       req(v %in% c("1", "s", "2", "d", "3", "t", "dd"))
                     }


                     # multiplicity should remain character-valued so as to allow for specifications like "dd", "s", and so forth.
                     rv$dspedt_user_reference_data[[edtd_colname]][changed_row] <- v

                     # Store the unsaved changes
                     rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data

                   } else if(edtd_colname == "Quantify"){

                     if(v %ni% c("0", "1")){

                       ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                  pltproxy = refmet_dspedt_plot_proxy,
                                                  newdat = rv$dspedt_user_reference_data)

                       shinyWidgets::show_alert(
                         title = "Invalid entry.",
                         text = "Specify 0 to exclude the corresponding peak from quantification; 1 otherwise.",
                         type = "error"
                       )
                     }

                     req(v %in% c("0", "1"))

                     if(rv$dspedt_user_reference_data$"Multiplicity"[changed_row] %ni%
                        c("1", "s", "2", "d", "3", "t", "4", "q", "dd")){

                       ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                  pltproxy = refmet_dspedt_plot_proxy,
                                                  newdat = rv$dspedt_user_reference_data)

                       shinyWidgets::show_alert(
                         title = "Not currently supported.",
                         text = "Profiling is currently only supported for s, d, t, q, and dd splitting patterns (multiplicities)",
                         type = "error"
                       )

                     }

                     req(rv$dspedt_user_reference_data$"Multiplicity"[changed_row] %in%
                           c("1", "s", "2", "d", "3", "t", "4", "q", "dd"))

                     rv$dspedt_user_reference_data[[edtd_colname]][changed_row] <- as.numeric(v)

                     ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                pltproxy = refmet_dspedt_plot_proxy,
                                                newdat = rv$dspedt_user_reference_data)

                     # Store the unsaved changes
                     rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data

                   } else {

                     ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                pltproxy = refmet_dspedt_plot_proxy,
                                                newdat = rv$dspedt_user_reference_data)

                     shinyWidgets::show_alert(
                       title = "Non-editable Field.",
                       type = "error"
                     )
                   }
                 })

    #----------------------------------------------------------------------------------------------------------

    # Quantification Data, Fit Check, and Profiling Trigger ---------------------------------------------------------------

    # Define selection options for quantification checks to be one of the signals for the selected metabolite
    output$fitcheck <- renderUI({

      req(input$which_refmet_dspedt)
      req(rv$user_reference_data)

      temp <- rv$user_reference_data %>% dplyr::ungroup() %>%
        dplyr::filter(.data$Quantify == 1) %>%
        dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
        dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"))

      fluidRow(
        column(
          width = 2,
          selectInput(NS(id, "signal_to_check"),
                      label = NULL,
                      choices = temp$Signal)
        ),
        column(
          width = 2,
          actionButton(NS(id, "show_metquant"), label = "Check Signal Fit")
        )
      )

    })

    # Update selection options for quantification checks to be one of the signals for the selected metabolite
    observeEvent(c(input$which_refmet_dspedt), ignoreNULL = TRUE, ignoreInit = TRUE,
                 {
                   req(rv$user_reference_data)

                   temp <- rv$user_reference_data %>% dplyr::ungroup() %>%
                     dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
                     dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"))

                   updateSelectInput(inputId = NS(id, "signal_to_check"),
                                     choices = temp$Signal)
                 })

    # Defines new, "collapsed" or "merged" ROIs to be used for quantification.
    observe({
      req(rv$user_reference_data)

      # Retain only those signals that will be quantified.
      tempdf <- rv$user_reference_data %>% dplyr::filter(.data$Quantify == 1)

      # Merge metabolite signals whose bounds overlap into single ROIs
      tempdf <- tempdf %>% dplyr::arrange(.data$`Chemical shift(ppm)`)
      if(nrow(tempdf) > 1){
        for(i in 1:(nrow(tempdf) - 1)){
          if(tempdf$`ROI left edge (ppm)`[i] - tempdf$`ROI right edge (ppm)`[i+1] >= 0){
            # If there is an overlap, update the right end (lower value) of the subsequent, overlapped ROI to that of
            # the minimum between the two.
            tempdf$`ROI right edge (ppm)`[i+1] <- min(tempdf$`ROI right edge (ppm)`[i], tempdf$`ROI right edge (ppm)`[i+1])
            # Also update the ith right end and all others that share the same right end value to the same minimum.
            tempdf$`ROI right edge (ppm)`[which(tempdf$`ROI right edge (ppm)` == tempdf$`ROI right edge (ppm)`[i])] <-
              tempdf$`ROI right edge (ppm)`[i+1]
            # Last, update the left end (higher value) of any other ROIs with the maximum right end value among all that share the
            # updated ROI right edge
            tempdf$`ROI left edge (ppm)`[which(tempdf$`ROI right edge (ppm)` == tempdf$`ROI right edge (ppm)`[i])] <-
              max(tempdf$`ROI left edge (ppm)`[which(tempdf$`ROI right edge (ppm)` == tempdf$`ROI right edge (ppm)`[i])])
          }
        }
      }

      # Change experimental conditions of final quantification data to reflect that of the current experimental sample
      tempdf$`Frequency (MHz)`    <- ifelse(is.null(attr(xpmt_data(), "exp_info")$instrument_strength),
                                            NA, attr(xpmt_data(), "exp_info")$instrument_strength)
      tempdf$pH                   <- ifelse(is.null(attr(xpmt_data(), "exp_info")$ph),
                                            NA, attr(xpmt_data(), "exp_info")$ph)
      tempdf$`Concentration (mM)` <- ifelse(is.null(attr(xpmt_data(), "exp_info")$concentration),
                                            NA, attr(xpmt_data(), "exp_info")$concentration)
      tempdf$`Temperature (K)`    <- ifelse(is.null(attr(xpmt_data(), "exp_info")$temperature),
                                            NA, attr(xpmt_data(), "exp_info")$temperature)
      tempdf$Solvent              <- ifelse(is.null(attr(xpmt_data(), "exp_info")$solvent),
                                            NA, attr(xpmt_data(), "exp_info")$solvent)

      # Create new reactive value to store the ROI-collapsed data
      rv$quantdat <- tempdf
    })

    # Observer that performs quantification for individually selected metabolite and spectrum, and plots
    # resulting fit on the displayed plot (i.e. output$refmet_dspedt_selected_plot)
    observeEvent(c(input$show_metquant),{
      req(input$show_metquant > 0)
      req(rv$dspedt_user_reference_data)

      if(!is.null(rv$unsaved_change[[input$which_refmet_dspedt]])){
        shinyWidgets::show_alert(
          title = "Unsaved changes present.",
          text = "Save changes prior to checking the signal fit.",
          type = "error"
        )
      }
      req(is.null(rv$unsaved_change[[input$which_refmet_dspedt]]))


      temp <- rv$quantdat %>%
        dplyr::filter(paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]") == input$signal_to_check)

      if(temp$`ROI left edge (ppm)` < temp$`ROI right edge (ppm)`){
        shinyWidgets::show_alert(
          title = "Fitting parameter error.",
          text = "The signal left edge should be larger than the signal right edge.",
          type = "error"
        )
      }
      req(temp$`ROI left edge (ppm)` > temp$`ROI right edge (ppm)`)

      if(temp$`Chemical shift tolerance (ppm)` > round(abs(temp$`ROI right edge (ppm)` - temp$`Chemical shift(ppm)`), 3) |
         temp$`Chemical shift tolerance (ppm)` > round(abs(temp$`ROI left edge (ppm)` - temp$`Chemical shift(ppm)`), 3)){
        shinyWidgets::show_alert(
          title = "Fitting parameter error.",
          text = "The chemical shift tolerance is larger than half the specified width of the signal region.",
          type = "error"
        )
      }
      req(temp$`Chemical shift tolerance (ppm)` <= round(abs(temp$`ROI right edge (ppm)` - temp$`Chemical shift(ppm)`), 3) &
            temp$`Chemical shift tolerance (ppm)` <= round(abs(temp$`ROI left edge (ppm)` - temp$`Chemical shift(ppm)`), 3))

      if((temp$`Half bandwidth (Hz)` - input$gpp_widthtolerance) <= 0){
        shinyWidgets::show_alert(
          title = "Fitting parameter error.",
          text = "The lower bandwidth bound based on the specified bandwidth and tolerance is less than or equal to 0.",
          type = "error"
        )
      }
      req((temp$`Half bandwidth (Hz)` - input$gpp_widthtolerance) > 0)

      if(temp$`Multiplicity` %ni% c("1", "2", "3", "4", "s", "d", "t", "q", "dd")){
        shinyWidgets::show_alert(
          title = "Fitting parameter error.",
          text = "The specified multiplicity is not supported.",
          type = "error"
        )
      }
      req(temp$`Multiplicity` %in% c("1", "2", "3", "4", "s", "d", "t", "q", "dd"))

      if(temp$`Multiplicity` %ni% c("1", "s")){
        if((temp$`J coupling (Hz)` - input$gpp_j_coupling_variation) <= 0){
          shinyWidgets::show_alert(
            title = "Fitting parameter error.",
            text = "The lower J-coupling bound based on the specified J-coupling and tolerance must be greater than 0 for non-singlet multiplicities.",
            type = "error"
          )
        }
        req((temp$`J coupling (Hz)` - input$gpp_j_coupling_variation) > 0)

        if(temp$`J coupling 2 (Hz)` != 0){
          if((temp$`J coupling 2 (Hz)` - input$gpp_j_coupling_variation) <= 0){
            shinyWidgets::show_alert(
              title = "Fitting parameter error.",
              text = "The lower J-coupling bound based on the specified J-coupling 2 and tolerance must be greater than 0 for non-singlet multiplicities.",
              type = "error"
            )
          }
          req((temp$`J coupling 2 (Hz)` - input$gpp_j_coupling_variation) > 0)

          if((temp$`J coupling 2 (Hz)` - temp$`J coupling (Hz)`) > 0){
            shinyWidgets::show_alert(
              title = "Fitting parameter error.",
              text = "J coupling 2 must be smaller than J coupling.",
              type = "error"
            )
          }
          req((temp$`J coupling 2 (Hz)` - temp$`J coupling (Hz)`) <= 0)
        }
      } else if(temp$`Multiplicity` %in% c("1", "s")){
        if(temp$`J coupling (Hz)` != 0 | temp$`J coupling 2 (Hz)` != 0){
          shinyWidgets::show_alert(
            title = "Fitting parameter error.",
            text = "J-coupling values should be set to 0 for singlets.",
            type = "error"
          )
        }
        req(temp$`J coupling (Hz)` == 0 & temp$`J coupling 2 (Hz)` == 0)
      }

      signalROI_right <- temp %>%
        .$`ROI right edge (ppm)`
      signalROIdat <- rv$quantdat %>% dplyr::filter(.data$`ROI right edge (ppm)` == signalROI_right)

      temp_gpps <- list(BGdensity                          = input$gpp_BGdensity,
                        widthtolerance                     = input$gpp_widthtolerance,
                        gaussian                           = input$gpp_gaussian,
                        j_coupling_variation               = input$gpp_j_coupling_variation,
                        BG_gaussian_percentage             = input$gpp_BG_gaussian_percentage,
                        BG_width                           = input$gpp_BG_width,
                        BG_width_tolerance                 = input$gpp_BG_width_tolerance,
                        errorprov                          = input$gpp_errorprov,
                        fitting_maxiter                    = input$gpp_fitting_maxiter,
                        nls_lm_maxiter                     = 200, # from rDolphin defaults
                        ftol                               = 1e-06, # from rDolphin defaults
                        ptol                               = 1e-06, # from rDolphin defaults
                        factor                             = 0.01, # from rDolphin defaults
                        additional_signal_ppm_distance     = 0.002, # from rDolphin defaults
                        signals_to_add                     = 2, # from rDolphin defaults
                        fitting_maxiterrep                 = 0, # from rDolphin defaults
                        additional_signal_improvement      = 0.75, # from rDolphin defaults
                        additional_signal_percentage_limit = 3, # from rDolphin defaults
                        peakdet_minimum                    = 0.01) # from rDolphin defaults

      if(is.null(rv$dspedt_profiling_data[[input$sample_to_plot]][[input$signal_to_check]]) |
         !identical(rv$curr_ROI_profile[[input$sample_to_plot]][[input$signal_to_check]], signalROIdat) |
         !identical(rv$fitcheck_gpps, temp_gpps) |
         !identical(rv$fitcheck_autoptim, input$auto_optim)){

        shinyWidgets::progressSweetAlert(
          session = session,
          id = "metquant_profiling_progress",
          value = 0, title = "",
          display_pct = TRUE, striped = TRUE, status = "info"
        )

        shinyWidgets::updateProgressBar(
          session = session,
          id = "metquant_profiling_progress",
          title = "Importing and processing necessary data to begin the profiling...",
          value = 0
        )

        txpmt_data <- as.ppmData(e_data              = xpmt_data()$e_data %>% dplyr::select(.data$PPM, .data[[input$sample_to_plot]]),
                                 f_data              = xpmt_data()$f_data %>% dplyr::filter(.data$Sample == input$sample_to_plot),
                                 edata_cname         = "PPM",
                                 fdata_cname         = "Sample",
                                 instrument_strength = attr(xpmt_data(), "exp_info")$instrument_strength,
                                 ph                  = as.numeric(attr(xpmt_data(), "exp_info")$ph),
                                 solvent             = attr(xpmt_data(), "exp_info")$solvent,
                                 temperature         = attr(xpmt_data(), "exp_info")$temperature,
                                 concentration       = attr(xpmt_data(), "exp_info")$concentration)

        # Formats the data object
        imported_data <- ppmData_to_rDolphin(ppmData = txpmt_data,
                                             metabs  = signalROIdat)
        imported_data$program_parameters <- temp_gpps

        rv$fitcheck_gpps     <- imported_data$program_parameters
        rv$fitcheck_autoptim <- input$auto_optim

        ROI_data           <- imported_data$ROI_data
        spectra_to_profile <- which(rownames(imported_data$dataset) %in% input$sample_to_plot)

        # Begin direct pull from rDolphin source code.
        # We pull directly from source so that we could generate progress bars
        signals_names <- make.names(paste(ROI_data[,4], ROI_data[,5], sep='_'))
        dummy <- matrix(NaN, nrow(imported_data$dataset), length(signals_names),
                        dimnames = list(imported_data$Experiments, signals_names))

        final_output <- list(quantification    = dummy,
                             signal_area_ratio = dummy,
                             fitting_error     = dummy,
                             chemical_shift    = dummy,
                             intensity         = dummy,
                             half_bandwidth    = dummy)

        #creation of list of necessary parameters to load quantifications and evaluate quality of them
        reproducibility_data <- vector('list', length(imported_data$Experiments))

        for (i in seq_along(reproducibility_data)){
          reproducibility_data[[i]] <- vector('list',length(signals_names))
        }
        for (i in seq_along(reproducibility_data)) {
          for (j in seq_along(reproducibility_data[[i]])) {
            reproducibility_data[[i]][[j]] <- list(Ydata              = NULL,
                                                   Xdata              = NULL,
                                                   ROI_profile        = imported_data$ROI_data[j,],
                                                   program_parameters = NULL,
                                                   plot_data          = NULL,
                                                   FeaturesMatrix     = NULL,
                                                   signals_parameters = NULL,
                                                   results_to_save    = NULL,
                                                   error1             = 1000000)
          }}

        #Splitting of ROI data into individual signals to be quantified
        dummy <- which(is.na(ROI_data[, 1]))

        if (length(dummy) == 0){
          dummy <- dim(ROI_data)[1]+1
        }
        lal <- which(duplicated(ROI_data[-dummy,1:2]) == F)
        ROI_separator <- cbind(lal, c(lal[-1] - 1, dim(ROI_data[-dummy,])[1]))

        baselinedataset <- baseline::baseline.rollingBall(imported_data$dataset,5,5)$baseline

        # There will always only be a single ROI being profiled. However, there may be multiple signals
        ROI_index <- 1

        #Preparation of ROI parameters
        ROI_profile <- ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]
        ROI_buckets <- which.min(abs(as.numeric(ROI_profile[1, 1]) - imported_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2]) - imported_data$ppm))

        if (length(ROI_buckets) < 20) {
          shinyWidgets::updateProgressBar(
            session = session,
            id      = "metquant_profiling_progress",
            title   = paste0("Region of interest too small. Profiling aborted."),
            value   = trunc(1/2*100)
          )
          shinyWidgets::closeSweetAlert(session = session)
        }
        req(length(ROI_buckets) >= 20)

        if (ROI_buckets[1] > ROI_buckets[2]){
          ROI_buckets <- rev(ROI_buckets)
        }


        # Preparation of program parameters to be used during fitting, with some variables added to ease interpretability of code
        program_parameters             <- imported_data$program_parameters
        program_parameters$freq        <- imported_data$freq
        program_parameters$ROI_buckets <- ROI_buckets
        program_parameters$buck_step   <- imported_data$buck_step

        Xdata        <- imported_data$ppm[ROI_buckets]
        fitting_type <- as.character(ROI_profile[1, 3])
        program_parameters$clean_fit <- "N"

        signals_to_quantify <- which(ROI_profile[, 5] >= 1)
        signals_codes       <- (ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2])


        shinyWidgets::updateProgressBar(
          session = session,
          id      = "metquant_profiling_progress",
          title   = paste0('Profiling ', input$signal_to_check, ' and surrounding signals',
                           " for Spectrum: ", input$sample_to_plot),
          value   = trunc(1/2*100)
        )

        output <- profiling_func(spectrum_index       = which(rownames(imported_data$dataset) %in% input$sample_to_plot),
                                 signals_codes        = signals_codes,
                                 imported_data        = imported_data,
                                 ROI_buckets          = ROI_buckets,
                                 fitting_type         = fitting_type,
                                 program_parameters   = program_parameters,
                                 Xdata                = Xdata,
                                 Ydata                = NULL,
                                 final_output         = final_output,
                                 reproducibility_data = reproducibility_data,
                                 ROI_profile          = ROI_profile,
                                 baselinedataset      = baselinedataset,
                                 signals_to_quantify  = signals_to_quantify,
                                 pb                   = NULL)
        # Note that Ydata is internally defined by profiling function despite
        # what is supplied to the argument above. See automatic_profiling.R and
        # profiling_func().
        final_output <- output$final_output
        reproducibility_data <- output$reproducibility_data

        shinyWidgets::closeSweetAlert(session = session)


        rv$dspedt_profiling_data[[input$sample_to_plot]][[input$signal_to_check]] <-
          list(final_output = lapply(final_output,
                                     function(x) x[rownames(x) == input$sample_to_plot, , drop = FALSE]),
               reproducibility_data = reproducibility_data[[which(rownames(imported_data$dataset) %in% input$sample_to_plot)]])

        rv$curr_ROI_profile[[input$sample_to_plot]][[input$signal_to_check]] <- signalROIdat


        if(input$auto_optim){
          temp <- rv$dspedt_user_reference_data %>%
            dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"),
                          Signal2 = make.names(paste(.data$Metabolite, .data$`Quantification Signal`, sep='_')))
          whichrow <- which(temp$Signal == input$signal_to_check)
          whichsig <- which(signals_names %in% make.names(gsub("\\]", "", gsub("\\ \\[", "_", input$signal_to_check))))

          if(length(whichsig) == 0){
            shinyWidgets::show_alert(
              title = "Metabolite Naming error.",
              text = "The name of the profiled metabolite should not contain the following characters: '[', ']'",
              type = "error"
            )
          }
          req(length(whichsig) > 0)

          opt_signal_params <- reproducibility_data[[1]][[whichsig]]$signals_parameters[, whichsig, drop = FALSE]

          dist <- (rv$dspedt_user_reference_data[["ROI left edge (ppm)"]][[whichrow]] -
                     rv$dspedt_user_reference_data[["ROI right edge (ppm)"]][[whichrow]])/2

          new_chemshift <- as.numeric(opt_signal_params[2,])

          rv$dspedt_user_reference_data[["Chemical shift(ppm)"]][[whichrow]]  <- round(new_chemshift,3)
          rv$dspedt_user_reference_data[["ROI left edge (ppm)"]][[whichrow]]  <- round(new_chemshift + dist,3)
          rv$dspedt_user_reference_data[["ROI right edge (ppm)"]][[whichrow]] <- round(new_chemshift - dist,3)
          rv$dspedt_user_reference_data[["Chemical shift tolerance (ppm)"]][[whichrow]] <- round(min(dist/2,
                                                                                                     rv$dspedt_user_reference_data[["Chemical shift tolerance (ppm)"]][[whichrow]]),
                                                                                                 3)


          rv$dspedt_user_reference_data[["Half bandwidth (Hz)"]][[whichrow]] <- as.numeric(opt_signal_params[3,])
          rv$dspedt_user_reference_data[["J coupling (Hz)"]][[whichrow]]     <- as.numeric(opt_signal_params[5,])

          if(rv$dspedt_user_reference_data[["Multiplicity"]][[whichrow]] == "dd"){
            rv$dspedt_user_reference_data[["J coupling 2 (Hz)"]][[whichrow]] <- as.numeric(opt_signal_params[6,])
          }

          ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                     pltproxy = refmet_dspedt_plot_proxy,
                                     newdat = rv$dspedt_user_reference_data)

          # Store the unsaved changes
          rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data
        }
      }


    })

    observeEvent(c(rv$dspedt_profiling_data, input$show_metquant),{
      req(input$show_metquant)
      req(rv$dspedt_profiling_data[[input$sample_to_plot]][[input$signal_to_check]])


      isolate({
        temp <- rv$quantdat %>%
          dplyr::filter(paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]") == input$signal_to_check)

        req(temp$`ROI left edge (ppm)` > temp$`ROI right edge (ppm)`)

        req(temp$`Chemical shift tolerance (ppm)` <= abs(temp$`ROI right edge (ppm)` - temp$`Chemical shift(ppm)`) &
              temp$`Chemical shift tolerance (ppm)` <= abs(temp$`ROI left edge (ppm)` - temp$`Chemical shift(ppm)`))

        req((temp$`Half bandwidth (Hz)` - input$gpp_widthtolerance > 0))

        req(temp$`Multiplicity` %in% c("1", "2", "3", "4", "s", "d", "t", "q", "dd"))

        if(temp$`Multiplicity` %ni% c("1", "s")){
          req((temp$`J coupling (Hz)` - input$gpp_j_coupling_variation) > 0)

          if(temp$`J coupling 2 (Hz)` != 0){
            req((temp$`J coupling 2 (Hz)` - input$gpp_j_coupling_variation) > 0)
          }
        } else if(temp$`Multiplicity` %in% c("1", "s")){
          req(temp$`J coupling (Hz)` == 0 & temp$`J coupling 2 (Hz)` == 0)
        }
        rm(temp)
      })

      profiling_data <- rv$dspedt_profiling_data[[input$sample_to_plot]][[input$signal_to_check]]

      fmtted_signal_to_check <- rv$quantdat %>%
        dplyr::filter(paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]") == input$signal_to_check) %>%
        dplyr::mutate(SigName = paste0(.data$Metabolite, "_", .data$`Quantification Signal`)) %>% .$SigName

      # This recomputes a version of quantdat based on the (not yet saved) optimized tweaks to the signal parameters that
      # resulted from the fit check.
      # Retain only those signals that will be quantified.
      tempdf <- rv$user_reference_data %>%
        dplyr::filter(.data$Quantify == 1)
      tempdf[paste0(tempdf$Metabolite, "_", tempdf$`Quantification Signal`) %in% fmtted_signal_to_check,] <-
        rv$dspedt_user_reference_data[paste0(rv$dspedt_user_reference_data$Metabolite, "_", rv$dspedt_user_reference_data$`Quantification Signal`) %in% fmtted_signal_to_check,]


      # Merge metabolite signals whose bounds overlap into single ROIs
      tempdf <- tempdf %>% dplyr::arrange(.data$`Chemical shift(ppm)`)
      if(nrow(tempdf) > 1){
        for(i in 1:(nrow(tempdf) - 1)){
          if(tempdf$`ROI left edge (ppm)`[i] - tempdf$`ROI right edge (ppm)`[i+1] >= 0){
            # If there is an overlap, update the right end (lower value) of the subsequent, overlapped ROI to that of
            # the minimum between the two.
            tempdf$`ROI right edge (ppm)`[i+1] <- min(tempdf$`ROI right edge (ppm)`[i], tempdf$`ROI right edge (ppm)`[i+1])
            # Also update the ith right end and all others that share the same right end value to the same minimum.
            tempdf$`ROI right edge (ppm)`[which(tempdf$`ROI right edge (ppm)` == tempdf$`ROI right edge (ppm)`[i])] <-
              tempdf$`ROI right edge (ppm)`[i+1]
            # Last, update the left end (higher value) of any other ROIs with the maximum right end value among all that share the
            # updated ROI right edge
            tempdf$`ROI left edge (ppm)`[which(tempdf$`ROI right edge (ppm)` == tempdf$`ROI right edge (ppm)`[i])] <-
              max(tempdf$`ROI left edge (ppm)`[which(tempdf$`ROI right edge (ppm)` == tempdf$`ROI right edge (ppm)`[i])])
          }
        }
      }

      # Change experimental conditions of final quantification data to reflect that of the current experimental sample
      tempdf$`Frequency (MHz)`    <- ifelse(is.null(attr(xpmt_data(), "exp_info")$instrument_strength),
                                            NA, attr(xpmt_data(), "exp_info")$instrument_strength)
      tempdf$pH                   <- ifelse(is.null(attr(xpmt_data(), "exp_info")$ph),
                                            NA, attr(xpmt_data(), "exp_info")$ph)
      tempdf$`Concentration (mM)` <- ifelse(is.null(attr(xpmt_data(), "exp_info")$concentration),
                                            NA, attr(xpmt_data(), "exp_info")$concentration)
      tempdf$`Temperature (K)`    <- ifelse(is.null(attr(xpmt_data(), "exp_info")$temperature),
                                            NA, attr(xpmt_data(), "exp_info")$temperature)
      tempdf$Solvent              <- ifelse(is.null(attr(xpmt_data(), "exp_info")$solvent),
                                            NA, attr(xpmt_data(), "exp_info")$solvent)

      # End recompute version of quantdat




      roidat <- tempdf %>%
        dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]")) %>%
        dplyr::filter(.data$Signal == input$signal_to_check)
      signames <- tempdf %>%
        dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]")) %>%
        dplyr::filter(.data$`ROI left edge (ppm)` == roidat$`ROI left edge (ppm)`) %>% .$Signal

      # All information is the same provided that signals are part of the same ROI (so index doesn't matter), which they are in this case
      tempdat <- profiling_data$reproducibility_data[[1]]

      # If there are no surrounding signals, then tempdat$plot_data should have only four rows. The first
      # row is the summed intensities across all signals (target and surrounding); the second row is
      # the summed intensities across all baseline signals; the third row is the sum of the first and second
      # rows (so effectively the "generated" spectrum based on the signal and background signal fits); and the
      # fourth and subsequent rows are the fitted intensities of the specific signals in the ROI
      # Initialize plotdata
      rownames(tempdat$plot_data) <- c("All Signals", "Background", "Generated", signames)

      plotdata <- data.frame(PPM                = rep(tempdat$Xdata, times = nrow(tempdat$plot_data) - 1),
                             Intensity          = rep(tempdat$Ydata, times = nrow(tempdat$plot_data) - 1),
                             Intensity_Type     = rep(c("Generated", "Background", signames),
                                                      each = length(tempdat$Xdata)))

      # Update intensities accordingly
      for(plotdatname in rownames(tempdat$plot_data)[-1]){
        plotdata$Intensity[plotdata$Intensity_Type == plotdatname] <-
          tempdat$plot_data[rownames(tempdat$plot_data) == plotdatname, ]
      }


      ROI_plots <- plotdata
      ROI_data <- profiling_data$reproducibility_data[[1]]$ROI_profile



      tempdat <- rv$user_reference_data
      tempdat[tempdat$rowid %in% rv$dspedt_user_reference_data$rowid,] <-
        rv$dspedt_user_reference_data[rv$dspedt_user_reference_data$rowid %in% tempdat$rowid,]
      sigidx <- which(paste0(tempdat$Metabolite, " [", tempdat$`Quantification Signal`, "]") == input$signal_to_check)

      ROI_annots <- list(
        list(
          x = tempdat[sigidx,,drop = FALSE]$"Chemical shift(ppm)",
          text = paste0(sprintf("<b>%s</b>", paste0(input$signal_to_check, ": ")),
                        tempdat[sigidx,,drop = FALSE]$"Chemical shift(ppm)", " (",
                        tempdat[sigidx,,drop = FALSE]$"ROI left edge (ppm)", ", ",
                        tempdat[sigidx,,drop = FALSE]$"ROI right edge (ppm)", ")", " <br> ",
                        sprintf("<b>%s</b>", "Fitting Error: "),
                        round(profiling_data$final_output$fitting_error[1, which(grepl(make.names(fmtted_signal_to_check),
                                                                                       colnames(profiling_data$final_output$fitting_error)))],3)),
          arrowsize = tempdat[sigidx,,drop = FALSE]$"Chemical shift tolerance (ppm)",
          showarrow = TRUE
        )
      )


      xpmt_data_sample <- xpmt_data()$e_data %>% dplyr::select(.data$PPM, .data[[input$sample_to_plot]]) %>%
        dplyr::filter(.data$PPM >= ROI_data$ROI.right.edge..ppm.[1] & .data$PPM <= ROI_data$ROI.left.edge..ppm.[1])
      df_long <- xpmt_data_sample %>%
        tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")


      p <- ROI_plots %>%
        plotly::plot_ly(source = "id_prof_refmet_view_plot", type = "scatter", mode = "lines") %>%
        plotly::config(displaylogo = FALSE,
                       modeBarButtons = list(list("select2d"), list("zoom2d"), list("zoomIn2d"),
                                             list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                             list("resetScale2d"), list("toImage"))) %>%
        plotly::layout(title = paste("Experimental Data:", input$sample_to_plot, "<br>", "<sup>",
                                     "Fit of", input$signal_to_check, " and surrounding signals displayed </sup>"),
                       xaxis = list(title     = "PPM",
                                    autorange = "reversed"),
                       yaxis = list(title     = "Intensity"),
                       showlegend = TRUE,
                       dragmode = "zoom2d",
                       annotations = ROI_annots) %>%
        plotly::config(edits = list(annotationTail     = TRUE,
                                    annotationText     = FALSE,
                                    annotationPosition = FALSE)) %>%
        plotly::add_trace(x    = df_long$PPM,
                          y    = df_long$Intensity,
                          type = 'scatter',
                          mode = 'lines',
                          line = list(width = 1),
                          opacity = 0.3,
                          hoverinfo = "text",
                          text = paste0("PPM: ", round(df_long$PPM, 4), "<br>",
                                        "Intensity: ", round(df_long$Intensity, 4)),
                          showlegend = FALSE) %>%
        plotly::add_trace(x = ROI_plots$PPM,
                          y = ROI_plots$Intensity,
                          name = ~ROI_plots$Intensity_Type,
                          type = "scatter",
                          mode = "lines",
                          line = list(color = ~ROI_plots$Intensity_Type,
                                      width = 1))

      rv$dspedt_profiling_data_plot[[input$sample_to_plot]][[input$signal_to_check]] <- p

    })


    # Plotting of quantified data
    output$dspedt_profiling_data_plot <- plotly::renderPlotly({
      req(rv$dspedt_profiling_data_plot[[input$sample_to_plot]][[input$signal_to_check]])

      if(is.null(rv$dspedt_profiling_data_plot[[input$sample_to_plot]][[input$signal_to_check]])){
        return(NULL)
      }

      rv$dspedt_profiling_data_plot[[input$sample_to_plot]][[input$signal_to_check]]

    })

    observeEvent(c(rv$dspedt_profiling_data_plot, input$show_metquant),{
      req(input$show_metquant > 0)
      req(rv$dspedt_profiling_data_plot[[input$sample_to_plot]][[input$signal_to_check]])


      isolate({
        temp <- rv$quantdat %>%
          dplyr::filter(paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]") == input$signal_to_check)

        req(temp$`ROI left edge (ppm)` > temp$`ROI right edge (ppm)`)

        req(temp$`Chemical shift tolerance (ppm)` <= abs(temp$`ROI right edge (ppm)` - temp$`Chemical shift(ppm)`) &
              temp$`Chemical shift tolerance (ppm)` <= abs(temp$`ROI left edge (ppm)` - temp$`Chemical shift(ppm)`))

        req((temp$`Half bandwidth (Hz)` - input$gpp_widthtolerance > 0))

        req(temp$`Multiplicity` %in% c("1", "2", "3", "4", "s", "d", "t", "q", "dd"))

        if(temp$`Multiplicity` %ni% c("1", "s")){
          req((temp$`J coupling (Hz)` - input$gpp_j_coupling_variation) > 0)

          if(temp$`J coupling 2 (Hz)` != 0){
            req((temp$`J coupling 2 (Hz)` - input$gpp_j_coupling_variation) > 0)
          }
        } else if(temp$`Multiplicity` %in% c("1", "s")){
          req(temp$`J coupling (Hz)` == 0 & temp$`J coupling 2 (Hz)` == 0)
        }
        rm(temp)
      })


      removeModal()
      showModal(
        modalDialog(
          shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, 'dspedt_profiling_data_plot'))),
          title = "Fit Check: Fitting Error Computed Between Generated and Observed Spectra",
          size = "xl",
          easyClose = TRUE,
          fade = FALSE
        ))
    })

    output$ui_global_profiling_parameters <- renderUI({
      req(ref_data())

      shinyBS::bsCollapse(id = NS(id, "global_fitting_params"),
                          shinyBS::bsCollapsePanel(title = "Global Profiling Parameters",
                                                   # These are never used by rDolphin...not sure why they are defined
                                                   # fluidRow(
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_left_spectral_edge"),
                                                   #                       label    = "Left Spectral Edge:",
                                                   #                       value    = 12)),
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_right_spectral_edge"),
                                                   #                       label    = "Right Spectral Edge:",
                                                   #                       value    = -0.5))
                                                   # ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_widthtolerance"),
                                                                         label    = "Bandwidth Tolerance (Hz):",
                                                                         value    = 0.2)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_widthtolerance"),
                                                                        title     = "Controls the tolerance about the bandwidth of a signal. Default value is 0.2.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_gaussian"),
                                                                         label    = "Pseudo-Voigt Lineshape Gaussian Ratio (0 < ratio < 1):",
                                                                         value    = 0)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_gaussian"),
                                                                        title     = "Controls the gaussian ratio used for lineshape fitting. May be any value between zero and one. Default value is zero.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")
                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_j_coupling_variation"),
                                                                         label    = "J-coupling Tolerance (Hz):",
                                                                         value    = 0.2)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_j_coupling_variation"),
                                                                        title     = "Controls the tolerance about the measured J coupling of a signal. Default value is 0.2.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_errorprov"),
                                                                         label    = "Acceptable Fitting Error (%):",
                                                                         value    = 3)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_errorprov"),
                                                                        title     = "Minimum acceptable fitting error of optimized fit. Optimization stops when the error falls below this threshold. Default value is 3%.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_fitting_maxiter"),
                                                                         label    = "Maximum fitting parameter optimization iterations:",
                                                                         value    = 8)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_fitting_maxiter"),
                                                                        title     = "The maximum number of iterations for the algorithm to optimize fitting parameters.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     # column(width = 6,
                                                     #        numericInput(inputId  = NS(id, "gpp_nls_lm_maxiter"),
                                                     #                     label    = "Max iterations of Levenberg Marquardt (LM) algorithm:",
                                                     #                     value    = 200)),
                                                     # shinyBS::bsTooltip(id        = NS(id, "gpp_nls_lm_maxiter"),
                                                     #                    title     = "The maximum number of iterations for the Levenberg Marquardt algorithm used to optimize fitting parameters. Default value is 200.",
                                                     #                    placement = "bottom",
                                                     #                    trigger   = "hover")

                                                   ),
                                                   # fluidRow(
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_ftol"),
                                                   #                       label    = "LM algorithm sum of squares error threshold:",
                                                   #                       value    = 1e-06)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_ftol"),
                                                   #                      title     = "Minimum sum of squared errors threshold for LM algorithm. Default value is 0.000001.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover"),
                                                   #
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_ptol"),
                                                   #                       label    = "LM algorithm relative error threshold:",
                                                   #                       value    = 1e-06)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_ptol"),
                                                   #                      title     = "Minimum relative error threshold for LM algorithm. Default value is 0.000001.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover")
                                                   #
                                                   # ),
                                                   # fluidRow(
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_factor"),
                                                   #                       label    = "LM algorithm control factor:",
                                                   #                       value    = 0.01)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_factor"),
                                                   #                      title     = "Control parameter used to determine the initial step bound for the LM algorithm. Default value is 0.01.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover"),
                                                   #
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_fitting_maxiterrep"),
                                                   #                       label    = "fitting_maxiterrep:",
                                                   #                       value    = 0)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_fitting_maxiterrep"),
                                                   #                      title     = "Default value is zero.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover")
                                                   #
                                                   # ),
                                                   # fluidRow(
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_peakdet_minimum"),
                                                   #                       label    = "peakdet_minimum:",
                                                   #                       value    = 0.01)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_peakdet_minimum"),
                                                   #                      title     = "Default value is 0.01.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover")
                                                   #
                                                   #   # These only apply to the "fitting error / signal area ratio analyses"
                                                   #   # performed in the rDolphin GUI. We do not implement them, so these
                                                   #   # parameters are unnecessary.
                                                   #   # column(width = 6,
                                                   #   #        selectInput(inputId   = NS(id, "gpp_automatic_removal"),
                                                   #   #                    label     = "automatic_removal:",
                                                   #   #                    choices   = c("Yes" = "Y", "No" = "N"),
                                                   #   #                    selected  = "Y"))
                                                   # ),
                                                   # fluidRow(
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_fitting_error_analysis_limit"),
                                                   #                       label    = "fitting_error_analysis_limit:",
                                                   #                       value    = 0.15)),
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_signal_area_ratio_analysis_limit"),
                                                   #                       label    = "signal_area_ratio_analysis_limit:",
                                                   #                       value    = 10))
                                                   # ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_BGdensity"),
                                                                         label    = "Background Signal (BGS) Density:",
                                                                         value    = 70)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_BGdensity"),
                                                                        title     = "Default value is 70.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_BG_gaussian_percentage"),
                                                                         label    = "BGS Pseudo-Voigt Lineshape Gaussian Ratio (0 < ratio < 1):",
                                                                         value    = 0)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_BG_gaussian_percentage"),
                                                                        title     = "Controls the gaussian ratio used for lineshape fitting of background signals. May be any value between zero and one. Default value is zero.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_BG_width"),
                                                                         label    = "BGS Bandwidth (Hz):",
                                                                         value    = 8)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_BG_width"),
                                                                        title     = "The bandwidth applied to all background signals. Default value is eight.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_BG_width_tolerance"),
                                                                         label    = "BGS Bandwidth Tolerance (Hz):",
                                                                         value    = 0.25)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_BG_width_tolerance"),
                                                                        title     = "Controls the tolerance about the bandwidth of a background signal. Default value is 0.25.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),

                                                   # fluidRow(
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_additional_signal_ppm_distance"),
                                                   #                       label    = "additional_signal_ppm_distance:",
                                                   #                       value    = 0.002)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_additional_signal_ppm_distance"),
                                                   #                      title     = "Default value is 0.002.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover"),
                                                   #
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_signals_to_add"),
                                                   #                       label    = "signals_to_add:",
                                                   #                       value    = 2)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_signals_to_add"),
                                                   #                      title     = "Default value is two.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover")
                                                   #
                                                   # ),
                                                   #
                                                   # fluidRow(
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_additional_signal_improvement"),
                                                   #                       label    = "additional_signal_improvement:",
                                                   #                       value    = 0.75)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_additional_signal_improvement"),
                                                   #                      title     = "Default value is 0.75.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover"),
                                                   #
                                                   #   column(width = 6,
                                                   #          numericInput(inputId  = NS(id, "gpp_additional_signal_percentage_limit"),
                                                   #                       label    = "additional_signal_percentage_limit:",
                                                   #                       value    = 3)),
                                                   #   shinyBS::bsTooltip(id        = NS(id, "gpp_additional_signal_percentage_limit"),
                                                   #                      title     = "Default value is three.",
                                                   #                      placement = "bottom",
                                                   #                      trigger   = "hover")
                                                   #
                                                   # ),
                                                   style = "primary"
                          ))
    })

    #----------------------------------------------------------------------------------------------------------

    # Misc ----------------------------------------------------------------------------------------------------

    # Note for Auto-optimization
    output$autoptim_note <- renderUI({

      req(input$auto_optim)

      htmltools::HTML("<strong>Note:</strong> When toggled on, auto-optimization automatically tweaks the chemical shift, half bandwidth, and J-coupling value(s) subject to the tolerances set for each parameter. \n This automatic parameter adjustment occurs whenever 'Check Signal Fit' is pressed.")
    })

    # Observer to measure distances with box select
    output$distance_text <- renderUI({

      brushedData <- plotly::event_data("plotly_brushed", source = "id_refmet_dspedt_selected_plot")
      if(is.null(brushedData)){
        return(NULL)
      } else{
        distppm <- abs(diff(brushedData$x))
        disthz  <- distppm*attr(xpmt_data(), "exp_info")$instrument_strength
        intensity_diff <- abs(diff(brushedData$y))
        htmltools::HTML(paste0("<strong>Difference (ppm):</strong> ", round(distppm, 3), "<br/>",
                               "<strong>Difference (Hz):</strong> ", round(disthz, 3), "<br/>",
                               "<strong>Intensity Difference:</strong> ", round(intensity_diff, 3)))
      }
    })

    # Creates a datatable that displays reference database
    output$refmet_database <- DT::renderDataTable({

      bmse_associations %>%
        dplyr::rename(`BMRB ID` = .data$Entry_ID,
                      `CAS No.` = .data$CASno,
                      `Spectrometer Frequency (MHz)` = .data$Field_strength,
                      `Metabolite` = .data$Solute) %>%
        DT::datatable(rownames   = FALSE,
                      options = list(scrollX = TRUE))
    })

    # Options for metabolite signals
    output$ui_metabolite_signal_options <- renderUI({
      req(ref_data())

      shinyBS::bsCollapse(id = NS(id, "metabolite_signal_options"),
                          shinyBS::bsCollapsePanel(title = "Metabolite Signal Options",

                                                   fluidRow(
                                                     column(width = 2,
                                                            actionButton(NS(id, "signal_add"), "Add New Signal")),
                                                     column(width = 3,
                                                            uiOutput(NS(id, "ui_remove_signal"))),
                                                     column(width = 3,
                                                            shinyWidgets::materialSwitch(
                                                              inputId = NS(id, "set_metbwidth"),
                                                              label = "Set Signal-Wide Half Bandwidth",
                                                              status = "primary",
                                                              value = FALSE,
                                                              inline = TRUE,
                                                              right = TRUE
                                                            )),
                                                     column(width = 3,
                                                            uiOutput(NS(id, "ui_set_metbwidth")))
                                                   ),
                                                   h4(""),
                                                   fluidRow({
                                                     column(width = 4,
                                                            shinyWidgets::materialSwitch(
                                                              inputId = NS(id, "display_fulldat"),
                                                              label = "Display All Reference Entries",
                                                              status = "primary",
                                                              value = FALSE,
                                                              inline = TRUE,
                                                              right = TRUE
                                                            ))
                                                   }),
                                                   h4(""),
                                                   uiOutput(NS(id, "ui_fulldat_table")),
                                                   style = "primary"
                          ))
    })

    # Dynamically displaying table containing all entries for a given reference metabolite
    output$ui_fulldat_table <- renderUI({
      req(input$display_fulldat)
      req(rv$full_reference_data)
      req(input$which_refmet_dspedt)

      rv$full_reference_data %>% dplyr::ungroup() %>%
        dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
        dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"),
                      `Signal left edge (ppm)` = .data$`ROI left edge (ppm)`,
                      `Signal right edge (ppm)` = .data$`ROI right edge (ppm)`) %>%
        dplyr::arrange(dplyr::desc(.data$`Quantification Signal`)) %>%
        dplyr::select(.data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`,
                      .data$`Solvent`, .data$Signal, .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                      .data$`Signal left edge (ppm)`,
                      .data$`Signal right edge (ppm)`, .data$`Half bandwidth (Hz)`, .data$Multiplicity, .data$`J coupling (Hz)`,
                      .data$`J coupling 2 (Hz)`, .data$`Roof effect`, .data$`Roof effect 2`) %>%
        DT::datatable(rownames   = FALSE,
                      editable   = FALSE,
                      filter     = "top",
                      options = list(scrollX = TRUE,
                                     paging = TRUE,
                                     pageLength = 5)) %>%
        DT::formatRound(columns = c("Chemical shift(ppm)", "Chemical shift tolerance (ppm)", "Signal left edge (ppm)", "Signal right edge (ppm)",
                                    "Half bandwidth (Hz)", "J coupling (Hz)", "J coupling 2 (Hz)", "Roof effect",
                                    "Roof effect 2", "Frequency (MHz)", "pH", "Concentration (mM)", "Temperature (K)"),
                        digits = 3)


    })

    # Dynamically displaying text box for signal-wide halfbandwidth
    output$ui_set_metbwidth <- renderUI({
      req(input$set_metbwidth)

      textInput(inputId  = NS(id, "sigwide_bwidth"),
                label    = "Half Bandwidth (Hz):",
                value    = "")

    })

    # Remove (Most Recent) Added Signal
    output$ui_remove_signal <- renderUI({
      req(rv$user_reference_data)

      temp <- rv$full_reference_data %>%
        dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt, !duplicated(.data$`Quantification Signal`))
      req(nrow(rv$user_reference_data %>% dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt)) >
            nrow(temp))

      actionButton(NS(id, "signal_remove"), "Remove Last Added Signal")
    })

    # Add Signal
    observeEvent(c(input$signal_add), priority = 1,
                 {
                   req(ref_data())
                   req(xpmt_data())
                   req(input$which_refmet_dspedt)
                   req(input$signal_add > 0)

                   # Count the number of existing signals for the given metabolite
                   numSigs <- nrow(rv$user_reference_data %>% dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt))

                   added_entry_data <- data.frame(`ROI left edge (ppm)`            = rep(0.02, 1),
                                                  `ROI right edge (ppm)`           = rep(-0.02, 1),
                                                  `Quantification Mode`            = rep("Baseline Fitting", 1),
                                                  `Metabolite`                     = input$which_refmet_dspedt,
                                                  `Quantification Signal`          = rep(numSigs + 1, 1),
                                                  `Chemical shift(ppm)`            = rep(0, 1),
                                                  `Chemical shift tolerance (ppm)` = rep(0.005, 1),
                                                  `Half bandwidth (Hz)`            = rep(1.4, 1),
                                                  `Multiplicity`                   = rep("1", 1),
                                                  `J coupling (Hz)`                = rep(0, 1),
                                                  `Roof effect`                    = rep(0, 1),
                                                  `J coupling 2 (Hz)`              = rep(0, 1),
                                                  `Roof effect 2`                  = rep(0, 1),
                                                  `Quantify`                       = rep(1, 1),
                                                  `Frequency (MHz)`                = ifelse(is.null(attr(xpmt_data(), "exp_info")$instrument_strength),
                                                                                            NA, attr(xpmt_data(), "exp_info")$instrument_strength),
                                                  `pH`                             = ifelse(is.null(attr(xpmt_data(), "exp_info")$ph),
                                                                                            NA, attr(xpmt_data(), "exp_info")$ph),
                                                  `Concentration (mM)`             = ifelse(is.null(attr(xpmt_data(), "exp_info")$concentration),
                                                                                            NA, attr(xpmt_data(), "exp_info")$concentration),
                                                  `Temperature (K)`                = ifelse(is.null(attr(xpmt_data(), "exp_info")$temperature),
                                                                                            NA, attr(xpmt_data(), "exp_info")$temperature),
                                                  `Solvent`                        = ifelse(is.null(attr(xpmt_data(), "exp_info")$solvent),
                                                                                            NA, attr(xpmt_data(), "exp_info")$solvent),
                                                  `rowid`                          = paste0(input$which_refmet_dspedt, numSigs + 1),
                                                  check.names = FALSE)

                   rv$user_reference_data <- dplyr::bind_rows(rv$user_reference_data, added_entry_data)
                   rv$unedited_bestmatch_ref_data <- dplyr::bind_rows(rv$unedited_bestmatch_ref_data, added_entry_data)

                   # reference metabolite copy
                   rv$dspedt_user_reference_data <- rv$user_reference_data %>%
                     dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt)

                   # clear any unsaved changes
                   rv$unsaved_change <- list()

                   ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                              pltproxy = refmet_dspedt_plot_proxy,
                                              newdat = rv$dspedt_user_reference_data)


                 })

    # Remove Last Added Signal
    observeEvent(c(input$signal_remove), priority = 1,
                 {
                   req(ref_data())
                   req(xpmt_data())
                   req(input$which_refmet_dspedt)
                   req(input$signal_remove > 0)
                   req(nrow(rv$user_reference_data %>% dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt)) >
                         nrow(ref_data()$bestmatch_ref_data %>% dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt)))
                   # Count the number of existing signals for the given metabolite
                   numSigs <- nrow(rv$user_reference_data %>% dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt))

                   rv$user_reference_data <- rv$user_reference_data %>% dplyr::filter(!(.data$Metabolite %in% input$which_refmet_dspedt &
                                                                                        .data$`Quantification Signal` == numSigs))
                   rv$unedited_bestmatch_ref_data <- rv$unedited_bestmatch_ref_data %>% dplyr::filter(!(.data$Metabolite %in% input$which_refmet_dspedt &
                                                                                                                .data$`Quantification Signal` == numSigs))

                   # reference metabolite copy
                   rv$dspedt_user_reference_data <- rv$user_reference_data %>%
                     dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt)

                   # clear any unsaved changes
                   rv$unsaved_change <- list()

                   ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                              pltproxy = refmet_dspedt_plot_proxy,
                                              newdat = rv$dspedt_user_reference_data)
                 })

    # Set the specified halfbandwidth for all signals of the metabolite
    observeEvent(c(input$sigwide_bwidth),{
      req(input$set_metbwidth)
      req(input$sigwide_bwidth)

      newbw <- as.numeric(input$sigwide_bwidth)

      if(!is.na(newbw)){
        rv$dspedt_user_reference_data[["Half bandwidth (Hz)"]] <- newbw

        ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                   pltproxy = refmet_dspedt_plot_proxy,
                                   newdat = rv$dspedt_user_reference_data)

        # Store the unsaved changes
        rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data
      } else{
        shinyWidgets::show_alert(
          title = "Invalid Entry.",
          text = "The supplied value must be numeric.",
          type = "error"
        )
      }


    })

    #----------------------------------------------------------------------------------------------------------

    # Module output
    reactive({
      req(rv$user_reference_data)
      gpps <- list(BGdensity                          = input$gpp_BGdensity,
                   widthtolerance                     = input$gpp_widthtolerance,
                   gaussian                           = input$gpp_gaussian,
                   j_coupling_variation               = input$gpp_j_coupling_variation,
                   BG_gaussian_percentage             = input$gpp_BG_gaussian_percentage,
                   BG_width                           = input$gpp_BG_width,
                   BG_width_tolerance                 = input$gpp_BG_width_tolerance,
                   errorprov                          = input$gpp_errorprov,
                   fitting_maxiter                    = input$gpp_fitting_maxiter,
                   nls_lm_maxiter                     = 200, # from rDolphin defaults
                   ftol                               = 1e-06, # from rDolphin defaults
                   ptol                               = 1e-06, # from rDolphin defaults
                   factor                             = 0.01, # from rDolphin defaults
                   additional_signal_ppm_distance     = 0.002, # from rDolphin defaults
                   signals_to_add                     = 2, # from rDolphin defaults
                   fitting_maxiterrep                 = 0, # from rDolphin defaults
                   additional_signal_improvement      = 0.75, # from rDolphin defaults
                   additional_signal_percentage_limit = 3, # from rDolphin defaults
                   peakdet_minimum                    = 0.01) # from rDolphin defaults

      isolate({
        ## Document all changes relative to original reference data (ref_data()$ref_data)
        orig_refdat <- ref_data()$bestmatch_ref_data
        # Note also that Metabolite$[[1]] in all rv$refchanges entries correspond to the original data for Metabolite

        # Document all metabolites that were added or removed
        added_metabolites <- unique(rv$user_reference_data$Metabolite)[unique(rv$user_reference_data$Metabolite) %ni% unique(orig_refdat$Metabolite)]
        removed_metabolites <- unique(orig_refdat$Metabolite)[unique(orig_refdat$Metabolite) %ni% unique(rv$user_reference_data$Metabolite)]
        attr(rv$user_reference_data, "added_metabolites") <- added_metabolites
        attr(rv$user_reference_data, "removed_metabolites") <- removed_metabolites

        # Distill refchanges to include only the original entry, and subsequent rows/cols that were changed for
        # each metabolite.
        tempnames <- names(rv$refchanges)

        allres <- vector("list", length = length(tempnames))
        names(allres) <- tempnames
        for(name in tempnames){
          allres[[name]] <- list(OriginalEntry = orig_refdat %>% dplyr::filter(.data$Metabolite %in% name),
                                 FinalEntry    = rv$user_reference_data %>% dplyr::filter(.data$Metabolite == name))
        }
        attr(rv$user_reference_data, "edit_history") <- allres

        # Change experimental conditions of final quantification data to reflect that of the current experimental sample
        # This is already done for quantdat when it is created
        list(user_edited_refdata = rv$user_reference_data %>%
               dplyr::mutate(`Frequency (MHz)`    = ifelse(is.null(attr(xpmt_data(), "exp_info")$instrument_strength),
                                                           NA, attr(xpmt_data(), "exp_info")$instrument_strength),
                             pH                   = ifelse(is.null(attr(xpmt_data(), "exp_info")$ph),
                                                           NA, attr(xpmt_data(), "exp_info")$ph),
                             `Concentration (mM)` = ifelse(is.null(attr(xpmt_data(), "exp_info")$concentration),
                                                           NA, attr(xpmt_data(), "exp_info")$concentration),
                             `Temperature (K)`    = ifelse(is.null(attr(xpmt_data(), "exp_info")$temperature),
                                                           NA, attr(xpmt_data(), "exp_info")$temperature),
                             Solvent              = ifelse(is.null(attr(xpmt_data(), "exp_info")$solvent),
                                                           NA, attr(xpmt_data(), "exp_info")$solvent)),
             quantdata           = rv$quantdat,
             global_parameters   = gpps)
        })
      })
    })
}
