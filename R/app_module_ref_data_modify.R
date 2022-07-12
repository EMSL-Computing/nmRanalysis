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
      )
    ),
    h5(tags$b("Select a Signal:")),
    uiOutput(ns("fitcheck")),
    uiOutput(ns("ui_global_profiling_parameters")),
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
        width = 2,
        h4("Specify solute(s) to add:")
      ),
      column(
        width = 2,
        uiOutput(ns("ui_refmet_add_options"))
      ),
      column(
        width = 1,
        actionButton(ns("refmet_add"), "Add")
      )
    ),
    fluidRow(
      column(
        width = 2,
        h4("Specify solute(s) to remove:")
      ),
      column(
        width = 2,
        uiOutput(ns("ui_refmet_remove_options"))
      ),
      column(
        width = 1,
        actionButton(ns("refmet_remove"), "Remove")
      )
    ),
    DT::dataTableOutput(ns("refmet_database"))
  )
}

#' Module: UI element displaying target metabolite(s) to be quantified
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
#' This module component displays the set of target metabolite data that will be used for profiling. These data reflect any
#' and all edits made to the target metabolite data and are displayed within a searchable, filterable table.
#'
#' @import shiny
#'
ref_data_quantTab <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui_auto_profile")),
    uiOutput(ns("vizoptions_quantdata_ui")),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns('quantdata_plot'))),
    DT::dataTableOutput(ns("refmet_quant_table"))
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
ref_data_editingServer <- function(id, xpmt_data, ref_data, ref_db){
  stopifnot(is.reactive(xpmt_data))
  stopifnot(is.reactive(ref_data))
  stopifnot(!is.reactive(ref_db))
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
                         obs_annot_update_subplot_suspend = TRUE)

    observe(priority = 1, {
      req(ref_data())

      rv$user_reference_data <- ref_data()$ref_data
    })

    # Observer to populate refmet choices after reference metabolites have been uploaded/specified/added/removed
    observeEvent(c(input$refmet_add, input$refmet_remove, ref_data()$ref_data),
                 {
                   req(rv$user_reference_data)

                   updateSelectizeInput(inputId = "which_refmet_dspedt",
                                        choices = unique(rv$user_reference_data$Metabolite))
                 })

    # Add/Delete Reference Metabolites ---------------------------------

    # Observer to add specified reference metabolite(s) to the set of reference metabolites already under consideration
    observeEvent(c(input$refmet_add),
                 {
                   req(ref_data())
                   req(xpmt_data())

                   # specify list object on the user provided metabolite names
                   added.refchoices <- as.list(input$refmet_toadd)

                   # create an ROI reference object using nmRanalysis to be rendered as a table in the UI
                   added_reference_data <- roi_ref_export(name_list           = added.refchoices,
                                                          solvent_type        = attr(xpmt_data(), "exp_info")$solvent,
                                                          ph                  = attr(xpmt_data(), "exp_info")$ph,
                                                          instrument_strength = attr(xpmt_data(), "exp_info")$instrument_strength)

                   shinyFeedback::feedbackDanger("refmet_toadd",
                                                 nrow(added_reference_data) == 0,
                                                 "No ROI data available.")
                   req(nrow(added_reference_data) > 0)

                   added_reference_data <- added_reference_data %>% dplyr::group_by(.data$Metabolite) %>%
                     dplyr::mutate(Quantify = 1,
                                   rowid = paste0(.data$Metabolite, dplyr::row_number()),
                                   Multiplicity = as.character(.data$`Multiplicity`),
                                   `J coupling 2 (Hz)` = 0,
                                   `Roof effect 2` = 0)

                   rv$user_reference_data <- dplyr::bind_rows(rv$user_reference_data, added_reference_data)

                   updateSelectizeInput(session,
                                        "which_refmet_dspedt",
                                        "Select Reference Metabolite(s) to Display/Edit:",
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

                   # Also, remove stored changes and counter (from refchanges, change_counter) corresponding to removed metabolite
                   rv$refchanges <- rv$refchanges[names(rv$refchanges) %ni% input$refmet_toremove]
                   rv$change_counter <- rv$change_counter[names(rv$change_counter) %ni% input$refmet_toremove]

                   updateSelectizeInput(session,
                                        "which_refmet_dspedt",
                                        "Select Reference Metabolite(s) to Display/Edit:",
                                        choices = unique(rv$user_reference_data$Metabolite))
                 })

    # UI element for the options of adding of reference metabolites
    output$ui_refmet_add_options <- renderUI({

      req(ref_data())

      addchoices <- setdiff(unique(ref_db$Solute), unique(rv$user_reference_data$Metabolite))
      selectizeInput(NS(id, "refmet_toadd"),
                     label = NULL,
                     choices = addchoices, multiple = TRUE)
    })

    # UI element for the options of removal of reference metabolites
    output$ui_refmet_remove_options <- renderUI({

      req(ref_data())

      selectizeInput(NS(id, "refmet_toremove"),
                     label = NULL,
                     choices = unique(rv$user_reference_data$Metabolite), multiple = TRUE)
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

                   # Line shape update
                   ROI_lines <- ROI_line_gen(data = rv$dspedt_user_reference_data)

                   # Annotation update
                   ROI_annots <- ROI_annot_gen(data = rv$dspedt_user_reference_data)

                   # Update plot
                   plotly::plotlyProxyInvoke(refmet_dspedt_plot_proxy, "relayout",
                                             list(title = paste("Experimental Data:", input$sample_to_plot, "<br>", "<sup>",
                                                                input$which_refmet_dspedt, "Peak Location(s) displayed", "</sup>"),
                                                  annotations = ROI_annots,
                                                  shapes = ROI_lines))
                 })

    # This datatable corresponds to the selected reference metabolite data to display/edit
    output$refmet_dspedt_table <- DT::renderDT({

      req(ref_data())
      req(input$which_refmet_dspedt)

      isolate({
        # Note: Remove quantification mode column, but allow users to specify quantification mode on a per-ROI basis on the
        # separate subtab (Reference Data for Quantification) where reference data are grouped by ROIs
        rv$user_reference_data %>% dplyr::ungroup() %>%
          dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
          dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"),
                        `Signal left edge (ppm)` = .data$`ROI left edge (ppm)`,
                        `Signal right edge (ppm)` = .data$`ROI right edge (ppm)`) %>%
          dplyr::select(.data$Signal, .data$Quantify, .data$`Chemical shift(ppm)`, .data$`Signal left edge (ppm)`,
                        .data$`Signal right edge (ppm)`, .data$`Half bandwidth (Hz)`, .data$Multiplicity, .data$`J coupling (Hz)`,
                        .data$`J coupling 2 (Hz)`, .data$`Roof effect`, .data$`Roof effect 2`) %>%
          DT::datatable(rownames   = FALSE,
                        editable   = TRUE,
                        selection = "none")

      })
    })

    # Create proxy for the refmet datatable to more fluidly interact with refmet plot and table edits.
    refmet_dspedt_table_proxy <- DT::dataTableProxy('refmet_dspedt_table')

    # Plotly plot of selected rows. Rendered once corresponding action button is pressed
    output$refmet_dspedt_selected_plot <- plotly::renderPlotly({

      isolate({
        req(ref_data())
        req(xpmt_data())
      })

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

    # Create proxy for the above plotly plot to more fluidly interact with refmet plot and table edits.
    refmet_dspedt_plot_proxy <- plotly::plotlyProxy("refmet_dspedt_selected_plot")

    # This observer is responsible for plotting the trace (i.e. line) corresponding to a selected
    # experimental spectrum. This is implemented through proxy updates for the sake of efficiency.
    observeEvent(c(input$sample_to_plot, xpmt_data()), priority = -1, {
      req(input$sample_to_plot)
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
                       choices = unique(ref_data()$ref_data$Metabolite)),

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

      removeModal()
      showModal(
        modalDialog(
          shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, 'refmet_dspedt_selected_subplot'))),
          title = paste0("All Sample Spectra: ", round(min(brushedData$x),3)," PPM to ", round(max(brushedData$x),3), " PPM"),
          size = "xl",
          easyClose = TRUE,
          fade = FALSE
        ))
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
      req(rv$change_counter[[input$which_refmet_dspedt]] > 1)

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
                     dplyr::select(.data$Signal, .data$Quantify, .data$`Chemical shift(ppm)`, .data$`Signal left edge (ppm)`,
                                   .data$`Signal right edge (ppm)`, .data$`Half bandwidth (Hz)`, .data$Multiplicity, .data$`J coupling (Hz)`,
                                   .data$`J coupling 2 (Hz)`, .data$`Roof effect`, .data$`Roof effect 2`)


                   edtd_colname <- names(temp)[changed_col]

                   if(edtd_colname %in% c("Chemical shift(ppm)", "Signal left edge (ppm)", "Signal right edge (ppm)")) {

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

                   } else if(edtd_colname %in% c("Half bandwidth (Hz)", "J coupling (Hz)",
                                                 "J coupling 2 (Hz)", "Roof effect", "Roof effect 2")) {

                     rv$dspedt_user_reference_data[[edtd_colname]][changed_row] <- as.numeric(v)

                     # Store the unsaved changes
                     rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data

                   } else if(edtd_colname %in% c("Multiplicity")){

                     # multiplicity should remain character-valued so as to allow for specifications like "dd", "s", and so forth.
                     rv$dspedt_user_reference_data[[edtd_colname]][changed_row] <- v

                     # Store the unsaved changes
                     rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data

                   } else if(edtd_colname == "Quantify"){

                     if(v %ni% c("0", "1")){
                       shinyWidgets::show_alert(
                         title = "Invalid entry.",
                         text = "Specify 0 to exclude the corresponding peak from quantification; 1 otherwise.",
                         type = "error"
                       )
                     }

                     req(v %in% c("0", "1"))

                     rv$dspedt_user_reference_data[[edtd_colname]][changed_row] <- as.numeric(v)

                     ProxyUpdate_refmet_tabplot(tabproxy = refmet_dspedt_table_proxy,
                                                pltproxy = refmet_dspedt_plot_proxy,
                                                newdat = rv$dspedt_user_reference_data)

                     # Store the unsaved changes
                     rv$unsaved_change[[input$which_refmet_dspedt]] <- rv$dspedt_user_reference_data

                   } else {
                     shinyWidgets::show_alert(
                       title = "Non-editable Field.",
                       text = "Changes made to this field are not saved.",
                       type = "error"
                     )
                   }
                 })

    #----------------------------------------------------------------------------------------------------------

    # Quantification Data, Fit Check, and Profiling Trigger ---------------------------------------------------------------

    # Define selection options for quantification checks to be one of the signals for the selected metabolite
    output$fitcheck <- renderUI({

      req(input$which_refmet_dspedt)
      req(ref_data())
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
          width = 1,
          actionButton(NS(id, "show_metquant"), label = "Check Signal Fit")
        )
      )

    })

    # Update selection options for quantification checks to be one of the signals for the selected metabolite
    observeEvent(c(input$which_refmet_dspedt), ignoreNULL = TRUE, ignoreInit = TRUE,
                 {
                   req(ref_data())
                   req(rv$user_reference_data)

                   temp <- rv$user_reference_data %>% dplyr::ungroup() %>%
                     dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
                     dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"))

                   updateSelectInput(inputId = NS(id, "signal_to_check"),
                                     choices = temp$Signal)
                 })

    # Defines new, "collapsed" or "merged" ROIs to be used for quantification.
    observe({
      req(ref_data())
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

      # Create new reactive value to store the ROI-collapsed data
      rv$quantdat <- tempdf
    })

    # This datatable corresponds to the reference metabolite data to be used for quantification
    output$refmet_quant_table <- DT::renderDT({

      req(ref_data())
      req(rv$user_reference_data)
      req(rv$quantdat)
      req(input$ROI_to_plot_quantdat)


      temp <- rv$quantdat %>%
        dplyr::mutate(ROI = paste0("(", .data$`ROI right edge (ppm)`, ", ", .data$`ROI left edge (ppm)`, ")")) %>%
        dplyr::filter(ROI %in% input$ROI_to_plot_quantdat)
      temp2 <- rv$user_reference_data %>% dplyr::ungroup() %>%
        dplyr::filter(.data$`Chemical shift(ppm)` %in% temp$`Chemical shift(ppm)`) %>%
        dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]")) %>%
        dplyr::filter(.data$Quantify == 1) %>%
        dplyr::arrange(.data$`Chemical shift(ppm)`) %>%
        dplyr::select(.data$Signal, .data$`Quantification Mode`, .data$`Chemical shift(ppm)`, .data$`Chemical shift tolerance (ppm)`,
                      .data$`Half bandwidth (Hz)`, .data$Multiplicity, .data$`J coupling (Hz)`, .data$`J coupling 2 (Hz)`,
                      .data$`Roof effect`, .data$`Roof effect 2`)

      temp2 %>%
        DT::datatable(rownames   = FALSE,
                      editable   = FALSE,
                      filter = "top",
                      extensions = "Responsive")
    })

    # Output (in HTML format) to display the filters that are currently applied to the data.
    output$applied_filters_text2 <- renderUI({

      if(length(attr(xpmt_data(), "filters")) == 0){
        htmltools::HTML("<strong>Currently applied filters:</strong><br/>None")

      } else{
        allfilts <- rlist::list.ungroup(rlist::list.select(attr(xpmt_data(), "filters"), range))
        allfilts <- Reduce("c", lapply(allfilts, function(x){paste0("(", x$min, ", ", x$max, ")")}))
        htmltools::HTML(paste0("<strong>Currently applied filters:</strong><br/>", paste(allfilts, collapse = "<br/>")))

      }
    })

    # Visualization options for the plot of ROI-collapsed quantification data
    output$vizoptions_quantdata_ui <- renderUI({

      req(xpmt_data())
      req(ref_data())
      req(rv$quantdat)

      ROIvec <- unique(paste0("(", rv$quantdat$`ROI right edge (ppm)`, ", ", rv$quantdat$`ROI left edge (ppm)`, ")"))

      shinyWidgets::dropdownButton(
        # Allows users to select which sample spectrum to display.
        selectInput(inputId = NS(id, "sample_to_plot_quantdat"),
                    label   = "Choose a spectrum to plot",
                    choices = colnames(xpmt_data()$e_data)[-1]),

        selectInput(inputId = NS(id, "ROI_to_plot_quantdat"),
                    label   = "Select a region of interest whose fitting data should be displayed.",
                    choices = ROIvec),

        # HTML output to display the filters currently applied
        htmlOutput(NS(id,"applied_filters_text2")),

        # Omit for now
        # # Toggle for subplot display
        # shinyWidgets::materialSwitch(inputId = NS(id, "show_subplot_quantdat"),
        #                              label   = "Show subplot on box select",
        #                              value   = FALSE,
        #                              status  = "primary",
        #                              right   = TRUE),

        circle = TRUE, status = "info",
        icon = icon("cog"), width = "300px",

        tooltip = shinyWidgets::tooltipOptions(title = "Plot Options")
      )
    })

    # Plot all collapsed ROIs over the spectrum
    output$quantdata_plot <- plotly::renderPlotly({

      isolate({
        req(ref_data())
        req(xpmt_data())
      })
      # req(rv$quantdat)
      # req(input$ROI_to_plot_quantdat)

      # # Generate line shapes based on collapsed ROIs
      # ROI_lines <- ROI_line_gen(data = rv$quantdat[!duplicated(rv$quantdat$`ROI left edge (ppm)`),])
      # ROI_annots <- ROI_annot_gen(data = rv$user_reference_data)

      plotly::plot_ly(source = "id_quantdata_plot", type = "scatter", mode = "lines") %>%
        plotly::config(displaylogo = FALSE,
                       modeBarButtons = list(list("select2d"), list("zoom2d"), list("zoomIn2d"),
                                             list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                             list("resetScale2d"), list("toImage"))) %>%
        plotly::layout(title = paste("Experimental Data:", "<br>", "<sup>",
                                     "Metabolite Peaks within Selected Region(s) of Interest (ROI) Annotated", "</sup>"),
                       xaxis = list(title     = "PPM",
                                    autorange = "reversed"),
                       yaxis = list(title     = "Intensity"),
                       dragmode = "zoom2d") %>% #,
        # annotations = ROI_annots,
        # shapes = ROI_lines) %>%
        plotly::config(edits = list(annotationTail     = TRUE,
                                    annotationText     = FALSE,
                                    annotationPosition = FALSE,
                                    shapePosition      = FALSE))

    })

    # Create proxy for the above plotly plot for improved efficiency
    quantdata_plot_proxy <- plotly::plotlyProxy("quantdata_plot")

    # This observer is responsible for plotting the trace (i.e. line) corresponding to a selected
    # experimental spectrum. This is implemented through proxy updates for the sake of efficiency.
    observeEvent(c(input$sample_to_plot_quantdat, xpmt_data()), priority = -1, {
      req(input$sample_to_plot_quantdat)

      xpmt_data_sample <- xpmt_data()$e_data %>% dplyr::select(.data$PPM, .data[[input$sample_to_plot_quantdat]])
      df_long <- xpmt_data_sample %>%
        tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")

      # Clear shapes and annotations
      plotly::plotlyProxyInvoke(quantdata_plot_proxy, "relayout",
                                list(annotations = NULL,
                                     shapes = NULL))

      # Clear plots
      plotly::plotlyProxyInvoke(quantdata_plot_proxy, "deleteTraces", as.list(as.integer(0)))

      plotly::plotlyProxyInvoke(quantdata_plot_proxy, "addTraces",
                                list(x    = df_long$PPM,
                                     y    = df_long$Intensity,
                                     type = 'scatter',
                                     mode = 'lines',
                                     line = list(width = 1),
                                     hoverinfo = "text",
                                     text = paste0("PPM: ", round(df_long$PPM, 4), "<br>",
                                                   "Intensity: ", round(df_long$Intensity, 4))))
      plotly::plotlyProxyInvoke(quantdata_plot_proxy, "relayout",
                                list(title = paste("Experimental Data:", input$sample_to_plot_quantdat, "<br>", "<sup>",
                                                   "Metabolite Peaks within Selected Region(s) of Interest (ROI) Annotated", "</sup>")))
      # Update shapes/annotations
      temp <- rv$quantdat %>%
        dplyr::mutate(ROI = paste0("(", .data$`ROI right edge (ppm)`, ", ", .data$`ROI left edge (ppm)`, ")")) %>%
        dplyr::filter(ROI %in% input$ROI_to_plot_quantdat)
      temp2 <- rv$user_reference_data %>% dplyr::filter(.data$`Chemical shift(ppm)` %in% temp$`Chemical shift(ppm)`)

      ROI_lines <- ROI_line_gen(data = temp[!duplicated(temp$`ROI left edge (ppm)`),,drop = FALSE])
      ROI_annots <- ROI_annot_gen(data = temp2)

      # Update plot
      plotly::plotlyProxyInvoke(quantdata_plot_proxy, "relayout",
                                list(annotations = ROI_annots,
                                     shapes = ROI_lines))
    })

    # This observer is responsible for plotting the annotations and shapes for entries within the
    # specified ROI. This is implemented through proxy updates for the sake of efficiency.
    observeEvent(c(input$ROI_to_plot_quantdat), ignoreNULL = TRUE, ignoreInit = TRUE,
                 {
                   req(ref_data())
                   req(xpmt_data())
                   req(rv$quantdat)

                   temp <- rv$quantdat %>%
                     dplyr::mutate(ROI = paste0("(", .data$`ROI right edge (ppm)`, ", ", .data$`ROI left edge (ppm)`, ")")) %>%
                     dplyr::filter(ROI %in% input$ROI_to_plot_quantdat)
                   temp2 <- rv$user_reference_data %>% dplyr::filter(.data$`Chemical shift(ppm)` %in% temp$`Chemical shift(ppm)`)

                   ROI_lines <- ROI_line_gen(data = temp[!duplicated(temp$`ROI left edge (ppm)`),,drop = FALSE])
                   ROI_annots <- ROI_annot_gen(data = temp2)

                   # Update plot
                   plotly::plotlyProxyInvoke(quantdata_plot_proxy, "relayout",
                                             list(annotations = ROI_annots,
                                                  shapes = ROI_lines))
                 })

    # Observer that prompts confirmation of profiling anytime the button is clicked.
    observeEvent(c(input$auto_profile),
                 {
                   req(isolate(ref_data()))
                   req(input$auto_profile > 0)

                   shinyWidgets::ask_confirmation(
                     inputId = NS(id, "profile_confirm"),
                     title = "Are you sure you would like to begin profiling?",
                     text = NULL,
                     type = "question",
                     btn_labels = c("Cancel", "Continue"),
                     btn_colors = c("#D3D3D3", "#428BCA"),
                     closeOnClickOutside = TRUE,
                     showCloseButton = TRUE,
                     allowEscapeKey = TRUE,
                     cancelOnDismiss = TRUE,
                     html = FALSE,
                     session = shiny::getDefaultReactiveDomain()
                   )
                 })

    # Observer that performs quantification for individually selected metabolite and spectrum, and plots
    # resulting fit on the displayed plot (i.e. output$refmet_dspedt_selected_plot)
    observeEvent(c(input$show_metquant),{
      req(input$show_metquant > 0)
      req(rv$dspedt_user_reference_data)

      signalROI_right <- rv$quantdat %>%
        dplyr::filter(paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]") == input$signal_to_check) %>%
        .$`ROI right edge (ppm)`
      signalROIdat <- rv$quantdat %>% dplyr::filter(.data$`ROI right edge (ppm)` == signalROI_right)

      if(is.null(rv$dspedt_profiling_data[[input$sample_to_plot]][[input$signal_to_check]]) |
         !identical(rv$curr_ROI_profile[[input$sample_to_plot]][[input$signal_to_check]], signalROIdat)){
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
                                 solvent             = attr(xpmt_data(), "exp_info")$solvent)

        # Formats the data object
        imported_data <- ppmData_to_rDolphin(ppmData = txpmt_data,
                                             metabs  = signalROIdat)
        imported_data$program_parameters <- list(BGdensity                          = input$gpp_BGdensity,
                                                 widthtolerance                     = input$gpp_widthtolerance,
                                                 gaussian                           = input$gpp_gaussian,
                                                 j_coupling_variation               = input$gpp_j_coupling_variation,
                                                 BG_gaussian_percentage             = input$gpp_BG_gaussian_percentage,
                                                 BG_width                           = input$gpp_BG_width,
                                                 BG_width_tolerance                 = input$gpp_BG_width_tolerance,
                                                 errorprov                          = input$gpp_errorprov,
                                                 fitting_maxiter                    = input$gpp_fitting_maxiter,
                                                 nls_lm_maxiter                     = input$gpp_nls_lm_maxiter,
                                                 ftol                               = input$gpp_ftol,
                                                 ptol                               = input$gpp_ptol,
                                                 factor                             = input$gpp_factor,
                                                 additional_signal_ppm_distance     = input$gpp_additional_signal_ppm_distance,
                                                 signals_to_add                     = input$gpp_signals_to_add,
                                                 fitting_maxiterrep                 = input$gpp_fitting_maxiterrep,
                                                 additional_signal_improvement      = input$gpp_additional_signal_improvement,
                                                 additional_signal_percentage_limit = input$gpp_additional_signal_percentage_limit,
                                                 peakdet_minimum                    = input$gpp_peakdet_minimum)

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
      }

    })

    observeEvent(c(rv$dspedt_profiling_data, input$show_metquant),{
      req(input$show_metquant)
      req(rv$dspedt_profiling_data[[input$sample_to_plot]][[input$signal_to_check]])

      profiling_data <- rv$dspedt_profiling_data[[input$sample_to_plot]][[input$signal_to_check]]

      fmtted_signal_to_check <- rv$quantdat %>%
        dplyr::filter(paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]") == input$signal_to_check) %>%
        dplyr::mutate(SigName = paste0(.data$Metabolite, "_", .data$`Quantification Signal`)) %>% .$SigName
      roidat <- rv$quantdat %>%
        dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]")) %>%
        dplyr::filter(Signal == input$signal_to_check)
      signames <- rv$quantdat %>%
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

    # Dynamic action button to automatically profile reference metabolite data. Only appears after
    # reference metabolite data are uploaded.
    output$ui_auto_profile <- renderUI({
      req(ref_data())

      actionButton(NS(id, "auto_profile"), label = "Profile")
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
                                                                         label    = "Gaussian Ratio:",
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
                                                                         label    = "Fitting error threshold:",
                                                                         value    = 3)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_errorprov"),
                                                                        title     = "Minimum acceptable fitting error of optimized fit. Optimization stops when the error falls below this threshold. Default value is 3.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_fitting_maxiter"),
                                                                         label    = "Max iterations of optimization subroutine:",
                                                                         value    = 8)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_fitting_maxiter"),
                                                                        title     = "(Optional) The maximum number of iterations for a subroutine of an algorithm to optimize fitting parameters.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_nls_lm_maxiter"),
                                                                         label    = "Max iterations of Levenberg Marquardt (LM) algorithm:",
                                                                         value    = 200)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_nls_lm_maxiter"),
                                                                        title     = "The maximum number of iterations for the Levenberg Marquardt algorithm used to optimize fitting parameters. Default value is 200.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_ftol"),
                                                                         label    = "LM algorithm sum of squares error threshold:",
                                                                         value    = 1e-06)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_ftol"),
                                                                        title     = "Minimum sum of squared errors threshold for LM algorithm. Default value is 0.000001.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_ptol"),
                                                                         label    = "LM algorithm relative error threshold:",
                                                                         value    = 1e-06)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_ptol"),
                                                                        title     = "Minimum relative error threshold for LM algorithm. Default value is 0.000001.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_factor"),
                                                                         label    = "LM algorithm control factor:",
                                                                         value    = 0.01)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_factor"),
                                                                        title     = "Control parameter used to determine the initial step bound for the LM algorithm. Default value is 0.01.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_fitting_maxiterrep"),
                                                                         label    = "fitting_maxiterrep:",
                                                                         value    = 0)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_fitting_maxiterrep"),
                                                                        title     = "Default value is zero.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_peakdet_minimum"),
                                                                         label    = "peakdet_minimum:",
                                                                         value    = 0.01)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_peakdet_minimum"),
                                                                        title     = "Default value is 0.01.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                     # These only apply to the "fitting error / signal area ratio analyses"
                                                     # performed in the rDolphin GUI. We do not implement them, so these
                                                     # parameters are unnecessary.
                                                     # column(width = 6,
                                                     #        selectInput(inputId   = NS(id, "gpp_automatic_removal"),
                                                     #                    label     = "automatic_removal:",
                                                     #                    choices   = c("Yes" = "Y", "No" = "N"),
                                                     #                    selected  = "Y"))
                                                   ),
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
                                                                         label    = "BGS Gaussian Ratio:",
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

                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_additional_signal_ppm_distance"),
                                                                         label    = "additional_signal_ppm_distance:",
                                                                         value    = 0.002)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_additional_signal_ppm_distance"),
                                                                        title     = "Default value is 0.002.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_signals_to_add"),
                                                                         label    = "signals_to_add:",
                                                                         value    = 2)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_signals_to_add"),
                                                                        title     = "Default value is two.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),

                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_additional_signal_improvement"),
                                                                         label    = "additional_signal_improvement:",
                                                                         value    = 0.75)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_additional_signal_improvement"),
                                                                        title     = "Default value is 0.75.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover"),

                                                     column(width = 6,
                                                            numericInput(inputId  = NS(id, "gpp_additional_signal_percentage_limit"),
                                                                         label    = "additional_signal_percentage_limit:",
                                                                         value    = 3)),
                                                     shinyBS::bsTooltip(id        = NS(id, "gpp_additional_signal_percentage_limit"),
                                                                        title     = "Default value is three.",
                                                                        placement = "bottom",
                                                                        trigger   = "hover")

                                                   ),
                                                   style = "primary"
                          ))
    })

    #----------------------------------------------------------------------------------------------------------

    # Misc ----------------------------------------------------------------------------------------------------

    # Creates a datatable that displays reference database
    output$refmet_database <- DT::renderDataTable({

      bmse_associations %>%
        DT::datatable(rownames   = FALSE,
                      extensions = "Responsive")
    })

    #----------------------------------------------------------------------------------------------------------

    # Module output
    eventReactive(input$profile_confirm, ignoreInit = TRUE, ignoreNULL = TRUE,
                  {
                    req(rv$user_reference_data)

                    ## Document all changes relative to original reference data (ref_data()$ref_data)
                    orig_refdat <- ref_data()$ref_data
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
                      templength <- length(rv$refchanges[[name]])
                      tempres <- list(OriginalEntry = rv$refchanges[[name]][[1]])
                      if (templength > 1){
                        for(i in 2:templength){
                          binary_diffmat <- !(rv$refchanges[[name]][[i]] == rv$refchanges[[name]][[1]])
                          crows <- which(apply(binary_diffmat,1,any))
                          ccols <- which(apply(binary_diffmat,2,any))
                          tempres <- append(tempres, list(rv$refchanges[[name]][[i]][crows, ccols, drop = FALSE]))
                        }
                        names(tempres) <- c("OriginalEntry", paste0("Edit_", 1:(templength-1)))
                      }
                      currdf <- rv$user_reference_data %>% dplyr::filter(.data$Metabolite == name)
                      allres[[name]] <- c(tempres, FinalEntry = list(currdf))
                    }
                    attr(rv$user_reference_data, "edit_history") <- allres

                    list(profile_confirm     = input$profile_confirm,
                         user_edited_refdata = rv$user_reference_data,
                         quantdata           = rv$quantdat,
                         global_parameters   = list(BGdensity                          = input$gpp_BGdensity,
                                                    widthtolerance                     = input$gpp_widthtolerance,
                                                    gaussian                           = input$gpp_gaussian,
                                                    j_coupling_variation               = input$gpp_j_coupling_variation,
                                                    BG_gaussian_percentage             = input$gpp_BG_gaussian_percentage,
                                                    BG_width                           = input$gpp_BG_width,
                                                    BG_width_tolerance                 = input$gpp_BG_width_tolerance,
                                                    errorprov                          = input$gpp_errorprov,
                                                    fitting_maxiter                    = input$gpp_fitting_maxiter,
                                                    nls_lm_maxiter                     = input$gpp_nls_lm_maxiter,
                                                    ftol                               = input$gpp_ftol,
                                                    ptol                               = input$gpp_ptol,
                                                    factor                             = input$gpp_factor,
                                                    additional_signal_ppm_distance     = input$gpp_additional_signal_ppm_distance,
                                                    signals_to_add                     = input$gpp_signals_to_add,
                                                    fitting_maxiterrep                 = input$gpp_fitting_maxiterrep,
                                                    additional_signal_improvement      = input$gpp_additional_signal_improvement,
                                                    additional_signal_percentage_limit = input$gpp_additional_signal_percentage_limit,
                                                    peakdet_minimum                    = input$gpp_peakdet_minimum))
                  })
  })
}
