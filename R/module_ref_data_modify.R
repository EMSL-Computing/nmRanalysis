#' Module: UI elements specific to reference data ROI editing
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
    DT::dataTableOutput(ns("refmet_dspedt_table"))
  )
}

#' Module: UI elements specific to adding/removing target metabolites
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
    DT::dataTableOutput(ns("refmet_quant_table"))
  )
}

#' Module: Profiling button
#'
#' @param id A string denoting the namespace id.
#'
#' @details This is one of the UI components for the module created to handle all editing of reference (target) metabolite data.
#' The value provided for 'id' should be identical across the following: ref_data_ROIeditingUI(), ref_data_add_delUI(),
#' ref_data_quantTab(), ref_data_profileUI(), and ref_data_editingServer().
#'
#' This module component provides the UI elements that allow users to initiate metabolite profiling
#'
#' @import shiny
#'
ref_data_profileUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui_auto_profile"))
  )
}

#' Module: Server functions specific to target metabolite data modification
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
#' changes made to the initial data; and a logical value (TRUE/FALSE) indicating whether the user confirmed proceeding with profiling.
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
                         unsaved_change = list())
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
                                   rowid = paste0(.data$Metabolite, dplyr::row_number()))

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

                   rv$user_reference_data <- rv$user_reference_data %>% dplyr::filter(.data$Metabolite %ni% input$refmet_toremove)

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

                   # clear any unsaved change and quantification checks
                   rv$unsaved_change <- list()
                   rv$dspedt_profiling_data <- NULL
                   rv$dspedt_profiling_data_plot <- NULL

                   # Line shape update
                   ROI_lines <- ROI_line_gen(dspedt_refmet_data = rv$dspedt_user_reference_data)

                   # Annotation update
                   ROI_annots <- ROI_annot_gen(dspedt_refmet_data = rv$dspedt_user_reference_data)

                   # Update plot
                   plotly::plotlyProxyInvoke(refmet_dspedt_plot_proxy, "relayout",
                                             list(title = paste("Experimental Data:", input$sample_to_plot, "<br>", "<sup>",
                                                                input$which_refmet_dspedt, "Region(s) of Interest (ROI) displayed", "</sup>"),
                                                  annotations = ROI_annots,
                                                  shapes = ROI_lines))
                 })

    # This datatable corresponds to the selected reference metabolite data to display/edit
    output$refmet_dspedt_table <- DT::renderDT({

      req(ref_data())
      req(input$which_refmet_dspedt)

      isolate({

        rv$user_reference_data %>%
          dplyr::filter(.data$Metabolite %in% input$which_refmet_dspedt) %>%
          dplyr::select(-.data$rowid) %>%
          DT::datatable(rownames   = FALSE,
                        editable   = TRUE,
                        selection = "none",
                        extensions = "Responsive")

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


      plotly::plot_ly(source = "id_refmet_dspedt_selected_plot") %>%
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

      # Clear any previous quantification checks
      rv$dspedt_profiling_data <- NULL
      rv$dspedt_profiling_data_plot <- NULL

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
      ROI_lines <- ROI_line_gen(dspedt_refmet_data = rv$dspedt_user_reference_data)

      # Annotation update
      ROI_annots <- ROI_annot_gen(dspedt_refmet_data = rv$dspedt_user_reference_data)

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
                                                   input$which_refmet_dspedt, "Region(s) of Interest (ROI) displayed", "</sup>"),
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

        # Action Button for metabolite- and spectrum-specific quantification check
        actionButton(NS(id, "show_metquant"), label = "Check ROI quantifications"),


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

      isolate({
        ROI_lines <- ROI_line_gen(dspedt_refmet_data = rv$dspedt_user_reference_data)
        ROI_annots <- ROI_annot_gen(dspedt_refmet_data = rv$dspedt_user_reference_data)

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
    observeEvent(plotly::event_data("plotly_brushed", source = "id_refmet_dspedt_selected_plot"),{
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
    observeEvent(plotly::event_data("plotly_relayout", source = "id_refmet_dspedt_selected_plot"),
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
    observeEvent(plotly::event_data("plotly_relayout", source = "id_refmet_dspedt_selected_subplot"),
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
                   ROI_lines <- ROI_line_gen(dspedt_refmet_data = rv$dspedt_user_reference_data)

                   # Annotation update
                   ROI_annots <- ROI_annot_gen(dspedt_refmet_data = rv$dspedt_user_reference_data)

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

                   edtd_colname <- names(rv$dspedt_user_reference_data)[changed_col]

                   if(edtd_colname %in% c("Chemical shift(ppm)", "Chemical shift tolerance (ppm)",
                                          "ROI left edge (ppm)", "ROI right edge (ppm)")) {

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

                   } else if(edtd_colname %in% c("Quantification Signal", "Half bandwidth (Hz)",
                                                 "Multiplicity", "J coupling (Hz)", "Roof effect")) {

                     rv$dspedt_user_reference_data[[edtd_colname]][changed_row] <- as.numeric(v)

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

    # Quantification Data and Profiling Trigger ---------------------------------------------------------------

    # This datatable corresponds to the reference metabolite data to be used for quantification
    output$refmet_quant_table <- DT::renderDT({

      req(ref_data())
      req(rv$user_reference_data)

      rv$user_reference_data %>%
        dplyr::filter(.data$Quantify == 1) %>%
        dplyr::select(-.data$rowid) %>%
        DT::datatable(rownames   = FALSE,
                      editable   = FALSE,
                      extensions = "Responsive")
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

      if(is.null(rv$dspedt_profiling_data)){
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

        # Formats the data object
        imported_data <- ppmData_to_rDolphin(ppmData = xpmt_data(),
                                                          metabs  = rv$dspedt_user_reference_data %>% dplyr::filter(.data$Quantify == 1))


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

        #Splitting of ROI data into individual ROIs to be quantified
        dummy <- which(is.na(ROI_data[, 1]))

        if (length(dummy) == 0){
          dummy <- dim(ROI_data)[1]+1
        }
        lal <- which(duplicated(ROI_data[-dummy,1:2]) == F)
        ROI_separator <- cbind(lal, c(lal[-1] - 1, dim(ROI_data[-dummy,])[1]))

        baselinedataset <- baseline::baseline.rollingBall(imported_data$dataset,5,5)$baseline

        #For every ROI
        totit <- max(seq_along(ROI_separator[, 1]))
        sumit <- 0
        for (ROI_index in seq_along(ROI_separator[, 1])) {

          #Preparation of ROI parameters
          ROI_profile <- ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]
          ROI_buckets <- which.min(abs(as.numeric(ROI_profile[1, 1]) - imported_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2]) - imported_data$ppm))

          if (length(ROI_buckets) < 20) {
            sumit <- sumit + nrow(imported_data$dataset)
            shinyWidgets::updateProgressBar(
              session = session,
              id      = "metquant_profiling_progress",
              title   = paste0("Ignoring ROI as width is too small"),
              value   = trunc(sumit/totit*100)
            )
            next
          }
          if (ROI_buckets[1] > ROI_buckets[2]){
            ROI_buckets <- rev(ROI_buckets)
          }


          # Preparation of program parameters to be sued during fitting, with some variables added to ease interpretability of code
          program_parameters             <- imported_data$program_parameters
          program_parameters$freq        <- imported_data$freq
          program_parameters$ROI_buckets <- ROI_buckets
          program_parameters$buck_step   <- imported_data$buck_step

          Xdata        <- imported_data$ppm[ROI_buckets]
          fitting_type <- as.character(ROI_profile[1, 3])
          if (length(grep("Clean",fitting_type)) == 1) {
            program_parameters$clean_fit <- "Y"
          } else {
            program_parameters$clean_fit <- "N"
          }
          signals_to_quantify <- which(ROI_profile[, 5] >= 1)
          signals_codes       <- (ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2])


          shinyWidgets::updateProgressBar(
            session = session,
            id      = "metquant_profiling_progress",
            title   = paste0('Profiling ROI ', ROI_index, ' of ', nrow(ROI_separator),
                             " for Spectrum: ", input$sample_to_plot),
            value   = trunc(sumit/totit*100)
          )

          # THIS IS A TEMPORARY SOLUTION. THE PERMANENT SOLUTION IS TO FORK
          # RDOLPHIN, ADAPT CODE AS NECESSARY, AND PROPERLY CITE THE ORIGINAL AUTHOR
          # OF RDOLPHIN
          profiling_func <- utils::getFromNamespace("profiling_func", "rDolphin")
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
          # profiling_func() is an internal rDolphin function that is not
          # meant to be accessed by the user.
          # Note that Ydata is internally defined by profiling function despite
          # what is supplied to the argument above. See automatic_profiling.R and
          # profiling_func().
          final_output <- output$final_output
          reproducibility_data <- output$reproducibility_data
          sumit <- sumit + 1

        }
        shinyWidgets::closeSweetAlert(session = session)

        rv$dspedt_profiling_data <- list(final_output = lapply(final_output,
                                                               function(x) x[rownames(x) == input$sample_to_plot, , drop = FALSE]),
                                         reproducibility_data = reproducibility_data[[which(rownames(imported_data$dataset) %in% input$sample_to_plot)]])
      }

    })

    observeEvent(c(rv$dspedt_profiling_data, input$show_metquant),{
      req(input$show_metquant)
      req(rv$dspedt_profiling_data)

      profiling_data <- rv$dspedt_profiling_data

      ROI_plots <- vector("list", length = ncol(profiling_data$final_output$quantification))

      for(i in 1:ncol(profiling_data$final_output$quantification)){

        tempdat <- profiling_data$reproducibility_data[[i]]

        # If there are no surrounding signals, then tempdat$plot_data should have only four rows. The first
        # row is the summed intensities across all signals (target and surrounding); the second row is
        # the summed intensities across all baseline signals; the third row is the sum of the first and second
        # rows (so effectively the "generated" spectrum based on the signal and background signal fits); and the
        # fourth row is the fitted intensities of the ROI of interest. Subsequent rows would correspond to surrounding
        # signals.
        # Initialize plotdata
        plotdata <- data.frame(PPM                = c(tempdat$Xdata, tempdat$Xdata, tempdat$Xdata, tempdat$Xdata),
                               Intensity          = c(tempdat$Ydata, tempdat$Ydata, tempdat$Ydata, tempdat$Ydata),
                               Intensity_Type     = rep(c("Generated", "Signal", "Background", "Surrounding"),
                                                        each = length(tempdat$Xdata)))

        # Update intensities accordingly
        plotdata$Intensity[plotdata$Intensity_Type == "Signal"] <-
          tempdat$plot_data[which(grepl(make.names(input$which_refmet_dspedt), rownames(tempdat$plot_data))),]
        plotdata$Intensity[plotdata$Intensity_Type == "Background"] <-
          tempdat$plot_data[rownames(tempdat$plot_data) == "baseline_sum", ]
        plotdata$Intensity[plotdata$Intensity_Type == "Generated"] <-
          tempdat$plot_data[rownames(tempdat$plot_data) == "fitted_sum", ]
        plotdata$Intensity[plotdata$Intensity_Type == "Surrounding"] <-
          tempdat$plot_data[rownames(tempdat$plot_data) == "signals_sum", ] - plotdata$Intensity[plotdata$Intensity_Type == "Signal"]
        plotdata$ROI <- i

        ROI_plots[[i]] <- plotdata
      }

      ROI_plots <- Reduce("rbind", ROI_plots)
      ROI_data <- rv$dspedt_user_reference_data %>% dplyr::filter(.data$Quantify == 1)

      # Line shape update
      # Create default line object to add as shape to plot
      ROI_line <- list(
        type = "line",
        line = list(color = "red"),
        xref = "x",
        yref = "y"
      )

      # Create list containing all line objects. For each line object in this list, populate with the
      # ROI information corresponding to the given reference metabolite peak.
      ROI_lines <- list()
      for(i in 1:nrow(ROI_data)){

        ROI_line[["x0"]]        <- ROI_data[i,,drop = FALSE]$"ROI left edge (ppm)"
        ROI_line[["x1"]]        <- ROI_data[i,,drop = FALSE]$"ROI right edge (ppm)"
        ROI_line[c("y0", "y1")] <- 0
        ROI_lines               <- c(ROI_lines, list(ROI_line))
      }

      # Annotation update
      # Create default annot object to add as annotation to plot
      ROI_annot <- list(
        y         = 0,
        xref      = "x",
        yref      = "y",
        arrowhead = 4,
        ay        = 40
      )

      ROI_annots <- list()
      for(i in 1:nrow(ROI_data)){
        ROI_annot[["x"]]         <- ROI_data[i,,drop = FALSE]$"Chemical shift(ppm)"
        ROI_annot[["text"]]      <- paste0(sprintf("<b>%s</b>", "ROI: "),
                                           ROI_data[i,,drop = FALSE]$"Chemical shift(ppm)", " (",
                                           ROI_data[i,,drop = FALSE]$"ROI left edge (ppm)", ", ",
                                           ROI_data[i,,drop = FALSE]$"ROI right edge (ppm)", ")", " <br> ",
                                           sprintf("<b>%s</b>", "Quantification: "), round(profiling_data$final_output$quantification[1, i],3), " <br> ",
                                           sprintf("<b>%s</b>", "Signal to Area Ratio: "), round(profiling_data$final_output$signal_area_ratio[1, i],3), " <br> ",
                                           sprintf("<b>%s</b>", "Fitting Error: "), round(profiling_data$final_output$fitting_error[1, i],3))
        ROI_annot[["arrowsize"]] <- ROI_data[i,,drop = FALSE]$"Chemical shift tolerance (ppm)"
        ROI_annot[["showarrow"]] <- TRUE
        ROI_annots               <- c(ROI_annots, list(ROI_annot))
      }


      xpmt_data_sample <- xpmt_data()$e_data %>% dplyr::select(.data$PPM, .data[[input$sample_to_plot]])
      df_long <- xpmt_data_sample %>%
        tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")


      p <- ROI_plots %>% dplyr::group_by(.data$ROI) %>% plotly::plot_ly(source = "id_prof_refmet_view_plot") %>%
        plotly::config(displaylogo = FALSE,
                       modeBarButtons = list(list("select2d"), list("zoom2d"), list("zoomIn2d"),
                                             list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                             list("resetScale2d"), list("toImage"))) %>%
        plotly::layout(title = paste("Experimental Data:", input$sample_to_plot, "<br>", "<sup>",
                                     input$refmet_to_plot, "Region(s) of Interest (ROI) displayed", "</sup>"),
                       xaxis = list(title     = "PPM",
                                    autorange = "reversed"),
                       yaxis = list(title     = "Intensity"),
                       showlegend = TRUE,
                       dragmode = "zoom2d",
                       annotations = ROI_annots,
                       shapes = ROI_lines) %>%
        plotly::config(edits = list(annotationTail     = TRUE,
                                    annotationText     = FALSE,
                                    annotationPosition = FALSE,
                                    shapePosition      = FALSE)) %>%
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

      rv$dspedt_profiling_data_plot <- p

    })


    # Plotting of quantified data
    output$dspedt_profiling_data_plot <- plotly::renderPlotly({
      req(rv$dspedt_profiling_data_plot)

      if(is.null(rv$dspedt_profiling_data_plot)){
        return(NULL)
      }

      rv$dspedt_profiling_data_plot

    })

    observeEvent(c(rv$dspedt_profiling_data_plot, input$show_metquant),{
      req(input$show_metquant > 0)
      req(rv$dspedt_profiling_data_plot)

      removeModal()
      showModal(
        modalDialog(
          shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, 'dspedt_profiling_data_plot'))),
          title = "Quantification",
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

    #----------------------------------------------------------------------------------------------------------

    # Misc ----------------------------------------------------------------------------------------------------

    # Creates a datatable that displays reference database
    output$refmet_database <- DT::renderDataTable({

      nmrapp::refmets %>%
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

                    allres <- vector("list", length = 2)
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
                      allres[[name]] <- tempres
                    }
                    attr(rv$user_reference_data, "edit_history") <- allres

                    list(profile_confirm     = input$profile_confirm,
                         user_edited_refdata = rv$user_reference_data)
                  })
  })
}
