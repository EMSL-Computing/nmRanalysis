#' Module: UI elements specific to experimental data visualization
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
#' @details This is the UI component for the module created to handle the visualization of experimental data.
#' The value provided for 'id' should be identical across xpmt_data_vizUI(), xpmt_data_vizoptionsUI(),
#' and xpmt_data_vizServer().
#'
#' This module component provides the UI elements that allow users to:
#' 1) Visualize uploaded experimental data through an interactive plotly figure
#' 2) Visualize experimental metadata through a searchable datatable.
#'
#' @import shiny
#' @importFrom magrittr %>%
#'
xpmt_data_vizUI <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns('e_data_plot'))),
    DT::dataTableOutput(ns("f_data_df"))
  )
}

#' Module: UI elements specific to data visualization options (e.g. filters, subplots)
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
#' @details This is the UI component for the module created to handle editing of uploaded experimental data.
#' The value provided for 'id' should be identical across xpmt_data_vizUI(), xpmt_data_vizoptionsUI(),
#' xpmt_metadata_vizUI, and xpmt_data_vizServer().
#'
#' This module component provides the dynamic UI elements that allow users to:
#' 1) Select which sample spectrum should be displayed on the main plotly plot
#' 2) Toggle the option for subplot display of any selected plot region. This subplot
#'    will shown the intensities across all sample spectra at the selected region.
#' 3) Apply or remove uploaded data filter(s)
#'
#' Note that since this UI component is dynamic, its code is found within the corresponding server component of the
#' module, xpmt_data_vizServer().
#'
#' @import shiny
#'
xpmt_data_vizoptionsUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("vizoptions_ui"))
  )
}

#' Module: Server functions specific to experimental data and metadata visualization
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
#' @param xpmt_data The reactive object returned by the xpmt_data_uploadServer() server module containing
#' the experimental data as a nmRanalysis ppmData object.
#'
#' @details This is the UI component for the module created to handle the visualization of experimental data and metadata.
#' The value provided for 'id' should be identical across xpmt_data_vizUI(), xpmt_data_vizoptionsUI(),
#' and xpmt_data_vizServer().
#'
#' This module component provides the back-end code that:
#' 1) Generates the plotly figure of experimental data
#' 2) Generates the datatable of experimental metadata
#' 3) Creates and displays subplots corresponding to selected regions
#' 4) Applies or removes filters to the data
#'
#' @return A reactive object containing the uploaded experimental data with any modifications (i.e. filters) applied. Note that
#' any applied filters are stored as an attribute of the object. The attribute contains both the filtered range and the set of data
#' that was filtered within this range.
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
xpmt_data_vizServer <- function(id, xpmt_data){
  stopifnot(is.reactive(xpmt_data))
  moduleServer(id, function(input, output, session){

    # Initialize reactiveValues object that will be used to store version of uploaded experimental data (xpmt_data())
    # that may be modified/updated at various points. Cannot call xpmt_data() directly in the initialization because
    # reactiveValues() is not a reactive environment. Therefore, we use the observer seen immediately below.
    rv <- reactiveValues(obs_show_subplot_suspend = TRUE,
                         subplot_dat = NULL)

    observe(priority = 10, {
      req(xpmt_data())
      rv$modified_xpmt_data <- xpmt_data()
    })

    # UI element creating a dropdown button containing several options that relate
    # to experimental data visualization. These options include:
    # 1) Choosing which sample spectrum to display
    # 2) Toggle for whether subplots should be displayed on region select
    # 3) The ability to specify a numeric (PPM) range for a filter to be applied, or,
    #    if filters are currently applied, to be removed.
    # 4) Not an option, but a text display indicating the filters that are currently applied to
    #    the data.
    output$vizoptions_ui <- renderUI({

      req(xpmt_data())

      shinyWidgets::dropdownButton(
        # Allows users to select which sample spectrum to display.
        selectInput(inputId = NS(id, "sample_to_plot"),
                    label   = "Choose a spectrum to plot",
                    choices = colnames(xpmt_data()$e_data)[-1]),

        # Toggle for subplot display
        fluidRow(column(9, shinyWidgets::materialSwitch(inputId = NS(id, "show_subplot"),
                                                        label   = "Show subplot on box select",
                                                        value   = FALSE,
                                                        status  = "primary",
                                                        right   = TRUE)),
                 column(3, actionButton(inputId = NS(id, "show_subplot_help"),
                                        label = "?"))),
        shinyBS::bsTooltip(id = NS(id, "show_subplot_help"),
                           title     = "Use the Box Select tool in the upper-right corner of the plot to select a region to be shown in a subplot.",
                           placement = "bottom",
                           trigger   = "hover"),

        # Allows users to specify filter range
        shinyWidgets::numericRangeInput(inputId = NS(id, "range"),
                                        label = "Specify a filter range:",
                                        value = 0),

        # Buttons to apply or remove specified filters.
        fluidRow(
          column(
            width = 6,
            actionButton(inputId = NS(id, "apply_filter"),
                         label = "Apply Filter"),
            shinyBS::bsTooltip(id = NS(id, "apply_filter"),
                               title     = "Changes to this filters will carry over to downstream plots",
                               placement = "bottom",
                               trigger   = "hover"),
          ),
          column(
            width = 6,
            actionButton(inputId = NS(id, "remove_filter"),
                         label = "Remove Filter"),
            shinyBS::bsTooltip(id = NS(id, "remove_filter"),
                               title     = "Changes to this filters will carry over to downstream plots",
                               placement = "bottom",
                               trigger   = "hover"),
          ),
        ),
        h4(""),

        # HTML output to display the filters currently applied
        htmlOutput(NS(id,"applied_filters_text")),

        circle = TRUE, status = "info",
        icon = icon("cog"), width = "300px",

        tooltip = shinyWidgets::tooltipOptions(title = "Data Options")
      )
    })

    # Output (in HTML format) to display the filters that are currently applied to the data.
    output$applied_filters_text <- renderUI({

      req(xpmt_data())

      if(length(attr(rv$modified_xpmt_data, "filters")) == 0){
        htmltools::HTML("<strong>Currently applied filters:</strong><br/>None")

      } else{
        allfilts <- rlist::list.ungroup(rlist::list.select(attr(rv$modified_xpmt_data, "filters"), range))
        allfilts <- Reduce("c", lapply(allfilts, function(x){paste0("(", x$min, ", ", x$max, ")")}))
        htmltools::HTML(paste0("<strong>Currently applied filters:</strong><br/>", paste(allfilts, collapse = "<br/>")))

      }
    })


    # Initialize plot of single spectrum from uploaded experimental data
    output$e_data_plot <- plotly::renderPlotly({
      # Will not evaluate unless experimental data has been uploaded
      req(xpmt_data())
      req(input$sample_to_plot)
      req(input$sample_to_plot %in% names(xpmt_data()$e_data))

      # Code to resume the observer that was started in a suspended state. We also update the value of
      # rv$obs_show_subplot_suspend so that $resume() is not called every time this plot is rendered,
      # but only after the first rendering of the plot.
      if(rv$obs_show_subplot_suspend){
        obs_show_subplot$resume()
        rv$obs_show_subplot_suspend <- FALSE
      }

      plot_xpmt_data(xpmt_data      = isolate(rv$modified_xpmt_data$e_data),
                     sourceid       = "e_data_plot",
                     sample_to_plot = input$sample_to_plot)
    })

    # Proxy object of the main plot of the single selected spectrum data.
    e_data_plot_proxy <- plotly::plotlyProxy("e_data_plot")

    # Plotting of the subplot of ppm data across all sample spectra at the selected region
    output$e_data_subplot <- plotly::renderPlotly({
      # Will not evaluate unless experimental data has been uploaded
      # and toggle to show subplot on region select is activated.
      req(xpmt_data())
      req(input$show_subplot)

      brushedData <- plotly::event_data("plotly_brushed", source = "e_data_plot")

      if(is.null(brushedData)){
        return(NULL)
      }

      plot_xpmt_data(xpmt_data      = isolate(rv$modified_xpmt_data$e_data),
                     sourceid       = "e_data_subplot",
                     sample_to_plot = input$sample_to_plot,
                     brushed_data   = brushedData)
    })

    # Observer to control pop-up (i.e. modal) containing the subplot of spectral data at a selected region.
    # Note: This works fine, but the only thing that I would like to change is
    # loading of subsequent plots generated by different brush events.
    # On initial plot, the loading spinner shows, but on subsequent plots, it does not.
    # Not sure how to fix this yet, but I suspect the issue lies in the execution order.
    # This observer triggers before "e_data_subplot" invalidates.
    # Note that we specify suspended = TRUE. This forces the observer to begin in a suspended state on app initialization.
    # We do this here because on app start e_data_plot is not defined and so a warning error is output. Given that the
    # observer is in a suspended state, we need to resume it after e_data_plot is defined so that the observer can
    # operate as intended. See the code snippet under output$e_data_plot for how this is done.
    obs_show_subplot <- observeEvent(plotly::event_data("plotly_brushed", source = "e_data_plot"), suspended = TRUE, {
      req(input$show_subplot)
      req(xpmt_data())


      brushedData <- plotly::event_data("plotly_brushed", source = "e_data_plot")

      req(!identical(brushedData, rv$subplot_dat))

      removeModal()
      showModal(
        modalDialog(
          shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, 'e_data_subplot'))),
          title = paste0("All Sample Spectra: ", round(min(brushedData$x),3)," PPM to ", round(max(brushedData$x),3), " PPM"),
          size = "xl",
          easyClose = TRUE,
          fade = FALSE
        ))

      rv$subplot_dat <- brushedData
    })


    # This datatable corresponds to the metadata
    output$f_data_df <- DT::renderDataTable({
      # Will not evaluate unless experimental data has been uploaded
      req(xpmt_data())

      xpmt_data()$f_data %>%
        DT::datatable(rownames   = FALSE,
                      options = list(scrollX = TRUE))
    })

    # Observer to apply filter specified to ALL data, not just the data of the selected sample spectrum.
    observeEvent(c(input$apply_filter), ignoreInit = TRUE, ignoreNULL = TRUE,
                 {
                   req(xpmt_data())
                   req(input$apply_filter > 0)

                   shinyFeedback::feedbackDanger("range",
                                                 input$range[1] > input$range[2],
                                                 'Filter range format is "a to b", where a < b.')

                   req(input$range[1] < input$range[2])

                   allfilts <- rlist::list.ungroup(rlist::list.select(attr(rv$modified_xpmt_data, "filters"), range))
                   idx_of_filt2add <- which(lapply(allfilts, function(x){all(input$range %in% c(x$min, x$max))}) == TRUE)
                   req(length(idx_of_filt2add) == 0)

                   rv$modified_xpmt_data <- filter_ppm(rv$modified_xpmt_data,
                                                       range = list(min = min(as.numeric(input$range)),
                                                                    max = max(as.numeric(input$range))))

                   plotly::plotlyProxyInvoke(e_data_plot_proxy, "deleteTraces", as.list(as.integer(0)))

                   plotly::plotlyProxyInvoke(e_data_plot_proxy, "addTraces",
                                             list(x    = rv$modified_xpmt_data$e_data[["PPM"]],
                                                  y    = rv$modified_xpmt_data$e_data[[input$sample_to_plot]],
                                                  type = 'scatter',
                                                  mode = 'lines',
                                                  line = list(width = 1)))
                 })

    # Observer to remove filter specified from ALL data, not just the data of the selected sample spectrum.
    observeEvent(c(input$remove_filter), ignoreInit = TRUE, ignoreNULL = TRUE,
                 {
                   req(xpmt_data())
                   req(input$remove_filter > 0)

                   allfilts       <- rlist::list.ungroup(rlist::list.select(attr(rv$modified_xpmt_data, "filters"), range))
                   req(allfilts)

                   idx_of_filt2rm <- which(lapply(allfilts, function(x){all(input$range %in% c(x$min, x$max))}) == TRUE)
                   req(idx_of_filt2rm)

                   rv$modified_xpmt_data <- remove_filter_ppm(rv$modified_xpmt_data,
                                                              filters = as.numeric(idx_of_filt2rm))
                   rv$modified_xpmt_data$e_data <- rv$modified_xpmt_data$e_data %>% dplyr::arrange(.data[["PPM"]])

                   plotly::plotlyProxyInvoke(e_data_plot_proxy, "deleteTraces", as.list(as.integer(0)))

                   plotly::plotlyProxyInvoke(e_data_plot_proxy, "addTraces",
                                             list(x    = rv$modified_xpmt_data$e_data[["PPM"]],
                                                  y    = rv$modified_xpmt_data$e_data[[input$sample_to_plot]],
                                                  type = 'scatter',
                                                  mode = 'lines',
                                                  line = list(width = 1)))

                 })

    # This reactive generates the output of this module. The output is the uploaded experimental data with any modifications
    # applied.
    reactive({
      req(xpmt_data())
      rv$modified_xpmt_data
    })
  })
}
#' Module: UI elements specific to experimental data visualization
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
#' @details This is the UI component for the module created to handle the visualization of experimental data.
#' The value provided for 'id' should be identical across xpmt_data_vizUI(), xpmt_data_vizoptionsUI(),
#' and xpmt_data_vizServer().
#'
#' This module component provides the UI elements that allow users to:
#' 1) Visualize uploaded experimental data through an interactive plotly figure
#' 2) Visualize experimental metadata through a searchable datatable.
#'
#' @import shiny
#' @importFrom magrittr %>%
#'
xpmt_data_vizUI <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns('e_data_plot'))),
    DT::dataTableOutput(ns("f_data_df"))
  )
}

#' Module: UI elements specific to data visualization options (e.g. filters, subplots)
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
#' @details This is the UI component for the module created to handle editing of uploaded experimental data.
#' The value provided for 'id' should be identical across xpmt_data_vizUI(), xpmt_data_vizoptionsUI(),
#' xpmt_metadata_vizUI, and xpmt_data_vizServer().
#'
#' This module component provides the dynamic UI elements that allow users to:
#' 1) Select which sample spectrum should be displayed on the main plotly plot
#' 2) Toggle the option for subplot display of any selected plot region. This subplot
#'    will shown the intensities across all sample spectra at the selected region.
#' 3) Apply or remove uploaded data filter(s)
#'
#' Note that since this UI component is dynamic, its code is found within the corresponding server component of the
#' module, xpmt_data_vizServer().
#'
#' @import shiny
#'
xpmt_data_vizoptionsUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("vizoptions_ui"))
  )
}

#' Module: Server functions specific to experimental data and metadata visualization
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
#' @param xpmt_data The reactive object returned by the xpmt_data_uploadServer() server module containing
#' the experimental data as a nmRanalysis ppmData object.
#'
#' @details This is the UI component for the module created to handle the visualization of experimental data and metadata.
#' The value provided for 'id' should be identical across xpmt_data_vizUI(), xpmt_data_vizoptionsUI(),
#' and xpmt_data_vizServer().
#'
#' This module component provides the back-end code that:
#' 1) Generates the plotly figure of experimental data
#' 2) Generates the datatable of experimental metadata
#' 3) Creates and displays subplots corresponding to selected regions
#' 4) Applies or removes filters to the data
#'
#' @return A reactive object containing the uploaded experimental data with any modifications (i.e. filters) applied. Note that
#' any applied filters are stored as an attribute of the object. The attribute contains both the filtered range and the set of data
#' that was filtered within this range.
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
xpmt_data_vizServer <- function(id, xpmt_data){
  stopifnot(is.reactive(xpmt_data))
  moduleServer(id, function(input, output, session){

    # Initialize reactiveValues object that will be used to store version of uploaded experimental data (xpmt_data())
    # that may be modified/updated at various points. Cannot call xpmt_data() directly in the initialization because
    # reactiveValues() is not a reactive environment. Therefore, we use the observer seen immediately below.
    rv <- reactiveValues(obs_show_subplot_suspend = TRUE,
                         subplot_dat = NULL)

    observe(priority = 10, {
      req(xpmt_data())
      rv$modified_xpmt_data <- xpmt_data()
    })

    # UI element creating a dropdown button containing several options that relate
    # to experimental data visualization. These options include:
    # 1) Choosing which sample spectrum to display
    # 2) Toggle for whether subplots should be displayed on region select
    # 3) The ability to specify a numeric (PPM) range for a filter to be applied, or,
    #    if filters are currently applied, to be removed.
    # 4) Not an option, but a text display indicating the filters that are currently applied to
    #    the data.
    output$vizoptions_ui <- renderUI({

      req(xpmt_data())

      shinyWidgets::dropdownButton(
        # Allows users to select which sample spectrum to display.
        selectInput(inputId = NS(id, "sample_to_plot"),
                    label   = "Choose a spectrum to plot",
                    choices = colnames(xpmt_data()$e_data)[-1]),

        # Toggle for subplot display
        shinyWidgets::materialSwitch(inputId = NS(id, "show_subplot"),
                                     label   = "Show subplot on box select",
                                     value   = FALSE,
                                     status  = "primary",
                                     right   = TRUE),

        # Allows users to specify filter range
        shinyWidgets::numericRangeInput(inputId = NS(id, "range"),
                                        label = "Specify a filter range:",
                                        value = 0),

        # Buttons to apply or remove specified filters.
        fluidRow(
          column(
            width = 6,
            actionButton(inputId = NS(id, "apply_filter"),
                         label = "Apply Filter")
          ),
          column(
            width = 6,
            actionButton(inputId = NS(id, "remove_filter"),
                         label = "Remove Filter")
          ),
        ),
        h4(""),

        # HTML output to display the filters currently applied
        htmlOutput(NS(id,"applied_filters_text")),

        circle = TRUE, status = "info",
        icon = icon("cog"), width = "300px",

        tooltip = shinyWidgets::tooltipOptions(title = "Data Options")
      )
    })

    # Output (in HTML format) to display the filters that are currently applied to the data.
    output$applied_filters_text <- renderUI({

      req(xpmt_data())

      if(length(attr(rv$modified_xpmt_data, "filters")) == 0){
        htmltools::HTML("<strong>Currently applied filters:</strong><br/>None")

      } else{
        allfilts <- rlist::list.ungroup(rlist::list.select(attr(rv$modified_xpmt_data, "filters"), range))
        allfilts <- Reduce("c", lapply(allfilts, function(x){paste0("(", x$min, ", ", x$max, ")")}))
        htmltools::HTML(paste0("<strong>Currently applied filters:</strong><br/>", paste(allfilts, collapse = "<br/>")))

      }
    })


    # Initialize plot of single spectrum from uploaded experimental data
    output$e_data_plot <- plotly::renderPlotly({
      # Will not evaluate unless experimental data has been uploaded
      req(xpmt_data())
      req(input$sample_to_plot)
      req(input$sample_to_plot %in% names(xpmt_data()$e_data))

      # Code to resume the observer that was started in a suspended state. We also update the value of
      # rv$obs_show_subplot_suspend so that $resume() is not called every time this plot is rendered,
      # but only after the first rendering of the plot.
      if(rv$obs_show_subplot_suspend){
        obs_show_subplot$resume()
        rv$obs_show_subplot_suspend <- FALSE
      }

      plot_xpmt_data(xpmt_data      = isolate(rv$modified_xpmt_data$e_data),
                     sourceid       = "e_data_plot",
                     sample_to_plot = input$sample_to_plot)
    })

    # Proxy object of the main plot of the single selected spectrum data.
    e_data_plot_proxy <- plotly::plotlyProxy("e_data_plot")

    # Plotting of the subplot of ppm data across all sample spectra at the selected region
    output$e_data_subplot <- plotly::renderPlotly({
      # Will not evaluate unless experimental data has been uploaded
      # and toggle to show subplot on region select is activated.
      req(xpmt_data())
      req(input$show_subplot)

      brushedData <- plotly::event_data("plotly_brushed", source = "e_data_plot")

      if(is.null(brushedData)){
        return(NULL)
      }

      plot_xpmt_data(xpmt_data      = isolate(rv$modified_xpmt_data$e_data),
                     sourceid       = "e_data_subplot",
                     sample_to_plot = input$sample_to_plot,
                     brushed_data   = brushedData)
    })

    # Observer to control pop-up (i.e. modal) containing the subplot of spectral data at a selected region.
    # Note: This works fine, but the only thing that I would like to change is
    # loading of subsequent plots generated by different brush events.
    # On initial plot, the loading spinner shows, but on subsequent plots, it does not.
    # Not sure how to fix this yet, but I suspect the issue lies in the execution order.
    # This observer triggers before "e_data_subplot" invalidates.
    # Note that we specify suspended = TRUE. This forces the observer to begin in a suspended state on app initialization.
    # We do this here because on app start e_data_plot is not defined and so a warning error is output. Given that the
    # observer is in a suspended state, we need to resume it after e_data_plot is defined so that the observer can
    # operate as intended. See the code snippet under output$e_data_plot for how this is done.
    obs_show_subplot <- observeEvent(plotly::event_data("plotly_brushed", source = "e_data_plot"), suspended = TRUE, {
      req(input$show_subplot)
      req(xpmt_data())


      brushedData <- plotly::event_data("plotly_brushed", source = "e_data_plot")

      req(!identical(brushedData, rv$subplot_dat))

      removeModal()
      showModal(
        modalDialog(
          shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, 'e_data_subplot'))),
          title = paste0("All Sample Spectra: ", round(min(brushedData$x),3)," PPM to ", round(max(brushedData$x),3), " PPM"),
          size = "xl",
          easyClose = TRUE,
          fade = FALSE
        ))

      rv$subplot_dat <- brushedData
    })


    # This datatable corresponds to the metadata
    output$f_data_df <- DT::renderDataTable({
      # Will not evaluate unless experimental data has been uploaded
      req(xpmt_data())

      xpmt_data()$f_data %>%
        DT::datatable(rownames   = FALSE,
                      options = list(scrollX = TRUE))
    })

    # Observer to apply filter specified to ALL data, not just the data of the selected sample spectrum.
    observeEvent(c(input$apply_filter), ignoreInit = TRUE, ignoreNULL = TRUE,
                 {
                   req(xpmt_data())
                   req(input$apply_filter > 0)

                   shinyFeedback::feedbackDanger("range",
                                                 input$range[1] > input$range[2],
                                                 'Filter range format is "a to b", where a < b.')

                   req(input$range[1] < input$range[2])

                   allfilts <- rlist::list.ungroup(rlist::list.select(attr(rv$modified_xpmt_data, "filters"), range))
                   idx_of_filt2add <- which(lapply(allfilts, function(x){all(input$range %in% c(x$min, x$max))}) == TRUE)
                   req(length(idx_of_filt2add) == 0)

                   rv$modified_xpmt_data <- filter_ppm(rv$modified_xpmt_data,
                                                       range = list(min = min(as.numeric(input$range)),
                                                                    max = max(as.numeric(input$range))))

                   plotly::plotlyProxyInvoke(e_data_plot_proxy, "deleteTraces", as.list(as.integer(0)))

                   plotly::plotlyProxyInvoke(e_data_plot_proxy, "addTraces",
                                             list(x    = rv$modified_xpmt_data$e_data[["PPM"]],
                                                  y    = rv$modified_xpmt_data$e_data[[input$sample_to_plot]],
                                                  type = 'scatter',
                                                  mode = 'lines',
                                                  line = list(width = 1)))
                 })

    # Observer to remove filter specified from ALL data, not just the data of the selected sample spectrum.
    observeEvent(c(input$remove_filter), ignoreInit = TRUE, ignoreNULL = TRUE,
                 {
                   req(xpmt_data())
                   req(input$remove_filter > 0)

                   allfilts       <- rlist::list.ungroup(rlist::list.select(attr(rv$modified_xpmt_data, "filters"), range))
                   req(allfilts)

                   idx_of_filt2rm <- which(lapply(allfilts, function(x){all(input$range %in% c(x$min, x$max))}) == TRUE)
                   req(idx_of_filt2rm)

                   rv$modified_xpmt_data <- remove_filter_ppm(rv$modified_xpmt_data,
                                                              filters = as.numeric(idx_of_filt2rm))
                   rv$modified_xpmt_data$e_data <- rv$modified_xpmt_data$e_data %>% dplyr::arrange(.data[["PPM"]])

                   plotly::plotlyProxyInvoke(e_data_plot_proxy, "deleteTraces", as.list(as.integer(0)))

                   plotly::plotlyProxyInvoke(e_data_plot_proxy, "addTraces",
                                             list(x    = rv$modified_xpmt_data$e_data[["PPM"]],
                                                  y    = rv$modified_xpmt_data$e_data[[input$sample_to_plot]],
                                                  type = 'scatter',
                                                  mode = 'lines',
                                                  line = list(width = 1)))

                 })

    # This reactive generates the output of this module. The output is the uploaded experimental data with any modifications
    # applied.
    reactive({
      req(xpmt_data())
      rv$modified_xpmt_data
    })
  })
}
