#' Module: UI elements specific to peak picking (speaq) options
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
#' @details
#'
#'
#' @import shiny
#' @importFrom magrittr %>%
#'
metid_peakfinderUI <- function(id){
  ns <- NS(id)
  tagList(
    shinyBS::bsCollapse(id = ns("pf_options"), open = "Peak Detection Options",
                        shinyBS::bsCollapsePanel(title = "Peak Detection Options",
                                                 fluidRow(
                                                   column(width = 6,
                                                          numericInput(inputId  = ns("nDivRange"),
                                                                       label    = "Spectral Segment Length:",
                                                                       value    = 128)
                                                          ),
                                                   column(width = 6,
                                                          shinyWidgets::numericRangeInput(inputId  = ns("CWTscale"),
                                                                                          label    = "CWT scale factor range:",
                                                                                          value    = c(1, 16),
                                                                                          min      = 1,
                                                                                          max      = 64)
                                                          )
                                                   ),
                                                 fluidRow(
                                                   column(width = 6,
                                                          numericInput(inputId  = ns("CWTscale_step"),
                                                                       label    = "CWT scale factor step size:",
                                                                       value    = 2)
                                                   ),
                                                   column(width = 6,
                                                          numericInput(inputId  = ns("snrThresh"),
                                                                       label    = "Signal-to-Noise Threshold:",
                                                                       value    = 0.8)
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(width = 6,
                                                          numericInput(inputId  = ns("baselineThresh"),
                                                                       label    = "Intensity Threshold:",
                                                                       value    = 50000)
                                                   )
                                                 ),
                                                 style = "primary"
                                                 )
                        ),

    # clickable button
    shinyWidgets::actionBttn(inputId = ns("metid_button"),
                             label = "Detect Peaks",
                             style = "unite",
                             color = "primary",
                             size = "sm")

  )
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
#' @details
#'
#'
#' @import shiny
#' @importFrom magrittr %>%
#'
metid_vizUI <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns('metid_e_data_plot'))),
  )
}

#' Module: UI elements specific to data visualization options
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
#' @import shiny
#'
metid_vizoptionsUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("metid_vizoptions_ui"))
  )
}

#' Module: Server functions specific to metabolite identification
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
#'
#' @details
#'
#' @return
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
metid_Server <- function(id, xpmt_data){
  stopifnot(is.reactive(xpmt_data))
  moduleServer(id, function(input, output, session){

    rv <- reactiveValues(obs_show_subplot_suspend = TRUE,
                         subplot_dat = NULL)


    # UI element creating a dropdown button containing several options that relate
    # to experimental data visualization for this tab. These options include:
    # 1) Choosing which sample spectrum to display
    # 2) Toggle for whether subplots should be displayed on region select
    # TO BE ADDED: Choice of which peak group to annotate on plot
    output$metid_vizoptions_ui <- renderUI({

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

        # Toggle for display of annotations
        shinyWidgets::materialSwitch(inputId = NS(id, "show_annotations"),
                                     label   = "Toggle detected feature annotations",
                                     value   = TRUE,
                                     status  = "primary",
                                     right   = TRUE),

        numericInput(inputId = NS(id, "peak_group_dist"),
                     label   = "Annotation Grouping Threshold",
                     value   = round(mean(diff(xpmt_data()$e_data$PPM)), 5),
                     min     = min(diff(xpmt_data()$e_data$PPM))),

        circle = TRUE, status = "info",
        icon = icon("cog"), width = "300px",

        tooltip = shinyWidgets::tooltipOptions(title = "Data Options")
      )
    })

    output$metid_e_data_plot <- plotly::renderPlotly({

      isolate({
        req(xpmt_data())

        # Code to resume the observer that was started in a suspended state. We also update the value of
        # rv$obs_show_subplot_suspend so that $resume() is not called every time this plot is rendered,
        # but only after the first rendering of the plot.
        if(rv$obs_show_subplot_suspend){
          obs_show_subplot$resume()
          rv$obs_show_subplot_suspend <- FALSE
        }

        plotly::plot_ly(source = "id_metid_e_data_plot", type = "scatter", mode = "lines") %>%
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

    # Create proxy for the above plotly plot to more fluidly interact with plot.
    metid_e_data_plot_proxy <- plotly::plotlyProxy("metid_e_data_plot")

    # This observer is responsible for plotting the trace (i.e. line) corresponding to a selected
    # experimental spectrum. This is implemented through proxy updates for the sake of efficiency.
    observeEvent(c(input$sample_to_plot, xpmt_data()), priority = -1, {
      req(input$sample_to_plot)
      req(input$sample_to_plot %in% names(xpmt_data()$e_data))

      xpmt_data_sample <- xpmt_data()$e_data %>% dplyr::select(.data$PPM, .data[[input$sample_to_plot]])
      df_long <- xpmt_data_sample %>%
        tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")

      # # Clear shapes and annotations
      # plotly::plotlyProxyInvoke(refmet_dspedt_plot_proxy, "relayout",
      #                           list(annotations = NULL,
      #                                shapes = NULL))
      # Clear plots
      plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "deleteTraces", as.list(as.integer(0)))

      # # Line shape update
      # ROI_lines <- ROI_line_gen(data = rv$dspedt_user_reference_data)
      #
      # # Annotation update
      # ROI_annots <- ROI_annot_gen(data = rv$dspedt_user_reference_data)

      plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "addTraces",
                                list(x    = df_long$PPM,
                                     y    = df_long$Intensity,
                                     type = 'scatter',
                                     mode = 'lines',
                                     line = list(width = 1),
                                     hoverinfo = "text",
                                     text = paste0("PPM: ", round(df_long$PPM, 4), "<br>",
                                                   "Intensity: ", round(df_long$Intensity, 4))))
      plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                list(title = paste("Experimental Data:", input$sample_to_plot)))

      # plotly::plotlyProxyInvoke(refmet_dspedt_plot_proxy, "relayout",
      #                           list(title = paste("Experimental Data:", input$sample_to_plot, "<br>", "<sup>",
      #                                              input$which_refmet_dspedt, "Peak Location(s) displayed", "</sup>"),
      #                                annotations = ROI_annots,
      #                                shapes = ROI_lines))
    })

    # Plotting of the subplot of ppm data across all sample spectra at the selected region
    output$metid_selected_subplot <- plotly::renderPlotly({
      req(input$sample_to_plot)
      req(input$show_subplot)

      brushedData <- plotly::event_data("plotly_brushed", source = "id_metid_e_data_plot")


      if(is.null(brushedData)){
        return(NULL)
      }

      isolate({

        xpmt_data      <- xpmt_data()$e_data
        brushed_data   <- brushedData
        sample_to_plot <- input$sample_to_plot
        sourceid       <- "id_metid_selected_subplot"

        if(!is.null(rv$spectra_peaks)){
          ppm     <- as.numeric(xpmt_data[, 1, drop = TRUE])

          selsamp_peaks <- rv$spectra_peaks[[which(names(rv$spectra_peaks) == input$sample_to_plot)]]

          ppm_peaks         <- ppm[selsamp_peaks]
          ppm_peak_diffs    <- diff(ppm_peaks)
          split_idxs        <- which(ppm_peak_diffs > input$peak_group_dist) + 1
          peak_groups       <- split(ppm_peaks, cumsum(seq_along(ppm_peaks) %in% split_idxs))
          feature_locations <- Reduce("c", lapply(peak_groups, mean))
          filt_features     <- feature_locations[which(feature_locations >= min(brushed_data$x) & feature_locations <= max(brushed_data$x))]

          # Create default annot object to add as annotation to plot
          ROI_annot <- list(
            y         = 0,
            xref      = "x",
            yref      = "y",
            arrowhead = 4,
            ay        = 40
          )

          ROI_annots <- list()
          if(length(filt_features) > 0 & input$show_annotations){
            for(i in 1:length(filt_features)){
              ROI_annot[["x"]]         <- filt_features[i]
              ROI_annot[["text"]]      <- paste0(sprintf("<b>%s</b>", "PPM"), ": ",
                                                 round(filt_features[i], 5))
              ROI_annot[["arrowsize"]] <- 1
              ROI_annot[["showarrow"]] <- TRUE

              ROI_annots               <- c(ROI_annots, list(ROI_annot))
            }
          }

          df_long <- xpmt_data %>%
            tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")

          df_long <- df_long %>% dplyr::filter(.data$PPM >= min(brushed_data$x), .data$PPM <= max(brushed_data$x))
          df_long_selsamp <- df_long %>% dplyr::filter(.data$Sample == sample_to_plot)
          df_long_nonselsamp <- df_long %>% dplyr::filter(.data$Sample != sample_to_plot)

          p <- plotly::plot_ly(source = sourceid) %>%
            plotly::config(displaylogo = FALSE,
                           modeBarButtons = list(list("zoom2d"), list("zoomIn2d"),
                                                 list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                                 list("resetScale2d"), list("toImage"))) %>%
            plotly::layout(xaxis = list(title     = "PPM",
                                        autorange = "reversed"),
                           yaxis = list(title     = "Intensity"),
                           annotations = ROI_annots,
                           showlegend = TRUE,
                           dragmode = "zoom2d") %>%
            plotly::add_trace(data = df_long_selsamp,
                              x=~.data$PPM, y=~.data$Intensity, type = "scatter", mode = "lines", name = sample_to_plot,
                              line = list(width = 1.3), hoverinfo = "text",
                              hovertext = paste0("Sample: ", sample_to_plot,
                                                 "\nPPM: ", round(df_long_selsamp$PPM, 4),
                                                 "\nIntensity: ", round(df_long_selsamp$Intensity, 4))) %>%
            plotly::add_trace(data = df_long_nonselsamp,
                              x=~.data$PPM, y=~.data$Intensity, type = "scatter", name = ~.data$Sample,
                              opacity = 0.3, mode = "lines", line = list(color = "#000000", width = 0.75),
                              hoverinfo = "text", hovertext = paste0("Sample: ", df_long_nonselsamp$Sample,
                                                                     "\nPPM: ", round(df_long_nonselsamp$PPM, 4),
                                                                     "\nIntensity: ", round(df_long_nonselsamp$Intensity, 4)),
                              showlegend = FALSE) %>%
            plotly::config(edits = list(annotationTail     = TRUE,
                                        annotationText     = FALSE,
                                        annotationPosition = FALSE))



        } else{
          df_long <- xpmt_data %>%
            tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")

          df_long <- df_long %>% dplyr::filter(.data$PPM >= min(brushed_data$x), .data$PPM <= max(brushed_data$x))
          df_long_selsamp <- df_long %>% dplyr::filter(.data$Sample == sample_to_plot)
          df_long_nonselsamp <- df_long %>% dplyr::filter(.data$Sample != sample_to_plot)

          p <- plotly::plot_ly(source = sourceid) %>%
            plotly::config(displaylogo = FALSE,
                           modeBarButtons = list(list("zoom2d"), list("zoomIn2d"),
                                                 list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                                 list("resetScale2d"), list("toImage"))) %>%
            plotly::layout(xaxis = list(title     = "PPM",
                                        autorange = "reversed"),
                           yaxis = list(title     = "Intensity"),
                           showlegend = TRUE,
                           dragmode = "zoom2d") %>%
            plotly::add_trace(data = df_long_selsamp,
                              x=~.data$PPM, y=~.data$Intensity, type = "scatter", mode = "lines", name = sample_to_plot,
                              line = list(width = 1.3), hoverinfo = "text",
                              hovertext = paste0("Sample: ", sample_to_plot,
                                                 "\nPPM: ", round(df_long_selsamp$PPM, 4),
                                                 "\nIntensity: ", round(df_long_selsamp$Intensity, 4))) %>%
            plotly::add_trace(data = df_long_nonselsamp,
                              x=~.data$PPM, y=~.data$Intensity, type = "scatter", name = ~.data$Sample,
                              opacity = 0.3, mode = "lines", line = list(color = "#000000", width = 0.75),
                              hoverinfo = "text", hovertext = paste0("Sample: ", df_long_nonselsamp$Sample,
                                                                     "\nPPM: ", round(df_long_nonselsamp$PPM, 4),
                                                                     "\nIntensity: ", round(df_long_nonselsamp$Intensity, 4)),
                              showlegend = FALSE) %>%
            plotly::config(edits = list(annotationTail     = TRUE,
                                        annotationText     = FALSE,
                                        annotationPosition = FALSE,
                                        shapePosition      = TRUE))

        }

        p
      })

    })

    # Create proxy for the above plotly subplot to more fluidly interact with refmet plot and table edits.
    metid_subplot_proxy <- plotly::plotlyProxy("metid_selected_subplot")

    # Observer to control pop-up (i.e. modal) containing the subplot of spectral data at a selected region.
    # Note: This works fine, but the only thing that I would like to change is
    # loading of subsequent plots generated by different brush events.
    # On initial plot, the loading spinner shows, but on subsequent plots, it does not.
    # Not sure how to fix this yet, but I suspect the issue lies in the execution order.
    # This observer triggers before "e_data_subplot" invalidates.
    obs_show_subplot <- observeEvent(plotly::event_data("plotly_brushed", source = "id_metid_e_data_plot"), suspended = TRUE, {
      req(input$show_subplot)


      brushedData <- plotly::event_data("plotly_brushed", source = "id_metid_e_data_plot")

      req(!identical(brushedData, rv$subplot_dat))

      removeModal()
      showModal(
        modalDialog(
          shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, 'metid_selected_subplot'))),
          title = paste0("All Sample Spectra: ", round(min(brushedData$x),3)," PPM to ", round(max(brushedData$x),3), " PPM"),
          size = "xl",
          easyClose = TRUE,
          fade = FALSE
        ))
      rv$subplot_dat <- brushedData
    })


    observeEvent(input$metid_button,
                 {
                   req(input$metid_button > 0)
                   req(xpmt_data())
                   req(input$nDivRange < nrow(xpmt_data()$e_data))
                   req(input$CWTscale)
                   req(input$CWTscale_step < input$CWTscale[2])
                   req(input$CWTscale[1] < input$CWTscale[2])
                   req(input$baselineThresh >= 0)
                   req(input$snrThresh > 0 | input$snrThresh == -1)

                   shinyWidgets::sendSweetAlert(
                     session = getDefaultReactiveDomain(),
                     title = "Detecting Peaks...",
                     text = "This may take several minutes. Do not refresh the page or re-initiate the detection algorithm. This alert will close when identification is complete.",
                     type = NULL,
                     btn_labels = NULL,
                     btn_colors = NULL,
                     html = FALSE,
                     closeOnClickOutside = FALSE,
                     showCloseButton = FALSE,
                     width = NULL,
                     showConfirmButton = FALSE,
                     closeOnEscapeKey = FALSE
                   )


                   spectra <- xpmt_data()$e_data %>% dplyr::select(-.data$PPM) %>% t()

                   spectra_peaks <- speaq::detectSpecPeaks(X              = spectra,
                                                           nDivRange      = input$nDivRange,
                                                           scales         = seq(input$CWTscale[1], input$CWTscale[2], input$CWTscale_step),
                                                           baselineThresh = input$baselineThresh,
                                                           SNR.Th         = input$snrThresh,
                                                           verbose        = FALSE)

                   rescheck <- Reduce("c", lapply(spectra_peaks, length))

                   if(sum(rescheck) == 0){
                     shinyWidgets::closeSweetAlert()

                     shinyWidgets::show_alert(
                       title = "No peaks detected.",
                       text = "Try adjusting the Intensity and/or Signal-to-Noise Thresholds.",
                       type = "warning"
                     )
                   }

                   req(sum(rescheck) > 0)

                   names(spectra_peaks) <- rownames(spectra)

                   rv$spectra_peaks <- spectra_peaks

                   shinyWidgets::closeSweetAlert()
                 })

    observeEvent(c(rv$spectra_peaks, input$sample_to_plot, input$peak_group_dist, input$show_annotations),
                 {
                   req(xpmt_data())
                   req(input$sample_to_plot)
                   req(input$peak_group_dist)
                   req(rv$spectra_peaks)

                   if(!input$show_annotations){
                     plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                               list(annotations = NULL))
                   }
                   req(input$show_annotations)

                   ppm     <- as.numeric(xpmt_data()$e_data[, 1, drop = TRUE])

                   selsamp_peaks <- rv$spectra_peaks[[which(names(rv$spectra_peaks) == input$sample_to_plot)]]

                   ppm_peaks         <- ppm[selsamp_peaks]
                   ppm_peak_diffs    <- diff(ppm_peaks)
                   split_idxs        <- which(ppm_peak_diffs > input$peak_group_dist) + 1
                   peak_groups       <- split(ppm_peaks, cumsum(seq_along(ppm_peaks) %in% split_idxs))
                   feature_locations <- Reduce("c", lapply(peak_groups, mean))

                   # Create default annot object to add as annotation to plot
                   ROI_annot <- list(
                     y         = 0,
                     xref      = "x",
                     yref      = "y",
                     arrowhead = 4,
                     ay        = 40
                   )

                   ROI_annots <- list()
                   for(i in 1:length(feature_locations)){
                     ROI_annot[["x"]]         <- feature_locations[i]
                     ROI_annot[["text"]]      <- paste0(sprintf("<b>%s</b>", "PPM"), ": ",
                                                        round(feature_locations[i], 5))
                     ROI_annot[["arrowsize"]] <- 1
                     ROI_annot[["showarrow"]] <- TRUE

                     ROI_annots               <- c(ROI_annots, list(ROI_annot))
                   }

                   plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                             list(annotations = ROI_annots))

                 })

  })
}
