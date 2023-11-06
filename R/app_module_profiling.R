#' Module: UI element displaying action button to trigger batch profiling, and a set of editable global profiling options
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
#' @details This is one of the UI components for the module created to handle all profiling-related functions.
#' The value provided for 'id' should be identical across the following: profiling_quant_sidebarUI(), profiling_prequantUI(),
#' profiling_completeviewUI(), profiling_detailedviewUI(), and profilingServer().
#'
#' This module component displays an action button to trigger batch profiling, and a set of editable global profiling options
#'
#' @import shiny
#'
profiling_quant_sidebarUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui_global_profiling_parameters")),
    uiOutput(ns("prof_type_select")),
    uiOutput(ns("ui_auto_profile"))
  )
}

#' Module: UI element displaying aggregated ROIs based on final edited metabolite fitting data
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
#' @details This is one of the UI components for the module created to handle all profiling-related functions.
#' The value provided for 'id' should be identical across the following: profiling_quant_sidebarUI(), profiling_prequantUI(),
#' profiling_completeviewUI(), profiling_detailedviewUI(), and profilingServer().
#'
#' This module component displays the set of target metabolite data that will be used for profiling, aggregated into ROIs.
#' These data reflect any and all edits made to the target metabolite data and are displayed within a searchable, filterable table.
#'
#' @import shiny
#'
profiling_prequantUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("vizoptions_quantdata_ui")),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns('quantdata_plot'))),
    DT::dataTableOutput(ns("refmet_quant_table")),
    uiOutput(ns("tempsoln"))
  )
}

#' Module: UI element to display trelliscope plot of fitted spectra
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
#'
#' @param id A string denoting the namespace id.
#'
#' @details This is one of the UI components for the module created to handle all functions related to profiling
#' of experimental data, based on user edits to the fitting parameters of a pre-specified list of
#' reference (target) metabolites. The value provided for 'id' should be identical across the following:
#' profiling_quant_sidebarUI(), profiling_prequantUI(), profiling_completeviewUI(), profiling_detailedviewUI(),
#' and profilingServer().
#'
#' This module component provides the UI element that allow users to:
#' 1) View (within a trelliscope display) the fitted spectra over the sample spectra for all samples and target metabolites
#'
#'
#' @import shiny
#'
profiling_completeviewUI <- function(id){
  ns <- NS(id)
  tagList(
    #shinycssloaders::withSpinner(trelliscopejs::trelliscopeOutput(ns("trelliscope"))),
    shinycssloaders::withSpinner(plotOutput(ns("trelliscope"))),
    uiOutput(ns("graph_switch_ui")),
    uiOutput(ns("spectra_ui")),
    br(),
    DT::dataTableOutput(ns("complete_profres_tab"))
  )
}

#' Module: UI element to display trelliscope plot of fitted spectra
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
#' @details This is one of the UI components for the module created to handle all functions related to profiling
#' of experimental data, based on user edits to the fitting parameters of a pre-specified list of
#' reference (target) metabolites. The value provided for 'id' should be identical across the following:
#' profiling_quant_sidebarUI(), profiling_prequantUI(), profiling_completeviewUI(), profiling_detailedviewUI(),
#' and profilingServer().
#'
#' This module component provides the UI elements that allow users to:
#' 1) Select from several options to control the shown display
#' 2) View interactively the fitted spectra of a given metabolite over a given sample spectrum
#'
#'
#' @import shiny
#'
profiling_detailedviewUI <- function(id){
  ns <- NS(id)

  tagList(
    uiOutput(ns("profvizoptions_ui")),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns('prof_refmet_view_plot')))
  )
}



#' Module: Server functions specific to metabolite profiling and profiling result generation
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
#' @param ref_data A reactive object containing target metabolite data, after having been edited.
#'
#' @details This is the server component for the module created to handle all functions related to profiling
#' of experimental data, based on user edits to the fitting parameters of a pre-specified list of
#' reference (target) metabolites. The value provided for 'id' should be identical across the following:
#' profiling_quant_sidebarUI(), profiling_prequantUI(), profiling_completeviewUI(), profiling_detailedviewUI(),
#' and profilingServer().
#'
#' This module component provides the back-end code that:
#' 1) Allows for final editing of global profiling paramters, and displays metabolite data aggregated into ROIs
#' 2) Performs the profiling for all target metabolites for all sample spectra
#' 3) Generates plots and table(s) used to visualize and summarize the profiling results
#'
#'
#' @return A reactive object containing a list of two elements. The first element of this list, final_output, is the
#' output of rDolphin's profiling procedure. The second element of the list, reproducibility_data, is some other component
#' generated by rDolphin's profiling procedure. For details, refer to rDolhpin's documentation and/or git repo.

#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom plyr .
#'
profilingServer <- function(id, xpmt_data, ref_data, connec){

  stopifnot(is.reactive(xpmt_data))
  stopifnot(is.reactive(ref_data))
  stopifnot(is.reactive(connec))
  moduleServer(id, function(input, output, session){

    rv <- reactiveValues(subplot_dat = NULL)

    # Observer that prompts confirmation of profiling anytime the button is clicked.
    observeEvent(c(input$auto_profile),
                 {
                   req(isolate(ref_data()))
                   req(input$auto_profile > 0)
                   ###

                   temp <- ref_data()$quantdata %>%
                     dplyr::mutate(SigName = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"))

                   if(any(temp$`ROI left edge (ppm)` < temp$`ROI right edge (ppm)`)){
                     badsignals <- paste(temp$SigName[(temp$`ROI left edge (ppm)` < temp$`ROI right edge (ppm)`)], collapse = "; ")
                     shinyWidgets::show_alert(
                       title = "Fitting parameter error.",
                       text = paste("The signal left edge should be larger than the signal right edge.
                       Revise the fitting parameters of the following signals:", badsignals),
                       type = "error"
                     )
                   }
                   req(all(temp$`ROI left edge (ppm)` > temp$`ROI right edge (ppm)`))

                   if(any(temp$`Chemical shift tolerance (ppm)` > round(abs(temp$`ROI right edge (ppm)` - temp$`Chemical shift(ppm)`), 3)) |
                      any(temp$`Chemical shift tolerance (ppm)` > round(abs(temp$`ROI left edge (ppm)` - temp$`Chemical shift(ppm)`), 3))){
                     badsignals <- paste(temp$SigName[(temp$`Chemical shift tolerance (ppm)` > round(abs(temp$`ROI right edge (ppm)` - temp$`Chemical shift(ppm)`), 3)) |
                                                        (temp$`Chemical shift tolerance (ppm)` > round(abs(temp$`ROI left edge (ppm)` - temp$`Chemical shift(ppm)`), 3))], collapse = "; ")

                     shinyWidgets::show_alert(
                       title = "Fitting parameter error.",
                       text = paste("The chemical shift tolerance is larger than half the specified width of the signal region for the following
                       signals:", badsignals),
                       type = "error"
                     )
                   }
                   req(all(temp$`Chemical shift tolerance (ppm)` <= round(abs(temp$`ROI right edge (ppm)` - temp$`Chemical shift(ppm)`), 3)) &
                         all(temp$`Chemical shift tolerance (ppm)` <= round(abs(temp$`ROI left edge (ppm)` - temp$`Chemical shift(ppm)`), 3)))

                   if(any((temp$`Half bandwidth (Hz)` - input$gpp_widthtolerance) <= 0)){
                     badsignals <- paste(temp$SigName[(temp$`Half bandwidth (Hz)` - input$gpp_widthtolerance) <= 0], collapse = "; ")
                     shinyWidgets::show_alert(
                       title = "Fitting parameter error.",
                       text = paste("The lower bandwidth bound based on the specified bandwidth and tolerance is less than or equal to 0 for the following
                       signals:", badsignals),
                       type = "error"
                     )
                   }
                   req(all((temp$`Half bandwidth (Hz)` - input$gpp_widthtolerance) > 0))

                   if(any(temp$`Multiplicity` %ni% c("1", "2", "3", "4", "s", "d", "t", "q", "dd"))){
                     badsignals <- paste(temp$SigName[temp$`Multiplicity` %ni% c("1", "2", "3", "4", "s", "d", "t", "q", "dd")], collapse = "; ")
                     shinyWidgets::show_alert(
                       title = "Fitting parameter error.",
                       text = paste("The specified multiplicity is not supported. Revise the fitting parameters of the
                                    following signals:", badsignals),
                       type = "error"
                     )
                   }
                   req(all(temp$`Multiplicity` %in% c("1", "2", "3", "4", "s", "d", "t", "q", "dd")))

                   if(any(temp$`Multiplicity` %ni% c("1", "s"))){
                     temp2 <- temp %>% dplyr::filter(.data$`Multiplicity` %ni% c("1", "s"))

                     if(any((temp2$`J coupling (Hz)` - input$gpp_j_coupling_variation) <= 0)){
                       badsignals <- paste(temp2$SigName[(temp2$`J coupling (Hz)` - input$gpp_j_coupling_variation) <= 0], collapse = "; ")
                       shinyWidgets::show_alert(
                         title = "Fitting parameter error.",
                         text = paste("The lower J-coupling bound based on the specified J-coupling and tolerance must be
                                       greater than 0 for non-singlet multiplicities. Revise the following signals:", badsignals),
                         type = "error"
                       )
                     }
                     req(all((temp2$`J coupling (Hz)` - input$gpp_j_coupling_variation) > 0))

                     if(any(temp2$`J coupling 2 (Hz)` != 0)){
                       temp2b <- temp2 %>% dplyr::filter(.data$`J coupling 2 (Hz)` != 0)
                       if(any((temp2b$`J coupling 2 (Hz)` - input$gpp_j_coupling_variation) <= 0)){
                         badsignals <- paste(temp2b$SigName[(temp2b$`J coupling 2 (Hz)` - input$gpp_j_coupling_variation) <= 0], collapse = "; ")
                         shinyWidgets::show_alert(
                           title = "Fitting parameter error.",
                           text = paste("The lower J-coupling bound based on the specified J-coupling 2 and tolerance
                                        must be greater than 0 for non-singlet multiplicities.  Revise the following signals:", badsignals),
                           type = "error"
                         )
                       }
                       req(all((temp2b$`J coupling 2 (Hz)` - input$gpp_j_coupling_variation) > 0))

                       if(any((temp2b$`J coupling 2 (Hz)` - temp2b$`J coupling (Hz)`) > 0)){
                         badsignals <- paste(temp2b$SigName[(temp2b$`J coupling 2 (Hz)` - temp2b$`J coupling (Hz)`) > 0], collapse = "; ")
                         shinyWidgets::show_alert(
                           title = "Fitting parameter error.",
                           text = paste("J coupling 2 must be smaller than J coupling. Revise the following signals:", badsignals),
                           type = "error"
                         )
                       }
                       req(all((temp2b$`J coupling 2 (Hz)` - temp2b$`J coupling (Hz)`) <= 0))
                     }
                   } else if(temp$`Multiplicity` %in% c("1", "s")){
                     temp3 <- temp %>% dplyr::filter(.data$`Multiplicity` %in% c("1", "s"))
                     if(any(temp3$`J coupling (Hz)` != 0) | any(temp3$`J coupling 2 (Hz)` != 0)){
                       badsignals <- paste(temp3$SigName[temp3$`J coupling (Hz)` != 0 | temp3$`J coupling 2 (Hz)` != 0], collapse = "; ")
                       shinyWidgets::show_alert(
                         title = "Fitting parameter error.",
                         text = paste("J-coupling values should be set to 0 for singlets. Revise the following signals:", badsignals),
                         type = "error"
                       )
                     }
                     req(all(temp3$`J coupling (Hz)` == 0) & all(temp3$`J coupling 2 (Hz)` == 0))
                   }

                   ###
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

    # This datatable corresponds to the reference metabolite data to be used for quantification
    ref_quant_table_data <- reactiveValues(data=NULL)
    ref_quant_table_data_fill <- reactive({
      req(ref_data())
      req(input$ROI_to_plot_quantdat)
      #browser()

      temp <- ref_data()$quantdata %>%
        dplyr::mutate(ROI = paste0("(", .data$`ROI right edge (ppm)`, ", ", .data$`ROI left edge (ppm)`, ")")) %>%
        dplyr::filter(.data$ROI %in% input$ROI_to_plot_quantdat)
      ref_quant_table_data$data <- ref_data()$user_edited_refdata %>% dplyr::ungroup() %>%
        dplyr::filter(.data$`Chemical shift(ppm)` %in% temp$`Chemical shift(ppm)`) %>%
        dplyr::mutate(Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]")) %>%
        dplyr::filter(.data$Quantify == 1) %>%
        dplyr::arrange(.data$`Chemical shift(ppm)`) %>%
        dplyr::select(.data$Signal, .data$`Quantification Mode`, .data$`Chemical shift(ppm)`, #.data$`Chemical shift tolerance (ppm)`,
                      .data$`Half bandwidth (Hz)`, .data$Multiplicity, .data$`J coupling (Hz)`, .data$`J coupling 2 (Hz)`,
                      .data$`Roof effect`, .data$`Roof effect 2`, .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`,
                      .data$`Temperature (K)`, .data$`Solvent`)
      # temp2 <- dplyr::mutate(temp2, `Quantification Mode` = as.character(selectInput("sel", "", choices = c("Baseline Fitting", "Baseline Sum"))))
      #temp2 <- dplyr::mutate(`Quantification Mode` = as.character(for(i in 1:nrow(temp2$Signal)){selectInput(paste0("sel", i), "", choices = c("Baseline Fitting", "Baseline Sum"))}))
    })
    output$refmet_quant_table <- DT::renderDT({
      ref_quant_table_data_fill()
      ref_quant_table_data$data %>%
        DT::datatable(rownames   = FALSE,
                      editable   = TRUE,
                      options = list(dom = 't', scrollX = TRUE),
                      # callback = DT::JS("table.rows().every(function(i, tab, row) {
                      #               var $this = $(this.node());
                      #               $this.attr('id', this.data()[0]);
                      #               $this.addClass('shiny-input-container');
                      #             });
                      #             Shiny.unbindAll(table.table().node());
                      #             Shiny.bindAll(table.table().node());")
        ) %>%
        DT::formatRound(columns = c("Chemical shift(ppm)", "Half bandwidth (Hz)", "J coupling (Hz)", "J coupling 2 (Hz)",
                                    "Roof effect", "Roof effect 2", "Frequency (MHz)", "pH", "Concentration (mM)",
                                    "Temperature (K)"),
                        digits = 3)
    })
    observeEvent(input$refmet_quant_table_cell_edit, {
      info = input$refmet_quant_table_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value

      ref_quant_table_data$data[i, j] <<- DT::coerceValue(v, ref_quant_table_data$data[i, j])
    })

    # Output (in HTML format) to display the filters that are currently applied to the data.
    output$applied_filters_text2 <- renderUI({
      #browser()
      if(length(attr(xpmt_data(), "filters")) == 0){
        htmltools::HTML("<strong>Currently applied filters:</strong><br/>None")

      } else{
        allfilts <- rlist::list.ungroup(rlist::list.select(attr(xpmt_data(), "filters"), range))
        allfilts <- Reduce("c", lapply(allfilts, function(x){paste0("(", x$min, ", ", x$max, ")")}))
        htmltools::HTML(paste0("<strong>Currently applied filters:</strong><br/>", paste(allfilts, collapse = "<br/>")))

      }
    })

    # Janky solution to get the profiling progress bar to show on the page where the profile button is clicked.
    output$tempsoln <- renderUI({
      req(user_profiling())
      htmltools::HTML("")
    })

    # Plot all collapsed ROIs over the spectrum
    output$quantdata_plot <- plotly::renderPlotly({

      req(isolate({ref_data()}))
      #browser()

      plotly::plot_ly(source = "id_quantdata_plot", type = "scatter", mode = "lines") %>%
        plotly::config(displaylogo = FALSE,
                       modeBarButtons = list(list("select2d"), list("zoom2d"), list("zoomIn2d"),
                                             list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                             list("resetScale2d"), list("toImage"))) %>%
        plotly::layout(title = paste("Experimental Data:", "<br>", "<sup>",
                                     "Annotated Metabolite Peaks within Selected Region(s) of Interest (ROI)", "</sup>"),
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
    observeEvent(c(input$sample_to_plot_quantdat, input$ROI_to_plot_quantdat, xpmt_data()), priority = -1, {
      req(input$sample_to_plot_quantdat)
      req(input$sample_to_plot_quantdat %in% names(xpmt_data()$e_data))
      req(input$ROI_to_plot_quantdat)
      req(ref_data())
      #browser()

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
                                                   "Annotated Metabolite Peaks within Selected Region(s) of Interest (ROI)", "</sup>")))
      # Update shapes/annotations
      temp <- ref_data()$quantdata %>%
        dplyr::mutate(ROI = paste0("(", .data$`ROI right edge (ppm)`, ", ", .data$`ROI left edge (ppm)`, ")")) %>%
        dplyr::filter(.data$ROI %in% input$ROI_to_plot_quantdat)
      temp2 <- ref_data()$user_edited_refdata %>% dplyr::filter(.data$`Chemical shift(ppm)` %in% temp$`Chemical shift(ppm)`)

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
                   #browser()

                   temp <- ref_data()$quantdata %>%
                     dplyr::mutate(ROI = paste0("(", .data$`ROI right edge (ppm)`, ", ", .data$`ROI left edge (ppm)`, ")")) %>%
                     dplyr::filter(.data$ROI %in% input$ROI_to_plot_quantdat)
                   temp2 <- ref_data()$user_edited_refdata %>% dplyr::filter(.data$`Chemical shift(ppm)` %in% temp$`Chemical shift(ppm)`)

                   ROI_lines <- ROI_line_gen(data = temp[!duplicated(temp$`ROI left edge (ppm)`),,drop = FALSE])
                   ROI_annots <- ROI_annot_gen(data = temp2)

                   # Update plot
                   plotly::plotlyProxyInvoke(quantdata_plot_proxy, "relayout",
                                             list(annotations = ROI_annots,
                                                  shapes = ROI_lines))
                 })

    # Visualization options for the plot of ROI-collapsed quantification data
    output$vizoptions_quantdata_ui <- renderUI({

      req(xpmt_data())
      req(ref_data())
      #browser()
      ROIvec <- unique(paste0("(", ref_data()$quantdata$`ROI right edge (ppm)`, ", ", ref_data()$quantdata$`ROI left edge (ppm)`, ")"))

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

        tooltip = shinyWidgets::tooltipOptions(title = "Options")
      )
    })

    #Select input to let users choose whether to run signals as BL Fitting or BL Sum
    output$prof_type_select <- renderUI({
      req(ref_data())
      # #grabing relevant signals
      # temp <- ref_data()$user_edited_refdata[,c("Metabolite", "Quantification Signal", "Quantify")]
      # temp$Signal <- paste0(temp$Metabolite, " [", temp$`Quantification Signal`, "]")
      # temp <- dplyr::filter(temp, Quantify >= 1)

      ROIvec <- unique(paste0("(", ref_data()$quantdata$`ROI right edge (ppm)`, ", ", ref_data()$quantdata$`ROI left edge (ppm)`, ")"))
      #browser()
      selectizeInput(inputId = NS(id, "select_bs_sum"),
                     label = "Choose which signals to profile using Baseline Sum instead of Baseline Fitting.",
                     choices = ROIvec,
                     width = "100%",
                     multiple = TRUE)
    })

    # Dynamic action button to automatically profile reference metabolite data. Only appears after
    # reference metabolite data are uploaded.
    output$ui_auto_profile <- renderUI({
      req(ref_data())
      # clickable button
      shinyWidgets::actionBttn(inputId = NS(id, "auto_profile"),
                               label = "Profile",
                               style = "unite",
                               color = "primary",
                               size = "sm")
    })


    output$ui_global_profiling_parameters <- renderUI({
      req(ref_data())
      #browser()
      if(any(Reduce("c", lapply(ref_data()$global_parameters, is.null)))){
        shinyBS::bsCollapse(id = NS(id, "global_fitting_params"),
                            shinyBS::bsCollapsePanel(title = "▽ Global Profiling Parameters",
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


                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_gaussian"),
                                                                           label    = "Pseudo-Voigt Lineshape Gaussian Ratio (0 < ratio < 1):",
                                                                           value    = 0))
                                                     ),
                                                     fluidRow(
                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_j_coupling_variation"),
                                                                           label    = "J-coupling Tolerance (Hz):",
                                                                           value    = 0.2)),


                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_errorprov"),
                                                                           label    = "Acceptable Fitting Error (%):",
                                                                           value    = 3))

                                                     ),
                                                     fluidRow(
                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_fitting_maxiter"),
                                                                           label    = "Maximum fitting parameter optimization iterations:",
                                                                           value    = 8)),


                                                       # column(width = 6,
                                                       #        numericInput(inputId  = NS(id, "gpp_nls_lm_maxiter"),
                                                       #                     label    = "Max iterations of Levenberg Marquardt (LM) algorithm:",
                                                       #                     value    = 200))

                                                     ),
                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_ftol"),
                                                     #                       label    = "LM algorithm sum of squares error threshold:",
                                                     #                       value    = 1e-06)),
                                                     #
                                                     #
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_ptol"),
                                                     #                       label    = "LM algorithm relative error threshold:",
                                                     #                       value    = 1e-06))
                                                     #
                                                     # ),
                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_factor"),
                                                     #                       label    = "LM algorithm control factor:",
                                                     #                       value    = 0.01)),
                                                     #
                                                     #
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_fitting_maxiterrep"),
                                                     #                       label    = "fitting_maxiterrep:",
                                                     #                       value    = 0))
                                                     #
                                                     # ),
                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_peakdet_minimum"),
                                                     #                       label    = "peakdet_minimum:",
                                                     #                       value    = 0.01))
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


                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_BG_gaussian_percentage"),
                                                                           label    = "BGS Pseudo-Voigt Lineshape Gaussian Ratio (0 < ratio < 1):",
                                                                           value    = 0))

                                                     ),
                                                     fluidRow(
                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_BG_width"),
                                                                           label    = "BGS Bandwidth (Hz):",
                                                                           value    = 8)),


                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_BG_width_tolerance"),
                                                                           label    = "BGS Bandwidth Tolerance (Hz):",
                                                                           value    = 0.25))

                                                     ),

                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_additional_signal_ppm_distance"),
                                                     #                       label    = "additional_signal_ppm_distance:",
                                                     #                       value    = 0.002)),
                                                     #
                                                     #
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_signals_to_add"),
                                                     #                       label    = "signals_to_add:",
                                                     #                       value    = 2))
                                                     #
                                                     # ),
                                                     #
                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_additional_signal_improvement"),
                                                     #                       label    = "additional_signal_improvement:",
                                                     #                       value    = 0.75)),
                                                     #
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_additional_signal_percentage_limit"),
                                                     #                       label    = "additional_signal_percentage_limit:",
                                                     #                       value    = 3))
                                                     #
                                                     # ),
                                                     style = "primary"
                            ))
      } else{
        shinyBS::bsCollapse(id = NS(id, "global_fitting_params"),
                            shinyBS::bsCollapsePanel(title = "▽ Global Profiling Parameters",
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
                                                                           value    = ref_data()$global_parameters$widthtolerance)),


                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_gaussian"),
                                                                           label    = "Pseudo-Voigt Lineshape Gaussian Ratio (0 < ratio < 1):",
                                                                           value    = ref_data()$global_parameters$gaussian))

                                                     ),
                                                     fluidRow(
                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_j_coupling_variation"),
                                                                           label    = "J-coupling Tolerance (Hz):",
                                                                           value    = ref_data()$global_parameters$j_coupling_variation)),


                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_errorprov"),
                                                                           label    = "Acceptable Fitting Error (%):",
                                                                           value    = ref_data()$global_parameters$errorprov))


                                                     ),
                                                     fluidRow(
                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_fitting_maxiter"),
                                                                           label    = "Maximum fitting parameter optimization iterations:",
                                                                           value    = ref_data()$global_parameters$fitting_maxiter)),


                                                       # column(width = 6,
                                                       #        numericInput(inputId  = NS(id, "gpp_nls_lm_maxiter"),
                                                       #                     label    = "Max iterations of Levenberg Marquardt (LM) algorithm:",
                                                       #                     value    = ref_data()$global_parameters$nls_lm_maxiter))


                                                     ),
                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_ftol"),
                                                     #                       label    = "LM algorithm sum of squares error threshold:",
                                                     #                       value    = ref_data()$global_parameters$ftol)),
                                                     #
                                                     #
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_ptol"),
                                                     #                       label    = "LM algorithm relative error threshold:",
                                                     #                       value    = ref_data()$global_parameters$ptol))
                                                     #
                                                     #
                                                     # ),
                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_factor"),
                                                     #                       label    = "LM algorithm control factor:",
                                                     #                       value    = ref_data()$global_parameters$factor)),
                                                     #
                                                     #
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_fitting_maxiterrep"),
                                                     #                       label    = "fitting_maxiterrep:",
                                                     #                       value    = ref_data()$global_parameters$fitting_maxiterrep))
                                                     #
                                                     #
                                                     # ),
                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_peakdet_minimum"),
                                                     #                       label    = "peakdet_minimum:",
                                                     #                       value    = ref_data()$global_parameters$peakdet_minimum))
                                                     #
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
                                                                           value    = ref_data()$global_parameters$BGdensity)),


                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_BG_gaussian_percentage"),
                                                                           label    = "BGS Pseudo-Voigt Lineshape Gaussian Ratio (0 < ratio < 1):",
                                                                           value    = ref_data()$global_parameters$BG_gaussian_percentage))


                                                     ),
                                                     fluidRow(
                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_BG_width"),
                                                                           label    = "BGS Bandwidth (Hz):",
                                                                           value    = ref_data()$global_parameters$BG_width)),


                                                       column(width = 6,
                                                              numericInput(inputId  = NS(id, "gpp_BG_width_tolerance"),
                                                                           label    = "BGS Bandwidth Tolerance (Hz):",
                                                                           value    = ref_data()$global_parameters$BG_width_tolerance))


                                                     ),

                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_additional_signal_ppm_distance"),
                                                     #                       label    = "additional_signal_ppm_distance:",
                                                     #                       value    = ref_data()$global_parameters$additional_signal_ppm_distance)),
                                                     #
                                                     #
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_signals_to_add"),
                                                     #                       label    = "signals_to_add:",
                                                     #                       value    = ref_data()$global_parameters$signals_to_add))
                                                     #
                                                     #
                                                     # ),
                                                     #
                                                     # fluidRow(
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_additional_signal_improvement"),
                                                     #                       label    = "additional_signal_improvement:",
                                                     #                       value    = ref_data()$global_parameters$additional_signal_improvement)),
                                                     #
                                                     #
                                                     #   column(width = 6,
                                                     #          numericInput(inputId  = NS(id, "gpp_additional_signal_percentage_limit"),
                                                     #                       label    = "additional_signal_percentage_limit:",
                                                     #                       value    = ref_data()$global_parameters$additional_signal_percentage_limit))
                                                     #
                                                     #
                                                     # ),
                                                     style = "primary"
                            ))

      }
    })

    rv <- reactiveValues(obs_show_subplot_suspend = TRUE,
                         new_profiling = FALSE)

    plot.data <- reactive({
      req(user_profiling())
      # browser()
      # Create a new directory in the temp directory for each new instance of this trelliscope.
      user_profiling <- user_profiling()
      profiling_data = user_profiling
      signals_to_plot = NULL

      ############################## # Within this chunk is code adapted from format_plotting()
      # plot all metabolites where there's at least one non-missing value
      if (is.null(signals_to_plot)){
        signals_to_plot <- which(apply(profiling_data$final_output$quantification, 2, function(x){all(is.na(x))}) == F)
      }

      # list of number of samples
      p <- vector(mode = "list", length = nrow(profiling_data$final_output$quantification))
      user_profiling$final_output$quantification <- 1
      #user_profiling$final_output$fitting_error <- user_profiling$final_output$fitting_error[which(user_profiling$final_output$fitting_error > 5)]

      # user_profiling$final_output <- user_profiling$final_output[which(user_profiling$final_output$fitting_error > 0.05)]

      plotdataall.out <- list()

      # loops for plotting
      # for each metabolite
      for (ind2 in signals_to_plot) {

        plotdataall.in <- list()

        # for each sample
        for (ind in 1:nrow(profiling_data$final_output$quantification)) {

          # x-axis data
          Xdata <- profiling_data$reproducibility_data[[ind]][[ind2]]$Xdata

          # if no data, move on
          if(is.null(Xdata)){
            next
          }

          # y-axis data
          Ydata <- profiling_data$reproducibility_data[[ind]][[ind2]]$Ydata

          # plot data
          plot_data <- profiling_data$reproducibility_data[[ind]][[ind2]]$plot_data

          # ROI profile
          ROI_profile <- profiling_data$reproducibility_data[[ind]][[ind2]]$ROI_profile

          # combine
          plotdata2 <- data.frame(Xdata        = Xdata,
                                  Ydata        = Ydata,
                                  fitted_sum   = plot_data[3, ] , # fitted_sum
                                  baseline_sum = plot_data[2, ]) # baseline_sum

          # make long
          plotdata3 <- reshape2::melt(plotdata2, id = "Xdata")

          # specify variables
          plotdata3$variable <- c(
            rep('Original Spectrum', length(Ydata)),
            rep('Generated Spectrum', length(Ydata)),
            rep('Generated Background', length(Ydata))
          )

          r <- which(make.names(paste(ROI_profile[,4],ROI_profile[,5],sep='_')) == colnames(profiling_data$final_output$quantification)[ind2])
          if(length(r) == 0){
            next
          }
          plotdata <- data.frame(Xdata, signals = plot_data[3 + r,] )

          # format plotdata3
          plotdata3$variable2 <- plotdata3$variable

          # format plotdata
          colnames(plotdata)[which(colnames(plotdata) == "signals")] <- "value"
          plotdata$variable  <- "Quantified Signal"
          plotdata$variable2 <- "Quantified Signal"

          # combine
          temp                  <- rbind(plotdata, plotdata3)
          temp$Sample           <- rownames(profiling_data$final_output$quantification)[ind]
          plotdataall.in[[ind]] <- temp

        }

        tempdat <- ref_data()$quantdata %>%
          dplyr::mutate(SigName = make.names(paste0(.data$Metabolite, "_", .data$`Quantification Signal`)),
                        Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]"))

        temp2               <- do.call(rbind, plotdataall.in)
        temp2$Signal        <- tempdat$Signal[which(tempdat$SigName == colnames(profiling_data$final_output$quantification)[ind2])]
        plotdataall.out[[ind2]] <- temp2
      }

      plotdataall <- do.call(rbind, plotdataall.out)

      # Used to fix Signal names to be more consistent with UI display formatting
      tempdat <- ref_data()$quantdata %>%
        dplyr::mutate(SigName = make.names(paste0(.data$Metabolite, "_", .data$`Quantification Signal`)),
                      Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]")) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$SigName, .data$Signal)

      # Quantification data
      temp_quantdat <- data.frame(Sample = rownames(profiling_data$final_output$quantification), profiling_data$final_output$quantification) %>%
        tidyr::pivot_longer(-.data$Sample, names_to = "SigName", values_to = "Quantification") %>%
        dplyr::left_join(tempdat, by = "SigName") %>% dplyr::select(-.data$SigName)

      # Signal to Area Ratio
      temp_sardat <- data.frame(Sample = rownames(profiling_data$final_output$signal_area_ratio), profiling_data$final_output$signal_area_ratio) %>%
        tidyr::pivot_longer(-.data$Sample, names_to = "SigName", values_to = "Signal to Area Ratio") %>%
        dplyr::left_join(tempdat, by = "SigName") %>% dplyr::select(-.data$SigName)

      # Fitting Error
      temp_fedat <- data.frame(Sample = rownames(profiling_data$final_output$fitting_error), profiling_data$final_output$fitting_error) %>%
        tidyr::pivot_longer(-.data$Sample, names_to = "SigName", values_to = "Fitting Error") %>%
        dplyr::left_join(tempdat, by = "SigName") %>% dplyr::select(-.data$SigName)

      # Chemical Shift
      temp_csdat <- data.frame(Sample = rownames(profiling_data$final_output$chemical_shift), profiling_data$final_output$chemical_shift) %>%
        tidyr::pivot_longer(-.data$Sample, names_to = "SigName", values_to = "Chemical Shift") %>%
        dplyr::left_join(tempdat, by = "SigName") %>% dplyr::select(-.data$SigName)

      # Intensity
      temp_intdat <- data.frame(Sample = rownames(profiling_data$final_output$intensity), profiling_data$final_output$intensity) %>%
        tidyr::pivot_longer(-.data$Sample, names_to = "SigName", values_to = "Intensity") %>%
        dplyr::left_join(tempdat, by = "SigName") %>% dplyr::select(-.data$SigName)

      # Half bandwidth
      temp_hwdat <- data.frame(Sample = rownames(profiling_data$final_output$half_bandwidth), profiling_data$final_output$half_bandwidth) %>%
        tidyr::pivot_longer(-.data$Sample, names_to = "SigName", values_to = "Half Bandwidth") %>%
        dplyr::left_join(tempdat, by = "SigName") %>% dplyr::select(-.data$SigName)


      # Add Quantification, Signal to Area Ratio, Fitting Error, Chemical Shift, Intensity, Half Bandwidth
      plot.data <- plotdataall %>% dplyr::full_join(temp_quantdat, by = c("Sample", "Signal")) %>%
        dplyr::full_join(temp_sardat, by = c("Sample", "Signal")) %>%
        dplyr::full_join(temp_fedat, by = c("Sample", "Signal")) %>%
        dplyr::full_join(temp_csdat, by = c("Sample", "Signal")) %>%
        dplyr::full_join(temp_intdat, by = c("Sample", "Signal")) %>%
        dplyr::full_join(temp_hwdat, by = c("Sample", "Signal"))

      return(plot.data)
    })
    # ##############################
    plot.data2 <- reactive({
      # trel_fethresh <- input$trel_fethresh
      plot.data <- plot.data()
      # #Grabbing signals that were profiled with Baseline Sum, since they have "NA" fitting error
      # plot.data_BLS <- plot.data[which(is.na(plot.data$`Fitting Error`)),]
      # plot.data <- plot.data[which(plot.data$`Fitting Error` >= trel_fethresh[1] & plot.data$`Fitting Error` <= trel_fethresh[2]),]
      # #Adding Baseline Sum profiled signals
      # plot.data <- rbind(plot.data, plot.data_BLS)
      plot.data
    })
    plot.data3 <- reactive({
      # browser()
      plot.data2 <- plot.data2()
      signal <- input$trel_sign
      filt_range_low <-input$trel_fethresh[1]
      filt_range_high <- input$trel_fethresh[2]
      shown_prof_type <- isolate(input$shown_prof_type)
      if(shown_prof_type == "Baseline Fitting"){
        choices2 <- unique(plot.data2$Sample[which(plot.data2$`Fitting Error` >= filt_range_low & plot.data2$`Fitting Error` <= filt_range_high & signal == plot.data2$Signal)])
        updateSelectizeInput(session, "trel_samp",
                     choices = choices2,
                     selected = choices2[1:6])
      }
      plot.data2
    })
      
    output$trelliscope <- renderPlot({
      
      plot.data3 <- plot.data3()
      filter_plot <- input$filter_plot
      signal <- input$trel_sign
      sample <- input$trel_samp
      req(!is.null(input$trel_sign))
      
      if(length(sample) == 0){
        shinyWidgets::show_alert(
          title = "No applicable graphs",
          text = "There are no applicable graphs based on the filters selected. Please change the settings and try again.",
          type = "error"
        )
      }
      req(length(sample) > 0)
      
      if(length(sample) > 6){
        for(i in 1:(6 - length(sample)))
          sample <- c(sample, NA)
      }
      
      filt_samp <- plot.data3 %>% dplyr::filter(plot.data3$Signal == signal)

      filt_temp <- filt_samp %>% dplyr::filter(filt_samp$Sample == sample[1])
      p1 <- ggplot2::ggplot(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color = .data$variable))+
              ggplot2::geom_line() +
              ggplot2::geom_line(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color= .data$variable), alpha = 0.5)+
              ggplot2::xlab('PPM') + ggplot2::ylab('Intensity') + ggplot2::theme_bw() + ggplot2::ggtitle(paste0(sample[1], "  Fitting Error: ", filt_temp$`Fitting Error`[1]))

      filt_temp <- filt_samp %>% dplyr::filter(filt_samp$Sample == sample[2])
      p2 <- ggplot2::ggplot(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color = .data$variable))+
        ggplot2::geom_line() +
        ggplot2::geom_line(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color= .data$variable), alpha = 0.5)+
        ggplot2::xlab('PPM') + ggplot2::ylab('Intensity') + ggplot2::theme_bw() + ggplot2::ggtitle(paste0(sample[2], "  Fitting Error: ", filt_temp$`Fitting Error`[1]))

      filt_temp <- filt_samp %>% dplyr::filter(filt_samp$Sample == sample[3])
      p3 <- ggplot2::ggplot(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color = .data$variable))+
        ggplot2::geom_line() +
        ggplot2::geom_line(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color= .data$variable), alpha = 0.5)+
        ggplot2::xlab('PPM') + ggplot2::ylab('Intensity') + ggplot2::theme_bw() + ggplot2::ggtitle(paste0(sample[3], "  Fitting Error: ", filt_temp$`Fitting Error`[1]))

      filt_temp <- filt_samp %>% dplyr::filter(filt_samp$Sample == sample[4])
      p4 <- ggplot2::ggplot(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color = .data$variable))+
        ggplot2::geom_line() +
        ggplot2::geom_line(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color= .data$variable), alpha = 0.5)+
        ggplot2::xlab('PPM') + ggplot2::ylab('Intensity') + ggplot2::theme_bw() + ggplot2::ggtitle(paste0(sample[4], "  Fitting Error: ", filt_temp$`Fitting Error`[1]))

      filt_temp <- filt_samp %>% dplyr::filter(filt_samp$Sample == sample[5])
      p5 <- ggplot2::ggplot(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color = .data$variable))+
        ggplot2::geom_line() +
        ggplot2::geom_line(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color= .data$variable), alpha = 0.5)+
        ggplot2::xlab('PPM') + ggplot2::ylab('Intensity') + ggplot2::theme_bw() + ggplot2::ggtitle(paste0(sample[5], "  Fitting Error: ", filt_temp$`Fitting Error`[1]))

      filt_temp <- filt_samp %>% dplyr::filter(filt_samp$Sample == sample[6])
      p6 <- ggplot2::ggplot(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color = .data$variable))+
        ggplot2::geom_line() +
        ggplot2::geom_line(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color= .data$variable), alpha = 0.5)+
        ggplot2::xlab('PPM') + ggplot2::ylab('Intensity') + ggplot2::theme_bw() + ggplot2::ggtitle(paste0(sample[6], "  Fitting Error: ", filt_temp$`Fitting Error`[1]))

      g_legend<-function(a.gplot){
        tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}

      mylegend<-g_legend(p1)

      # gridExtra::grid.arrange(gridExtra::arrangeGrob(p1 + ggplot2::theme(legend.position="none"),
      #                                                p2 + ggplot2::theme(legend.position="none"),
      #                                                p3 + ggplot2::theme(legend.position="none"),
      #                                                p4 + ggplot2::theme(legend.position="none"),
      #                                                p5 + ggplot2::theme(legend.position="none"),
      #                                                p6 + ggplot2::theme(legend.position="none"),
      #                                                ncol = 2))

      gridExtra::grid.arrange(gridExtra::arrangeGrob(p1 + ggplot2::theme(legend.position="none"),
                                          p2 + ggplot2::theme(legend.position="none"),
                                          p3 + ggplot2::theme(legend.position="none"),
                                          p4 + ggplot2::theme(legend.position="none"),
                                          p5 + ggplot2::theme(legend.position="none"),
                                          p6 + ggplot2::theme(legend.position="none"),
                                          ncol = 2), mylegend, ncol=2, widths=c(10, 2))

      # plist <- list()
      # for(i in 1:num_samps){
      #   filt_temp <- filt_samp %>% dplyr::filter(filt_samp$Sample == sample[i])
      #
      #   if(!length(filt_temp[,1]) == 0){
      #
      #     p <- ggplot2::ggplot(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color = .data$variable))+
      #       ggplot2::geom_line() +
      #       ggplot2::geom_line(data = filt_temp, ggplot2::aes(x = Xdata, y = .data$value, color= .data$variable), alpha = 0.5)+
      #       ggplot2::xlab('PPM') + ggplot2::ylab('Intensity') + ggplot2::theme_bw() + ggplot2::ggtitle(sample[i])
      #     plist <- append(plist, ggplotify::as.ggplot(p))
      #   }}
      # cowplot::plot_grid(plotlist = plist, ncol = 2)
      # plot.data <- plot.data %>%
      #   tidyr::nest(data = !tidyselect::one_of(c("Sample", "Signal"))) %>%
      #   dplyr::mutate(
      #     Quantification = purrr::map_dbl(.data$data, ~ unique(.$Quantification)),
      #     Signal_Area_Ratio = purrr::map_dbl(.data$data, ~ unique(.$`Signal to Area Ratio`)),
      #     Fitting_Error = purrr::map_dbl(.data$data, ~ unique(.$`Fitting Error`)),
      #     Chemical_Shift = purrr::map_dbl(.data$data, ~ unique(.$`Chemical Shift`)),
      #     Intensity = purrr::map_dbl(.data$data, ~ unique(.$Intensity)),
      #     Half_Bandwidth = purrr::map_dbl(.data$data, ~ unique(.$`Half Bandwidth`)),
      #     panel = lapply(.data$data, function(x){
      #       tempdat <- x %>% dplyr::filter(.data$variable != "Quantified Signal")
      #       ggplot2::ggplot(data = tempdat, ggplot2::aes(x = Xdata, y = .data$value, color = .data$variable))+
      #         ggplot2::geom_line() +
      #         ggplot2::geom_line(data = tempdat, ggplot2::aes(x = Xdata, y = .data$value, color= .data$variable), alpha = 0.5)+
      #         ggplot2::xlab('PPM') + ggplot2::ylab('Intensity') + ggplot2::theme_bw() #+ ggplot2::theme(legend.position = "none")
      #     })
      #   ) #%>%
        # trelliscopejs::trelliscope(name           = "Results",
        #                            path           = treldir,
        #                            self_contained = TRUE,

    })

    # complete_profres_tab
    output$complete_profres_tab <- DT::renderDataTable(server = FALSE, {

      req(user_profiling())
      #browser()
      user_profiling <- user_profiling()

      tempdat_quant <- as.data.frame(user_profiling$final_output$quantification) %>%
        dplyr::mutate(Sample = rownames(.)) %>%
        tidyr::pivot_longer(names_to  = "Metabolite",
                            values_to = "Quantity",
                            -.data$Sample)

      tempdat_sar <- as.data.frame(user_profiling$final_output$signal_area_ratio) %>%
        dplyr::mutate(Sample = rownames(.)) %>%
        tidyr::pivot_longer(names_to  = "Metabolite",
                            values_to = "Signal to Area Ratio",
                            -.data$Sample)

      tempdat_err <- as.data.frame(user_profiling$final_output$fitting_error) %>%
        dplyr::mutate(Sample = rownames(.)) %>%
        tidyr::pivot_longer(names_to  = "Metabolite",
                            values_to = "Fitting Error",
                            -.data$Sample)

      tempdat_cs <- as.data.frame(user_profiling$final_output$chemical_shift) %>%
        dplyr::mutate(Sample = rownames(.)) %>%
        tidyr::pivot_longer(names_to  = "Metabolite",
                            values_to = "Fitted Chemical Shift (ppm)",
                            -.data$Sample)

      tempdat_int <- as.data.frame(user_profiling$final_output$intensity) %>%
        dplyr::mutate(Sample = rownames(.)) %>%
        tidyr::pivot_longer(names_to  = "Metabolite",
                            values_to = "Fitted Intensity",
                            -.data$Sample)

      tempdat_hbw <- as.data.frame(user_profiling$final_output$half_bandwidth) %>%
        dplyr::mutate(Sample = rownames(.)) %>%
        tidyr::pivot_longer(names_to  = "Metabolite",
                            values_to = "Fitted Half Bandwidth (Hz)",
                            -.data$Sample)

      tempdat <- dplyr::left_join(tempdat_quant, tempdat_err, by = c("Sample", "Metabolite"))
      tempdat <- dplyr::left_join(tempdat, tempdat_sar, by = c("Sample", "Metabolite"))
      tempdat <- dplyr::left_join(tempdat, tempdat_cs, by = c("Sample", "Metabolite"))
      tempdat <- dplyr::left_join(tempdat, tempdat_int, by = c("Sample", "Metabolite"))
      tempdat <- dplyr::left_join(tempdat, tempdat_hbw, by = c("Sample", "Metabolite"))


      quant_refdata <- ref_data()$user_edited_refdata %>% dplyr::filter(.data$Quantify == 1)
      qmetnames <- unique(quant_refdata$Metabolite)
      qmet_signums <- sub(".*_", "", tempdat$Metabolite)

      for(name in qmetnames){
        signums <- qmet_signums[grepl(make.names(name), tempdat$Metabolite)]
        tempdat$Metabolite[grepl(make.names(name), tempdat$Metabolite)] <- paste0(name, " [", signums, "]")
      }

      refdat_info <- ref_data()$user_edited_refdata %>% dplyr::ungroup() %>%
        dplyr::mutate(SigName = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]")) %>%
        dplyr::select(.data$`SigName`, .data$`ROI left edge (ppm)`, .data$`ROI right edge (ppm)`, .data$`Chemical shift(ppm)`,
                      .data$`Chemical shift tolerance (ppm)`, .data$`Half bandwidth (Hz)`, .data$`Multiplicity`,
                      .data$`J coupling (Hz)`, .data$`J coupling 2 (Hz)`, .data$`Roof effect`, .data$`Roof effect 2`) %>%
        dplyr::rename(Metabolite = .data$SigName)

      tempdat <- dplyr::left_join(tempdat, refdat_info, by = "Metabolite")

      tempdat %>% dplyr::select(.data$Sample, .data$Metabolite, .data$`Fitted Chemical Shift (ppm)`, .data$`Fitted Intensity`,
                                .data$`Fitted Half Bandwidth (Hz)`, .data$`Quantity`, .data$`Fitting Error`,
                                .data$`Signal to Area Ratio`,
                                .data$`ROI left edge (ppm)`, .data$`ROI right edge (ppm)`, .data$`Chemical shift(ppm)`,
                                .data$`Chemical shift tolerance (ppm)`, .data$`Half bandwidth (Hz)`, .data$`Multiplicity`,
                                .data$`J coupling (Hz)`, .data$`J coupling 2 (Hz)`, .data$`Roof effect`, .data$`Roof effect 2`) %>%
        DT::datatable(rownames   = FALSE,
                      filter = "top",
                      extensions = c("Buttons", "Scroller"),
                      options = exprToFunction(
                        list(dom = 'Bfrtip',
                             buttons = list(
                               list(extend = 'csv', text = "Download Current Page (CSV)",
                                    filename =  paste0(lubridate::year(lubridate::now()), "-",
                                                       lubridate::month(lubridate::now()), "-",
                                                       lubridate::day(lubridate::now()), "_",
                                                       "Detailed_Profiling_Results.csv"),
                                    exportOptions = list(
                                      modifier = list(page = "current")
                                    )),
                               # list(extend = 'excel', text = "Download Current Page (XLSX)",
                               #      filename =  paste0(lubridate::year(lubridate::now()), "-",
                               #                         lubridate::month(lubridate::now()), "-",
                               #                         lubridate::day(lubridate::now()), "_",
                               #                         "Detailed_Profiling_Results_page.xlsx"),
                               #      exportOptions = list(
                               #        modifier = list(page = "current")
                               #      )),
                               list(extend = 'csv', text = "Download Full Results (CSV)",
                                    filename =  paste0(lubridate::year(lubridate::now()), "-",
                                                       lubridate::month(lubridate::now()), "-",
                                                       lubridate::day(lubridate::now()), "_",
                                                       "Detailed_Profiling_Results.csv"),
                                    exportOptions = list(
                                      modifier = list(page = "all")
                                    ))#,
                               # list(extend = 'excel', text = "Download Full Results (XLSX)",
                               #      filename =  paste0(lubridate::year(lubridate::now()), "-",
                               #                         lubridate::month(lubridate::now()), "-",
                               #                         lubridate::day(lubridate::now()), "_",
                               #                         "Detailed_Profiling_Results_full.xlsx"),
                               #      exportOptions = list(
                               #        modifier = list(page = "all")
                               #      ))
                             ),
                             scrollX = TRUE)
                      ),
                      class = "display") %>%
        DT::formatRound(columns = c("Fitted Chemical Shift (ppm)", "Fitted Intensity", "Fitted Half Bandwidth (Hz)",
                                    "Quantity", "Fitting Error", "Signal to Area Ratio",
                                    "ROI left edge (ppm)", "ROI right edge (ppm)", "Chemical shift(ppm)",
                                    "Chemical shift tolerance (ppm)", "Half bandwidth (Hz)",
                                    "J coupling (Hz)", "J coupling 2 (Hz)", "Roof effect", "Roof effect 2"),
                        digits = 3)

    })

    output$profvizoptions_ui <- renderUI({

      req(xpmt_data())
      req(ref_data())
      #browser()

      quant_refdata <- ref_data()$user_edited_refdata %>% dplyr::filter(.data$Quantify == 1)
      qmetnames <- unique(quant_refdata$Metabolite)

      shinyWidgets::dropdownButton(
        # Allows users to select which sample spectrum to display.
        selectInput(inputId = NS(id, "sample_to_plot"),
                    label   = "Choose a spectrum to plot",
                    choices = colnames(xpmt_data()$e_data)[-1]),

        selectInput(inputId = NS(id, "refmet_to_plot"),
                    label   = "Select the metabolite fit(s) to view:",
                    choices = qmetnames),

        actionButton(inputId = NS(id, "view_detailed"),
                     label = "View"),
        h4(),

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

        # HTML output to display the filters currently applied
        htmlOutput(NS(id, "applied_filters_text")),

        circle = TRUE, status = "info",
        icon = icon("cog"), width = "300px",

        tooltip = shinyWidgets::tooltipOptions(title = "Plot Options")
      )
    })

    output$graph_switch_ui <- renderUI({

      req(xpmt_data())
      req(ref_data())
      plot.data <- plot.data()
      # req(!is.null(plot.data))
      #browser()
      # Allows users to select which sample spectrum to display.
        selectInput(inputId = NS(id, "shown_prof_type"),
                    label   = "Show Baseline Fitting or Baseline Sum metabolite Outputs",
                    choices = c("Baseline Fitting", "Baseline Sum"),
                    selected = "Baseline Fitting")
    })
    
    output$spectra_ui <- renderUI({

      req(xpmt_data())
      req(ref_data())
      isolate(plot.data2 <- plot.data2())
      req(!is.null(plot.data2))
      req(input$shown_prof_type)
      #browser()
      # Allows users to select which sample spectrum to display.
      if(input$shown_prof_type == "Baseline Fitting"){
        plot.data2 <- plot.data2[which(!is.na(plot.data2$`Fitting Error`)),]
        fluidRow(
          column(3,
                 shinyWidgets::numericRangeInput(inputId = NS(id, "trel_fethresh"),
                                                 label = "Specify a Fitting Error range:",
                                                 value = c(0.5,100))),
          column(3,
                 selectizeInput(inputId = NS(id, "trel_sign"),
                             label   = "Select a signal",
                             choices = unique(plot.data2$Signal),
                             selected = unique(plot.data2$Signal)[1],
                             multiple = F,
                             size = 1),
          ),
          column(3,
                 selectizeInput(inputId = NS(id, "trel_samp"),
                                                 label   = "Select a sample ",
                                                 choices = unique(plot.data2$Sample),
                                                 selected = unique(plot.data2$Sample),
                                                 multiple = T,
                                                 size = 8)),
          column(3,
            actionButton(inputId = NS(id, "filter_plot"),
                         label = "Apply Filter",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
      } else{
        fluidRow(
          column(4,
                 selectInput(inputId = NS(id, "trel_sign"),
                             label   = "Select a signal",
                             choices = unique(plot.data2$Signal[which(is.na(plot.data2$`Fitting Error`))],),
                             selected = unique(plot.data2$Signal)[1]),
          ),
          column(4,
                 selectizeInput(inputId = NS(id, "trel_samp"),
                                label   = "Select a sample ",
                                choices = unique(plot.data2$Sample),
                                selected = unique(plot.data2$Sample),
                                multiple = T,
                                size = 8)),
          column(4,
                 actionButton(inputId = NS(id, "filter_plot"),
                              label = "Apply Filter",
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
      }})


    # Output (in HTML format) to display the filters that are currently applied to the data.
    output$applied_filters_text <- renderUI({
      #browser()
      if(length(attr(xpmt_data(), "filters")) == 0){
        htmltools::HTML("<strong>Currently applied filters:</strong><br/>None")

      } else{
        allfilts <- rlist::list.ungroup(rlist::list.select(attr(xpmt_data(), "filters"), range))
        allfilts <- Reduce("c", lapply(allfilts, function(x){paste0("(", x$min, ", ", x$max, ")")}))
        htmltools::HTML(paste0("<strong>Currently applied filters:</strong><br/>", paste(allfilts, collapse = "<br/>")))

      }
    })

    output$prof_refmet_view_plot <- plotly::renderPlotly({

      req(user_profiling())

      req(xpmt_data())
      req(ref_data())
      #browser()
      input$view_detailed

      if(rv$new_profiling){
        input$refmet_to_plot
      }

      isolate({
        profiling_data <- user_profiling()
        req(input$sample_to_plot)
        req(input$sample_to_plot %in% names(xpmt_data()$e_data))
        req(input$refmet_to_plot)

        # Extract indices of reproducibility data that correspond to selected sample and metabolite
        selsamp_ind <- which(rownames(profiling_data$final_output$quantification) == input$sample_to_plot)
        selmet_inds <- which(grepl(make.names(input$refmet_to_plot), colnames(profiling_data$final_output$quantification)))

        req(length(selmet_inds) > 0)

        rv$new_profiling <- FALSE

        ROI_plots <- vector("list", length = length(selmet_inds))
        for(i in 1:length(selmet_inds)){

          tempdat <- profiling_data$reproducibility_data[[selsamp_ind]][[selmet_inds[i]]]

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
            colSums(tempdat$plot_data[which(grepl(make.names(input$refmet_to_plot), rownames(tempdat$plot_data))),,drop = FALSE])
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
        ROI_data <- ref_data()$user_edited_refdata %>% dplyr::filter(.data$Metabolite == input$refmet_to_plot, .data$Quantify == 1)
        ROI_collapsed_data <- ref_data()$quantdata[!duplicated(ref_data()$quantdat$`ROI left edge (ppm)`),] %>%
          dplyr::filter(.data$Metabolite == input$refmet_to_plot, .data$Quantify == 1)

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
        for(i in 1:nrow(ROI_collapsed_data)){

          ROI_line[["x0"]]        <- ROI_collapsed_data[i,,drop = FALSE]$"ROI left edge (ppm)"
          ROI_line[["x1"]]        <- ROI_collapsed_data[i,,drop = FALSE]$"ROI right edge (ppm)"
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
          ROI_annot[["text"]]      <- paste0(sprintf("<b>%s</b>", paste0(ROI_data[i,,drop = FALSE]$Metabolite,
                                                                         " [", ROI_data[i,,drop = FALSE]$`Quantification Signal`,
                                                                         "]: ")),
                                             ROI_data[i,,drop = FALSE]$"Chemical shift(ppm)", " (",
                                             ROI_data[i,,drop = FALSE]$"ROI left edge (ppm)", ", ",
                                             ROI_data[i,,drop = FALSE]$"ROI right edge (ppm)", ")", " <br> ",
                                             sprintf("<b>%s</b>", "Quantification: "), round(profiling_data$final_output$quantification[selsamp_ind, selmet_inds[i]],3), " <br> ",
                                             sprintf("<b>%s</b>", "Signal to Area Ratio: "), round(profiling_data$final_output$signal_area_ratio[selsamp_ind, selmet_inds[i]],3), " <br> ",
                                             sprintf("<b>%s</b>", "Fitting Error: "), round(profiling_data$final_output$fitting_error[selsamp_ind, selmet_inds[i]],3))
          ROI_annot[["arrowsize"]] <- ROI_data[i,,drop = FALSE]$"Chemical shift tolerance (ppm)"
          ROI_annot[["showarrow"]] <- TRUE
          ROI_annots               <- c(ROI_annots, list(ROI_annot))
        }


        xpmt_data_sample <- xpmt_data()$e_data %>% dplyr::select(.data$PPM, .data[[input$sample_to_plot]])
        df_long <- xpmt_data_sample %>%
          tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")

        # Code to resume the observer that was started in a suspended state. We also update the value of
        # rv$obs_show_subplot_suspend so that $resume() is not called every time this plot is rendered,
        # but only after the first rendering of the plot.
        if(rv$obs_show_subplot_suspend){
          obs_show_subplot$resume()
          rv$obs_show_subplot_suspend <- FALSE
        }

        ROI_plots %>% dplyr::group_by(.data$ROI) %>%
          plotly::plot_ly(source = "id_prof_refmet_view_plot", type = "scatter", mode = "lines") %>%
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
      })


    })

    # Plotting of the subplot of ppm data across all sample spectra at the selected region
    output$prof_refmet_view_subplot <- plotly::renderPlotly({

      req(input$show_subplot)
      req(user_profiling())
      #browser()
      isolate({
        req(xpmt_data())
        req(ref_data())
        req(input$sample_to_plot)
        req(input$sample_to_plot %in% names(xpmt_data()$e_data))
        req(input$refmet_to_plot)
      })

      brushedData <- plotly::event_data("plotly_brushed", source = "id_prof_refmet_view_plot")
      #browser()
      if(is.null(brushedData)){
        return(NULL)
      }

      isolate({

        profiling_data <- user_profiling()

        # Extract indices of reproducibility data that correspond to selected sample and metabolite
        selsamp_ind <- which(rownames(profiling_data$final_output$quantification) == input$sample_to_plot)
        selmet_inds <- which(grepl(make.names(input$refmet_to_plot), colnames(profiling_data$final_output$quantification)))
        selmet_names <- colnames(profiling_data$final_output$quantification)[selmet_inds]

        ROI_plots <- vector("list", length = length(selmet_inds))
        for(i in 1:length(selmet_inds)){

          tempdat <- profiling_data$reproducibility_data[[selsamp_ind]][[selmet_inds[i]]]

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
            colSums(tempdat$plot_data[which(grepl(selmet_names[i], rownames(tempdat$plot_data))),,drop = FALSE])
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
        ROI_data <- ref_data()$user_edited_refdata %>% dplyr::filter(.data$Metabolite == input$refmet_to_plot,
                                                                     .data$Quantify == 1,
                                                                     .data$`Chemical shift(ppm)` >= min(brushedData$x),
                                                                     .data$`Chemical shift(ppm)` <= max(brushedData$x))
        ROI_collapsed_data <- ref_data()$quantdata %>%
          dplyr::filter(.data$Metabolite == input$refmet_to_plot, .data$Quantify == 1,
                        .data$`Chemical shift(ppm)` >= min(brushedData$x),
                        .data$`Chemical shift(ppm)` <= max(brushedData$x)) %>%
          dplyr::filter(!duplicated(.data$`ROI left edge (ppm)`))

        if(nrow(ROI_collapsed_data) !=0){

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
          for(i in 1:nrow(ROI_collapsed_data)){

            ROI_line[["x0"]]        <- ROI_collapsed_data[i,,drop = FALSE]$"ROI left edge (ppm)"
            ROI_line[["x1"]]        <- ROI_collapsed_data[i,,drop = FALSE]$"ROI right edge (ppm)"
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
            ROI_annot[["text"]]      <- paste0(sprintf("<b>%s</b>", paste0(ROI_data[i,,drop = FALSE]$Metabolite,
                                                                           " [", ROI_data[i,,drop = FALSE]$`Quantification Signal`,
                                                                           "]: ")),
                                               ROI_data[i,,drop = FALSE]$"Chemical shift(ppm)", " (",
                                               ROI_data[i,,drop = FALSE]$"ROI left edge (ppm)", ", ",
                                               ROI_data[i,,drop = FALSE]$"ROI right edge (ppm)", ")", " <br> ",
                                               sprintf("<b>%s</b>", "Quantification: "), round(profiling_data$final_output$quantification[selsamp_ind, selmet_inds[i]],3), " <br> ",
                                               sprintf("<b>%s</b>", "Signal to Area Ratio: "), round(profiling_data$final_output$signal_area_ratio[selsamp_ind, selmet_inds[i]],3), " <br> ",
                                               sprintf("<b>%s</b>", "Fitting Error: "), round(profiling_data$final_output$fitting_error[selsamp_ind, selmet_inds[i]],3))
            ROI_annot[["arrowsize"]] <- ROI_data[i,,drop = FALSE]$"Chemical shift tolerance (ppm)"
            ROI_annot[["showarrow"]] <- TRUE
            ROI_annots               <- c(ROI_annots, list(ROI_annot))
          }

          # xpmt_data_sample <- xpmt_data()$e_data %>% dplyr::select(.data$PPM, .data[[input$sample_to_plot]])
          df_long <- xpmt_data()$e_data %>%
            tidyr::pivot_longer(!.data$PPM, names_to = "Sample", values_to = "Intensity")
          df_long <- df_long %>% dplyr::filter(.data$PPM >= min(brushedData$x), .data$PPM <= max(brushedData$x))
          df_long_selsamp <- df_long %>% dplyr::filter(.data$Sample == input$sample_to_plot)
          df_long_nonselsamp <- df_long %>% dplyr::filter(.data$Sample != input$sample_to_plot)

          # Include only the signals within the selected range.
          tempdat <- ref_data()$user_edited_refdata %>%
            dplyr::mutate(SigName = make.names(paste0(.data$Metabolite, "_", .data$`Quantification Signal`)),
                          Signal = paste0(.data$Metabolite, " [", .data$`Quantification Signal`, "]")) %>%
            dplyr::filter(.data$`Chemical shift(ppm)` >= min(brushedData$x) & .data$`Chemical shift(ppm)` <= max(brushedData$x)) %>%
            dplyr::filter(grepl(make.names(input$refmet_to_plot), .data$Metabolite))

          if(nrow(tempdat) != 0){

            selmet_inds2 <- which(selmet_names %in% tempdat$SigName)

            ROI_plots2 <- ROI_plots %>% dplyr::filter(.data$ROI %in% selmet_inds2)


            ROI_plots2 %>% dplyr::group_by(.data$ROI) %>%
              plotly::plot_ly(source = "id_prof_refmet_view_subplot", type = "scatter", mode = "lines") %>%
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
              plotly::add_trace(x    = df_long_selsamp$PPM,
                                y    = df_long_selsamp$Intensity,
                                name = input$sample_to_plot,
                                type = 'scatter',
                                mode = 'lines',
                                line = list(width = 1.3),
                                hoverinfo = "text",
                                hovertext = paste0("Sample: ", input$sample_to_plot,
                                                   "\nPPM: ", round(df_long_selsamp$PPM, 4),
                                                   "\nIntensity: ", round(df_long_selsamp$Intensity, 4))) %>%
              plotly::add_trace(x    = df_long_nonselsamp$PPM,
                                y    = df_long_nonselsamp$Intensity,
                                name = ~df_long_nonselsamp$Sample,
                                type = 'scatter',
                                opacity = 0.3, mode = "lines", line = list(color = "#000000", width = 0.75),
                                hoverinfo = "text", hovertext = paste0("Sample: ", df_long_nonselsamp$Sample,
                                                                       "\nPPM: ", round(df_long_nonselsamp$PPM, 4),
                                                                       "\nIntensity: ", round(df_long_nonselsamp$Intensity, 4)),
                                showlegend = FALSE) %>%
              plotly::add_trace(x = ROI_plots2$PPM,
                                y = ROI_plots2$Intensity,
                                name = ~ROI_plots2$Intensity_Type,
                                type = "scatter",
                                mode = "lines",
                                line = list(color = ~ROI_plots2$Intensity_Type,
                                            width = 1))
          } else{

            plot_xpmt_data(xpmt_data      = xpmt_data()$e_data,
                           sourceid       = "id_prof_refmet_view_subplot",
                           sample_to_plot = input$sample_to_plot,
                           brushed_data   = brushedData)
          }


        } else{
          plot_xpmt_data(xpmt_data      = xpmt_data()$e_data,
                         sourceid       = "id_prof_refmet_view_subplot",
                         sample_to_plot = input$sample_to_plot,
                         brushed_data   = brushedData)
        }


      })

    })


    # Respond to the upload button being pressed and append the user defined
    # reference data to the db

    # observeEvent(input$upload_ref_data_db, {
    #
    #   req(input$upload_ref_data_db>0)
    #   user.name <- Sys.getenv(c("USERNAME"))
    #   timestamp <- Sys.time()
    #
    #   df <- ref_data()$user_edited_refdata
    #   df['user'] <- user.name
    #   df['session'] <- timestamp
    #
    #   df['proposal_number'] <- attr(xpmt_data(), "session_info")$proposal_num
    #   df['PI_name'] <- attr(xpmt_data(), "session_info")$proposal_num
    #   df['project_name'] <- attr(xpmt_data(), "session_info")$proposal_num
    #
    #   #append the project information (PI name, project num, project name)
    #
    #
    #   # connect to db table
    #   #create_new_table(connec(), "profiling_parameters", df)
    #   append_table(db_connection= connec(), table_name="profiling_parameters", df_object=df)
    #
    #   # add pop up to let the user know the entry has been added to the database
    #   removeModal()
    #   showModal(
    #     modalDialog(
    #       title = "Profiling parameters have been added to the database server.",
    #       size = "xl",
    #       easyClose = TRUE,
    #       fade = FALSE
    #     ))
    # })



    # Observer to control pop-up (i.e. modal) containing the subplot of spectral data at a selected region.
    # Note: This works fine, but the only thing that I would like to change is
    # loading of subsequent plots generated by different brush events.
    # On initial plot, the loading spinner shows, but on subsequent plots, it does not.
    # Not sure how to fix this yet, but I suspect the issue lies in the execution order.
    # This observer triggers before "e_data_subplot" invalidates.
    obs_show_subplot <- observeEvent(plotly::event_data("plotly_brushed", source = "id_prof_refmet_view_plot"), suspended = TRUE, {
      req(input$show_subplot)


      brushedData <- plotly::event_data("plotly_brushed", source = "id_prof_refmet_view_plot")

      req(!identical(brushedData, rv$subplot_dat))

      removeModal()
      showModal(
        modalDialog(
          shinycssloaders::withSpinner(plotly::plotlyOutput(NS(id, 'prof_refmet_view_subplot'))),
          title = paste0("All Sample Spectra: ", round(min(brushedData$x),3)," PPM to ", round(max(brushedData$x),3), " PPM"),
          size = "xl",
          easyClose = TRUE,
          fade = FALSE
        ))
      rv$subplot_dat <- brushedData
    })

    user_profiling <- eventReactive(input$profile_confirm,{
      req(xpmt_data())
      req(ref_data())
      req(input$profile_confirm)
      #browser()

      shinyWidgets::progressSweetAlert(
        session = shiny::getDefaultReactiveDomain(),
        id = "profiling_progress",
        value = 0, title = "",
        display_pct = TRUE, striped = TRUE, status = "info"
      )

      shinyWidgets::updateProgressBar(
        session = shiny::getDefaultReactiveDomain(),
        id = "profiling_progress",
        title = "Importing and processing necessary data to begin the profiling...",
        value = 0
      )

      #formats the data object
      imported_data <- ppmData_to_rDolphin(ppmData = xpmt_data(),
                                           metabs  = ref_data()$quantdata %>% dplyr::filter(.data$Quantify == 1))
      # In the above (metabs), we exclude the peaks designated by the user to remove from quantification (Quantify == 0).
      # Replace "program_parameters" with those specified by the user.
      imported_data$program_parameters <- list(BGdensity                          = input$gpp_BGdensity,
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


      ROI_data           = imported_data$ROI_data
      optimization       = FALSE # Set to FALSE for now
      spectra_to_profile = NULL

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
      totit <- max(seq_along(ROI_separator[, 1])) * nrow(imported_data$dataset)
      sumit <- 0
      for (ROI_index in seq_along(ROI_separator[, 1])) {

        #Preparation of ROI parameters
        ROI_profile <- ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]
        ROI_buckets <- which.min(abs(as.numeric(ROI_profile[1, 1]) - imported_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2]) - imported_data$ppm))

        if (length(ROI_buckets)<20) {
          sumit <- sumit + nrow(imported_data$dataset)
          shinyWidgets::updateProgressBar(
            session = shiny::getDefaultReactiveDomain(),
            id      = "profiling_progress",
            title   = paste0("Ignoring ROI as width is too small"),
            value   = trunc(sumit/totit*100)
          )
          next
        }
        if (ROI_buckets[1]>ROI_buckets[2]){
          ROI_buckets <- rev(ROI_buckets)
        }


        #Preparation of program parameters to be sued during fitting, with some variables added to ease interpretability of code
        program_parameters             <- imported_data$program_parameters
        program_parameters$freq        <- imported_data$freq
        program_parameters$ROI_buckets <- ROI_buckets
        program_parameters$buck_step   <- imported_data$buck_step

        Xdata        <- imported_data$ppm[ROI_buckets]


        # signal <- paste0(ROI_profile$Metabolite, " [", ROI_profile$`Quantification.Signal`, "]")
        signal <-unique(paste0("(", ROI_profile$ROI.right.edge..ppm., ", ", ROI_profile$ROI.left.edge..ppm., ")"))
        if(signal %in% input$select_bs_sum){
          fitting_type <- "Baseline Sum"
        } else{
          fitting_type <- "Baseline Fitting"
        }
        # possible_fit_types <- c("baseline fitting", "baseline sum")
        # if(!(tolower(fitting_type) %in% possible_fit_types)){
        #   shinyWidgets::show_alert(
        #     title = "Quantification Mode not supported.",
        #     text = "\"Baseline Fitting\" and \"Baseline Sum\" are the only supported quantification mode.
        #     Please make sure every row in the Quantification Mode column contains one of these. This entry will be skipped",
        #     type = "error"
        #   )
        #   next
        # }

        if (length(grep("Clean",fitting_type)) == 1) {
          program_parameters$clean_fit <- "Y"
        } else {
          program_parameters$clean_fit <- "N"
        }
        signals_to_quantify <- which(ROI_profile[, 5] >= 1)
        signals_codes       <- (ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2])


        #Quantification for every spectrum
        if (is.null(spectra_to_profile)){
          spectra_to_profile <- 1:nrow(imported_data$dataset)
        }

        for (spectrum_index in spectra_to_profile) {
          shinyWidgets::updateProgressBar(
            session = shiny::getDefaultReactiveDomain(),
            id      = "profiling_progress",
            title   = paste0('Profiling ROI ', ROI_index, ' of ', nrow(ROI_separator),
                             " for Spectrum ", spectrum_index, " of ",
                             nrow(imported_data$dataset)),
            value   = trunc(sumit/totit*100)
          )

          output=profiling_func(spectrum_index       = spectrum_index,
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
                                pb                   = NULL,
                                reimplementation     = FALSE,
                                max_shift            = NULL,
                                min_shift            = NULL,
                                max_intensity        = NULL,
                                min_intensity        = NULL,
                                max_width            = NULL,
                                min_width            = NULL,
                                signal_index         = NULL)
          # Note that Ydata is internally defined by profiling function despite
          # what is supplied to the argument above. See automatic_profiling.R and
          # profiling_func()
          final_output <- output$final_output
          reproducibility_data <- output$reproducibility_data
          sumit <- sumit + 1
        }
      }
      shinyWidgets::closeSweetAlert(session = shiny::getDefaultReactiveDomain())

      # For now, will not further optimize. Later, we will determine how best to incorporate additional signal parameter optimization
      # The original code written by rDolphin is very buggy - this is reason for avoidance.
      # tryCatch({
      #   if (optimization == TRUE & length(spectra_to_profile) > 20 & nrow(ROI_data) > 20) {
      #     # optimized_profiling_data <-
      #     #   rDolphin::automatic_profiling_improv(imported_data,
      #     #                                        final_output,
      #     #                                        reproducibility_data,
      #     #                                        ROI_data)
      #
      #
      #     ## begin original code
      #
      #     suppressMessages({
      #       predicted_info <- rDolphin::signparpred(initial_matrix = final_output$half_bandwidth,
      #                                               fitting_error  = final_output$fitting_error)
      #     })
      #
      #
      #     predicted_width = as.matrix(predicted_info$predicted_matrix)
      #
      #     max_width = as.matrix(predicted_info$upper_bound_matrix)
      #     min_width = as.matrix(predicted_info$lower_bound_matrix)
      #     ind = which(is.na(predicted_width[1,]))
      #     if (length(ind) > 0) {
      #       predicted_width[,ind] = t(replicate(nrow(predicted_width),ROI_data[ind,8]))
      #       min_width[,ind] = t(replicate(nrow(predicted_width),ROI_data[ind,8]*0.75))
      #       max_width[,ind] = t(replicate(nrow(predicted_width),ROI_data[ind,8]*1.25))
      #     }
      #
      #     suppressMessages({
      #       predicted_info <- rDolphin::signparpred(initial_matrix = final_output$chemical_shift,
      #                                               fitting_error  = final_output$fitting_error)
      #     })
      #
      #     predicted_shift = as.matrix(predicted_info$predicted_matrix)
      #     max_shift = as.matrix(predicted_info$upper_bound_matrix)
      #     min_shift = as.matrix(predicted_info$lower_bound_matrix)
      #
      #     ind = which(is.na(predicted_shift[1,]))
      #     if (length(ind) > 0) {
      #       predicted_shift[,ind] = as.matrix(t(replicate(nrow(predicted_width),ROI_data[ind,6])))
      #       max_shift[,ind] = t(replicate(nrow(predicted_width),ROI_data[ind,6]+ROI_data[ind,7]))
      #       min_shift[,ind] = t(replicate(nrow(predicted_width),ROI_data[ind,6]-ROI_data[ind,7]))
      #     }
      #
      #     suppressMessages({
      #       predicted_info <- rDolphin::signparpred(initial_matrix = final_output$intensity,
      #                                               fitting_error  = final_output$fitting_error,
      #                                               met_names      = ROI_data[,4])
      #     })
      #
      #
      #     predicted_intensity = as.matrix(predicted_info$predicted_matrix)
      #     max_intensity = as.matrix(predicted_info$upper_bound_matrix)
      #     min_intensity = as.matrix(predicted_info$lower_bound_matrix)
      #
      #     ind = which(is.na(predicted_intensity[1,]))
      #     max_intensity[!is.finite(max_intensity)] = NA
      #     min_intensity[!is.finite(min_intensity)] = NA
      #     min_intensity[min_intensity<0] = 0
      #
      #
      #     quantifications_to_repeat = matrix(0,nrow(predicted_width),ncol(predicted_width))
      #     if (level=="all") quantifications_to_repeat[,]=1
      #     if (is.numeric(level)) quantifications_to_repeat[which(final_output$fitting_error>level)]=1
      #     if (level=="outliers") {
      #       tryCatch({
      #
      #         outlier_indicator=sapply(which(!is.na(predicted_shift)),
      #                                  function(x)findInterval(final_output$chemical_shift[x],
      #                                                          c(min_shift[x],max_shift[x])))
      #         if (length(outlier_indicator)>0) quantifications_to_repeat[which(!is.na(predicted_shift))][sapply(outlier_indicator,function(x)x==0|x==2)]=1
      #         outlier_indicator=sapply(which(!is.na(predicted_width)),
      #                                  function(x)findInterval(final_output$half_bandwidth[x],
      #                                                          c(min_width[x],max_width[x])))
      #         if (length(outlier_indicator)>0) quantifications_to_repeat[which(!is.na(predicted_width))][sapply(outlier_indicator,function(x)x==0|x==2)]=1
      #         outlier_indicator=sapply(which(!is.na(predicted_intensity)),
      #                                  function(x)findInterval(final_output$intensity[x],
      #                                                          c(min_intensity[x],max_intensity[x])))
      #         if (length(outlier_indicator)>0) quantifications_to_repeat[which(!is.na(predicted_intensity))][sapply(outlier_indicator,function(x)x==0|x==2)]=1
      #
      #       }, error=function(e)quantifications_to_repeat[,]=1)
      #     }
      #
      #
      #     if (improvement_option=='reimplementation') {  #Splitting of ROI data into individual ROIs to be quantified
      #       dummy = which(is.na(ROI_data[, 1]))
      #       if (length(dummy)==0) dummy=dim(ROI_data)[1]+1
      #       lal=which(duplicated(ROI_data[-dummy,1:2])==F)
      #       ROI_separator = cbind(lal, c(lal[-1] - 1, dim(ROI_data[-dummy,])[1]))
      #
      #       baselinedataset=baseline::baseline.rollingBall(imported_data$dataset,5,5)$baseline
      #
      #       #For every ROI
      #       for (ROI_index in seq_along(ROI_separator[, 1])) {
      #
      #
      #         #Preparation of ROI parameters
      #         ROI_profile = ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]
      #         ROI_buckets = which.min(abs(as.numeric(ROI_profile[1, 1])-imported_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2])-imported_data$ppm))
      #         if (length(ROI_buckets)<20) {
      #           next
      #         }
      #         if (ROI_buckets[1]>ROI_buckets[2]) ROI_buckets=rev(ROI_buckets)
      #
      #         #Preparation of program parameters to be sued during fitting, with some variables added to ease interpretability of code
      #         program_parameters=imported_data$program_parameters
      #         program_parameters$freq = imported_data$freq
      #         program_parameters$ROI_buckets = ROI_buckets
      #         program_parameters$buck_step = imported_data$buck_step
      #
      #         Xdata = imported_data$ppm[ROI_buckets]
      #         fitting_type = as.character(ROI_profile[1, 3])
      #         if (length(grep("Clean",fitting_type))==1) {
      #           program_parameters$clean_fit="Y"
      #         } else {
      #           program_parameters$clean_fit="N"
      #         }
      #         signals_to_quantify = which(ROI_profile[, 5] >= 1)
      #         signals_codes = (ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2])
      #
      #
      #
      #
      #         index_to_use_3=which(rowSums(quantifications_to_repeat[,ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],drop=F])>0)
      #         pb   <- txtProgressBar(1, nrow(imported_data$dataset), style=3)
      #
      #         #Quantification for every spectrum
      #         for (spectrum_index in index_to_use_3) {
      #
      #           #Preparation of necessary variables to store figures and information of the fitting
      #           output=profiling_func(spectrum_index,signals_codes,
      #                                 imported_data,
      #                                 ROI_buckets,fitting_type,
      #                                 program_parameters,Xdata,Ydata,
      #                                 final_output,
      #                                 reproducibility_data,
      #                                 ROI_profile,baselinedataset,
      #                                 signals_to_quantify,pb,reimplementation=T,
      #                                 max_shift=max_shift,min_shift=min_shift,
      #                                 max_intensity=max_intensity,min_intensity=min_intensity,
      #                                 max_width=max_width,min_width=min_width,
      #                                 signal_index=ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2])
      #           final_output=output$final_output
      #           reproducibility_data=output$reproducibility_data
      #         }
      #
      #       }
      #
      #
      #     } else if (improvement_option=='correction') {
      #
      #       prova_intensity=predicted_intensity
      #       prova_intensity[,apply(predicted_intensity,2,function(x)all(is.na(x)))]=final_output$intensity[,apply(predicted_intensity,2,function(x)all(is.na(x)))]
      #       prova_shift=predicted_shift
      #       prova_shift[,apply(predicted_shift,2,function(x)all(is.na(x)))]=final_output$chemical_shift[,apply(predicted_shift,2,function(x)all(is.na(x)))]
      #       prova_width=predicted_width
      #       prova_width[,apply(predicted_width,2,function(x)all(is.na(x)))]=final_output$half_bandwidth[,apply(predicted_width,2,function(x)all(is.na(x)))]
      #
      #       tec=sapply(seq(length(prova_intensity)),function(x)sum(peakpvoigt(c(prova_intensity[x],prova_shift[x],prova_width[x]*0.5/600.2,0),imported_data$ppm))*imported_data$buck_step)
      #       dim(tec)=dim(prova_intensity)
      #       tec[,apply(tec,2,function(x)all(is.na(x)))]=final_output$quantification[,apply(tec,2,function(x)all(is.na(x)))]
      #       for (i in 1:ncol(final_output$quantification)) {
      #         index_to_use_3=which(quantifications_to_repeat[,i]>0)
      #         final_output$quantification[index_to_use_3,i]=tec[index_to_use_3,i]
      #       }
      #     }
      #     optimized_profiling_data=list(final_output=final_output,reproducibility_data=reproducibility_data,
      #                         predicted_shift=predicted_shift,predicted_width=predicted_width,
      #                         predicted_intensity=predicted_intensity,max_width=max_width,
      #                         min_width=min_width,max_shift=max_shift,min_shift=min_shift,
      #                         max_intensity=max_intensity,min_intensity=min_intensity)
      #
      #     ## end 'original' code
      #
      #
      #     nn <- optimized_profiling_data$final_output$fitting_error - final_output$fitting_error
      #     no <- grDevices::boxplot.stats(nn)$stats[5]
      #     ind <- which(nn>no)
      #
      #     for (i in 1:length(optimized_profiling_data$final_output)) {
      #       optimized_profiling_data$final_output[[i]][ind] <- final_output[[i]][ind]
      #     }
      #     for (i in 1:nrow(optimized_profiling_data$final_output$fitting_error)) {
      #       ind <- which(nn[i,] > no)
      #       if (length(ind) > 0){
      #         optimized_profiling_data$reproducibility_data[[i]][ind] <- reproducibility_data[[i]][ind]
      #       }
      #     }
      #     final_output <- optimized_profiling_data$final_output
      #     reproducibility_data <- optimized_profiling_data$reproducibility_data
      #   }
      # }, error = function(e)NA)

      profiling_data <- list(final_output         = final_output,
                             reproducibility_data = reproducibility_data)

      all_profiling_results <- profiling_data

      rv$new_profiling <- TRUE

      return(all_profiling_results)
    })

  })
}
