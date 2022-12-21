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
                                                 # fluidRow(
                                                 #   column(width = 6,
                                                 #          numericInput(inputId  = ns("snrThresh"),
                                                 #                       label    = "Signal-to-Noise Threshold:",
                                                 #                       value    = 0.8)
                                                 #   ),
                                                 #   column(width = 6,
                                                 #          shinyWidgets::switchInput(inputId = ns("snr_show"),
                                                 #                                       label   = "Visualize",
                                                 #                                       value   = FALSE)
                                                 #   )
                                                 # ),
                                                 fluidRow(
                                                   column(width = 6,
                                                          numericInput(inputId  = ns("baselineThresh"),
                                                                       label    = "Intensity Threshold:",
                                                                       value    = 50000)
                                                   ),
                                                   column(width = 6,
                                                          shinyWidgets::switchInput(inputId = ns("baseline_show"),
                                                                                       label   = "Visualize",
                                                                                       value   = FALSE)
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

#' Module: UI elements specific to metabolite identification
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
metid_mainUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("metid_groupthresh_ui")),
    tabsetPanel(
      id = ns("metid_tabs"),
      tabPanel(
        title = "Feature Querying",
        h4(""),
        fluidRow(
          column(width = 3,
                 selectInput(inputId = ns("metid_queryset"),
                             label   = "Query from:",
                             choices = c("Custom" = "cust"),
                             selected = c("cust"))
                 ),
          column(width = 4,
                 tabsetPanel(
                   id = ns("metid_queryset_tabs"),
                   type = "hidden",
                   selected = "cust",
                   tabPanelBody(
                     value = "det",
                     selectInput(inputId = ns("metid_det"),
                                 label   = "Select a location:",
                                 choices = c(""))
                   ),
                   tabPanelBody(
                     value = "cust",
                     numericInput(inputId = ns("metid_cust"),
                                  label   = "Specify a location:",
                                  value   = 0)
                   )
                 )
                 ),
          column(width = 3,
                 numericInput(inputId = ns("metid_querytol"),
                              label   = "Specify the search tolerance:",
                              value   = 0.01)
          )
        ),
        htmlOutput(ns("common_peaks_text")),
        h4(""),
        fluidRow(
          column(width = 6,
                 DT::dataTableOutput(ns("metid_query_table"))
          ),
          column(width = 6,
                 tabsetPanel(
                   id = ns("metid_more_query_info"),
                   type = "hidden",
                   selected = "none",
                   tabPanelBody(
                     value = "none",
                     h4("Select a candidate metabolite from the table.")
                   ),
                   tabPanelBody(
                     value = "query_info",
                     fluidRow(
                       # column(width = 9,
                       #        "Add to identifications: "),
                       column(width = 6,
                              actionButton(ns("metid_add"), htmltools::HTML("<b>Add selection to identifications</b>")))
                     ),
                     h4(""),
                     htmlOutput(ns("candidate_text"))
                   )
                 )
                 )
        )
      ),
      tabPanel(
        title = "Identified Metabolites",
        h4(""),
        fluidRow(
          column(width = 3,
                 selectInput(inputId = ns("metids_list"),
                             label   = "Identified Metabolites",
                             choices = NULL)
          ),
          column(width = 2,
                 actionButton(ns("metid_remove"), htmltools::HTML("<b>Remove selection from identifications</b>"))
          )
        )
      )
    )
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

    observeEvent(c(input$baseline_show, input$baselineThresh), {
      req(input$baselineThresh)
      req(xpmt_data())

      if(input$baseline_show){
        plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "deleteTraces", list(as.integer(1)))

        plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "addTraces",
                                  list(x = xpmt_data()$e_data$PPM,
                                       y = rep(input$baselineThresh, length(xpmt_data()$e_data$PPM)),
                                       mode = "lines",
                                       showlegend = FALSE,
                                       line = list(dash = "dash")
                                  ))
      } else{
        plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "deleteTraces", list(as.integer(1)))
      }

    })

    observeEvent(input$metid_add,{
      rv$metids <- unique(c(rv$metids, rv$entry_info$Metabolite))
      updateSelectInput(inputId = "metids_list", choices = rv$metids)
    })

    observeEvent(input$metid_remove,{
      req(rv$metids)
      rmidx <- which(rv$metids == input$metids_list)
      rv$metids <- rv$metids[-rmidx]
      updateSelectInput(inputId = "metids_list", choices = rv$metids)
    })


    output$metid_query_table <- DT::renderDT({

      req(xpmt_data())

      query_tol <- input$metid_querytol

      if(input$metid_queryset == "cust"){
        feature_loc <- as.numeric(input$metid_cust)
        querymets <- refmets_full %>% dplyr::filter(.data$`Chemical shift(ppm)` >= (feature_loc - query_tol) &
                                                      .data$`Chemical shift(ppm)` <= (feature_loc + query_tol))
        querymets <- querymets %>%
          dplyr::group_by(.data$`Metabolite`, .data$`Quantification Signal`, .data$`Frequency (MHz)`,
                          .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$`Solvent`) %>%
          dplyr::summarise(dplyr::across(dplyr::all_of(c('Chemical shift(ppm)')), mean, na.rm = TRUE),
                           dplyr::across(dplyr::all_of(c('Multiplicity')), getmode, useNA = "no")) %>%
          dplyr::ungroup() %>%
          dplyr::select(.data$Metabolite, .data$`Chemical shift(ppm)`, .data$Multiplicity, .data$`Frequency (MHz)`,
                        .data$pH, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$Solvent)

        querymets %>%
          dplyr::rename(`Peak Location` = `Chemical shift(ppm)`) %>%
          DT::datatable(rownames = FALSE,
                        filter = "top",
                        selection = "single",
                        options = exprToFunction(
                          list(dom = 'Bfrtip',
                               scrollX = TRUE)),
                        class = 'display') %>%
          DT::formatRound(columns = c("Peak Location", "Frequency (MHz)",
                                      "pH", "Concentration (mM)", "Temperature (K)"),
                          digits = 3)

      } else if(input$metid_queryset == "det"){
        feature_loc <- as.numeric(input$metid_det)
        querymets <- refmets_full %>% dplyr::filter(.data$`Chemical shift(ppm)` >= (feature_loc - query_tol) &
                                                      .data$`Chemical shift(ppm)` <= (feature_loc + query_tol))
        querymets <- querymets %>%
          dplyr::group_by(.data$`Metabolite`, .data$`Quantification Signal`, .data$`Frequency (MHz)`,
                          .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$`Solvent`) %>%
          dplyr::summarise(dplyr::across(dplyr::all_of(c('Chemical shift(ppm)')), mean, na.rm = TRUE),
                           dplyr::across(dplyr::all_of(c('Multiplicity')), getmode, useNA = "no")) %>%
          dplyr::ungroup() %>%
          dplyr::select(.data$Metabolite, .data$`Chemical shift(ppm)`, .data$Multiplicity, .data$`Frequency (MHz)`,
                        .data$pH, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$Solvent)

        querymets %>%
          dplyr::rename(`Peak Location` = `Chemical shift(ppm)`) %>%
          DT::datatable(rownames = FALSE,
                        filter = "top",
                        selection = "single",
                        options = exprToFunction(
                          list(dom = 'Bfrtip',
                               scrollX = TRUE)),
                        class = 'display') %>%
          DT::formatRound(columns = c("Peak Location", "Frequency (MHz)",
                                      "pH", "Concentration (mM)", "Temperature (K)"),
                          digits = 3)
      }

    })

    observeEvent(input$metid_query_table_rows_selected, ignoreNULL = FALSE, {
      req(xpmt_data())
      req(input$metid_querytol)
      req(input$metid_queryset)

      query_tol <- input$metid_querytol

      if(input$metid_queryset == "cust"){
        req(input$metid_cust)
        feature_loc <- as.numeric(input$metid_cust)
      } else if(input$metid_queryset == "det"){
        req(input$metid_det)
        feature_loc <- as.numeric(input$metid_det)
      }

      querymets <- refmets_full %>% dplyr::filter(.data$`Chemical shift(ppm)` >= (feature_loc - query_tol) &
                                                    .data$`Chemical shift(ppm)` <= (feature_loc + query_tol))
      querymets <- querymets %>%
        dplyr::group_by(.data$`Metabolite`, .data$`Quantification Signal`, .data$`Frequency (MHz)`,
                        .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$`Solvent`) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(c('Chemical shift(ppm)')), mean, na.rm = TRUE),
                         dplyr::across(dplyr::all_of(c('Multiplicity')), getmode, useNA = "no")) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$Metabolite, .data$`Chemical shift(ppm)`, .data$Multiplicity, .data$`Frequency (MHz)`,
                      .data$pH, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$Solvent)

      tab <- querymets %>%
        dplyr::rename(`Peak Location` = `Chemical shift(ppm)`)

      selrow <- input$metid_query_table_rows_selected


      if(!is.null(selrow)){
        rv$entry_info <- tab[selrow, , drop = FALSE]
        updateTabsetPanel(inputId = "metid_more_query_info", selected = "query_info")

        tempdat <-  refmets_full %>% dplyr::filter(.data$Metabolite == rv$entry_info$Metabolite)
        tempdat <- tempdat %>%
          dplyr::group_by(.data$`Metabolite`, .data$`Quantification Signal`, .data$`Frequency (MHz)`,
                          .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$`Solvent`) %>%
          dplyr::summarise(dplyr::across(dplyr::all_of(c('Chemical shift(ppm)')), mean, na.rm = TRUE),
                           dplyr::across(dplyr::all_of(c('Multiplicity')), getmode, useNA = "no")) %>%
          dplyr::ungroup() %>%
          dplyr::select(.data$Metabolite, .data$`Chemical shift(ppm)`, .data$Multiplicity, .data$`Frequency (MHz)`,
                        .data$pH, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$Solvent)

        if(!is.na(rv$entry_info$`Frequency (MHz)`)){
          tempdat <- tempdat %>% dplyr::filter(.data$`Frequency (MHz)` == rv$entry_info$`Frequency (MHz)`)
        }

        if(!is.na(rv$entry_info$pH)){
          tempdat <- tempdat %>% dplyr::filter(.data$pH == rv$entry_info$pH)
        }

        if(!is.na(rv$entry_info$`Concentration (mM)`)){
          tempdat <- tempdat %>% dplyr::filter(.data$`Concentration (mM)` == rv$entry_info$`Concentration (mM)`)
        }

        if(!is.na(rv$entry_info$`Temperature (K)`)){
          tempdat <- tempdat %>% dplyr::filter(.data$`Temperature (K)` == rv$entry_info$`Temperature (K)`)
        }

        if(!is.na(rv$entry_info$Solvent)){
          tempdat <- tempdat %>% dplyr::filter(.data$Solvent == rv$entry_info$Solvent)
        }


        # Create default annot object to add as annotation to plot
        ROI_annot <- list(
          y         = 0,
          xref      = "x",
          yref      = "y",
          arrowhead = 4,
          ay        = -40,
          arrowcolor = "red"
        )

        ROI_annots <- list()
        for(i in 1:nrow(tempdat)){
          ROI_annot[["x"]]         <- tempdat$`Chemical shift(ppm)`[i]
          ROI_annot[["text"]]      <- paste0(sprintf("<b>%s</b>", "Metabolite"), ": ",
                                             tempdat$Metabolite[i], "<br> ",
                                             sprintf("<b>%s</b>", "Peak"), ": ",
                                             round(tempdat$`Chemical shift(ppm)`[i], 5), "<br> ",
                                             sprintf("<b>%s</b>", "Multiplicity"), ": ",
                                             tempdat$Multiplicity[i])
          ROI_annot[["arrowsize"]] <- 1
          ROI_annot[["showarrow"]] <- TRUE

          ROI_annots               <- c(ROI_annots, list(ROI_annot))
        }

        rv$candidate_annots <- ROI_annots

        plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                  list(annotations = c(rv$feat_annots, rv$candidate_annots)))

      } else{
        rv$entry_info <- NULL
        updateTabsetPanel(inputId = "metid_more_query_info", selected = "none")

        rv$candidate_annots <- NULL

        plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                  list(annotations = c(rv$feat_annots, rv$candidate_annots)))
      }

    })

    # Observer to control which set of options for feature querying is displayed
    observeEvent(c(input$metid_queryset),
                 {
                   req(xpmt_data())

                   updateTabsetPanel(inputId = "metid_queryset_tabs", selected = input$metid_queryset)
                 })

    observeEvent(c(rv$spectra_peaks, input$sample_to_plot, input$peak_group_dist),
                 {
                   req(xpmt_data())
                   req(rv$spectra_peaks)
                   req(input$sample_to_plot)
                   req(input$peak_group_dist)

                   ppm     <- as.numeric(xpmt_data()$e_data[, 1, drop = TRUE])

                   selsamp_peaks <- rv$spectra_peaks[[which(names(rv$spectra_peaks) == input$sample_to_plot)]]

                   ppm_peaks         <- ppm[selsamp_peaks]
                   ppm_peak_diffs    <- abs(diff(ppm_peaks))
                   split_idxs        <- which(ppm_peak_diffs > input$peak_group_dist) + 1
                   peak_groups       <- split(ppm_peaks, cumsum(seq_along(ppm_peaks) %in% split_idxs))
                   feature_locations <- Reduce("c", lapply(peak_groups, mean))

                   updateSelectInput(inputId = "metid_det", choices = round(feature_locations, 5))
                 })

    output$common_peaks_text <- renderUI({

      req(xpmt_data())
      req(rv$spectra_peaks)
      req(input$metid_querytol)
      req(input$peak_group_dist)
      req(input$metid_queryset)

      xpmt_data <- xpmt_data()$e_data
      query_tol <- input$metid_querytol
      ppm       <- as.numeric(xpmt_data[, 1, drop = TRUE])

      if(input$metid_queryset == "cust"){
        req(input$metid_cust)
        feature_loc <- as.numeric(input$metid_cust)

        ppm_allfeats     <- lapply(rv$spectra_peaks, function(x, ppm, group_dist){

          ppm_peaks         <- ppm[x]
          ppm_peak_diffs    <- abs(diff(ppm_peaks))
          split_idxs        <- which(ppm_peak_diffs > group_dist) + 1
          peak_groups       <- split(ppm_peaks, cumsum(seq_along(ppm_peaks) %in% split_idxs))
          feature_locations <- Reduce("c", lapply(peak_groups, mean))
          return(feature_locations)
        }, ppm = ppm, group_dist = input$peak_group_dist)

        intfeat_samps <- Reduce("c", lapply(ppm_allfeats, function(x, loc, tol){
          length(x[which(x <= (loc + tol) & x >= (loc - tol))])
        }, loc = feature_loc, tol = query_tol))

        sampcount <- sum(intfeat_samps > 0)

        htmltools::HTML(paste0("Detected peak groups within ", query_tol, " ppm of the specified location (",
                               feature_loc, " ppm) are found in ", sampcount, " out of ", ncol(xpmt_data)-1, " sample spectra."))

      } else if(input$metid_queryset == "det"){
        req(input$metid_det)
        feature_loc <- as.numeric(input$metid_det)

        ppm_allfeats     <- lapply(rv$spectra_peaks, function(x, ppm, group_dist){

          ppm_peaks         <- ppm[x]
          ppm_peak_diffs    <- abs(diff(ppm_peaks))
          split_idxs        <- which(ppm_peak_diffs > group_dist) + 1
          peak_groups       <- split(ppm_peaks, cumsum(seq_along(ppm_peaks) %in% split_idxs))
          feature_locations <- Reduce("c", lapply(peak_groups, mean))
          return(feature_locations)
        }, ppm = ppm, group_dist = input$peak_group_dist)

        intfeat_samps <- Reduce("c", lapply(ppm_allfeats, function(x, loc, tol){
          length(x[which(x <= (loc + tol) & x >= (loc - tol))])
        }, loc = feature_loc, tol = query_tol))

        sampcount <- sum(intfeat_samps > 0)

        htmltools::HTML(paste0("Detected peak groups within ", query_tol, " ppm of the specified location (",
                               feature_loc, " ppm) are found in ", sampcount, " out of ", ncol(xpmt_data)-1, " sample spectra."))
      }
    })

    output$candidate_text <- renderUI({

      req(xpmt_data())
      req(input$metid_query_table_rows_selected)
      req(rv$entry_info)


      isinlist <- ifelse(rv$entry_info$Metabolite %in% rv$metids, "Yes", "No")
      currpeak <- rv$entry_info$`Peak Location`
      other_peaks <- refmets_full %>%
        dplyr::filter(.data$`Metabolite` == rv$entry_info$Metabolite)
      other_peaks <- other_peaks %>%
        dplyr::group_by(.data$`Metabolite`, .data$`Quantification Signal`, .data$`Frequency (MHz)`,
                        .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$`Solvent`) %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(c('Chemical shift(ppm)')), mean, na.rm = TRUE),
                         dplyr::across(dplyr::all_of(c('Multiplicity')), getmode, useNA = "no")) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$Metabolite, .data$`Chemical shift(ppm)`, .data$Multiplicity, .data$`Frequency (MHz)`,
                      .data$pH, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$Solvent)
      other_peaks <- other_peaks %>% dplyr::rename(`Peak Location` = `Chemical shift(ppm)`)

      if(!is.na(rv$entry_info$`Frequency (MHz)`)){
        other_peaks <- other_peaks %>% dplyr::filter(.data$`Frequency (MHz)` == rv$entry_info$`Frequency (MHz)`)
      }

      if(!is.na(rv$entry_info$pH)){
        other_peaks <- other_peaks %>% dplyr::filter(.data$pH == rv$entry_info$pH)
      }

      if(!is.na(rv$entry_info$`Concentration (mM)`)){
        other_peaks <- other_peaks %>% dplyr::filter(.data$`Concentration (mM)` == rv$entry_info$`Concentration (mM)`)
      }

      if(!is.na(rv$entry_info$`Temperature (K)`)){
        other_peaks <- other_peaks %>% dplyr::filter(.data$`Temperature (K)` == rv$entry_info$`Temperature (K)`)
      }

      if(!is.na(rv$entry_info$Solvent)){
        other_peaks <- other_peaks %>% dplyr::filter(.data$Solvent == rv$entry_info$Solvent)
      }


      if(!is.null(rv$spectra_peaks)){
        ppm     <- as.numeric(xpmt_data()$e_data[, 1, drop = TRUE])

        ppm_allfeats     <- lapply(rv$spectra_peaks, function(x, ppm, group_dist){

          ppm_peaks         <- ppm[x]
          ppm_peak_diffs    <- abs(diff(ppm_peaks))
          split_idxs        <- which(ppm_peak_diffs > group_dist) + 1
          peak_groups       <- split(ppm_peaks, cumsum(seq_along(ppm_peaks) %in% split_idxs))
          feature_locations <- Reduce("c", lapply(peak_groups, mean))
          return(feature_locations)
        }, ppm = ppm, group_dist = input$peak_group_dist)

        # peak_ppms <- lapply(rv$spectra_peaks, function(x, ppm){ppm[x]}, ppm = ppm)

        proximity_info <- vector("list", length = nrow(other_peaks))
        for(i in 1:nrow(other_peaks)){
          proximity_check <- lapply(ppm_allfeats, function(x, loc, tol){ # change ppm_allfeats to peak_ppms if you want peak locs not feature locs
            close_features <- x[x >= (loc - tol) & x <= (loc + tol)]
            return(ifelse(length(close_features) > 0, 1, 0))
          }, loc = other_peaks$`Peak Location`[i], tol = input$metid_querytol)
          proximity_info[[i]] <- list(count = sum(Reduce("c", proximity_check)),
                                      nosamps = names(proximity_check)[which(proximity_check == 0)])
        }
        other_peaks_counts <- Reduce("c", rlist::list.ungroup(rlist::list.select(proximity_info, count)))
        other_peaks_locs <- paste0(other_peaks$`Peak Location`, " (", other_peaks_counts, "/",
                                   ncol(xpmt_data()$e_data)-1, " samples with detected peak groups within ", input$metid_querytol, " ppm)")
        other_peaks_locs <- paste0(other_peaks_locs, collapse = "<br> &emsp; ")

        htmltools::HTML(paste0("<b>Selected metabolite:</b> ", rv$entry_info$Metabolite, "<br>",
                               "<b>Included in current identification list?</b> ", isinlist, "<br>",
                               "<br>",
                               "<b>All metabolite peak locations:</b>", "<br>",
                               "&emsp; ", other_peaks_locs))
      } else{
        ppm     <- as.numeric(xpmt_data()$e_data[, 1, drop = TRUE])

        other_peaks_locs <- paste0(other_peaks$`Peak Location`, collapse = "<br> &emsp; ")

        htmltools::HTML(paste0("<b>Selected metabolite:</b> ", rv$entry_info$Metabolite, "<br>",
                               "<b>Included in current identification list?</b> ", isinlist, "<br>",
                               "<br>",
                               "<b>All metabolite peak locations:</b>", "<br>",
                               "&emsp; ", other_peaks_locs))
      }

    })

    # UI element creating a dropdown button containing several options that relate
    # to experimental data visualization for this tab. These options include:
    # 1) Choosing which sample spectrum to display
    # 2) Toggle for whether subplots should be displayed on region select
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
                                     label   = "Show detected peak annotations",
                                     value   = TRUE,
                                     status  = "primary",
                                     right   = TRUE),


        circle = TRUE, status = "info",
        icon = icon("cog"), width = "300px",

        tooltip = shinyWidgets::tooltipOptions(title = "Data Options")
      )
    })

    output$metid_groupthresh_ui <- renderUI({
      req(xpmt_data())
      numericInput(inputId = NS(id, "peak_group_dist"),
                   label   = "Peak Grouping Threshold:",
                   value   = max(0,round(mean(diff(xpmt_data()$e_data$PPM)), 5)),
                   min     = max(0, min(diff(xpmt_data()$e_data$PPM))))
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
                                      shapePosition      = FALSE))
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

      # Clear plots
      plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "deleteTraces", as.list(as.integer(0)))


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
          ppm_peak_diffs    <- abs(diff(ppm_peaks))
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

          cand_annots_keep_idxs <- Reduce("c", lapply(rv$candidate_annots, function(x, lb, ub){
            (x$x >= lb) & (x$x <= ub)
          }, lb = min(brushed_data$x), ub = max(brushed_data$x)))

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
                           annotations = c(ROI_annots, rv$candidate_annots[cand_annots_keep_idxs]),
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

          cand_annots_keep_idxs <- Reduce("c", lapply(rv$candidate_annots, function(x, lb, ub){
            (x$x >= lb) & (x$x <= ub)
          }, lb = min(brushed_data$x), ub = max(brushed_data$x)))

          p <- plotly::plot_ly(source = sourceid) %>%
            plotly::config(displaylogo = FALSE,
                           modeBarButtons = list(list("zoom2d"), list("zoomIn2d"),
                                                 list("zoomOut2d"), list("pan2d"), list("autoScale2d"),
                                                 list("resetScale2d"), list("toImage"))) %>%
            plotly::layout(xaxis = list(title     = "PPM",
                                        autorange = "reversed"),
                           yaxis = list(title     = "Intensity"),
                           annotations = c(rv$candidate_annots[cand_annots_keep_idxs]),
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
                     text = "This may take several minutes. Do not refresh the page or re-initiate the detection algorithm. This alert will close when detection is complete.",
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

                   updateSelectInput(inputId = "metid_queryset",
                                     choices = c("Detected Peak Group Locations" = "det",
                                                 "Custom" = "cust"),
                                     selected = "cust")
                 })

    observeEvent(c(rv$spectra_peaks, input$sample_to_plot, input$peak_group_dist, input$show_annotations),
                 {
                   req(xpmt_data())
                   req(input$sample_to_plot)
                   req(input$peak_group_dist)
                   req(rv$spectra_peaks)

                   if(!input$show_annotations){
                     rv$feat_annots <- NULL
                     plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                               list(annotations = c(rv$candidate_annots, rv$feat_annots)))
                   }
                   req(input$show_annotations)


                   ppm     <- as.numeric(xpmt_data()$e_data[, 1, drop = TRUE])

                   selsamp_peaks <- rv$spectra_peaks[[which(names(rv$spectra_peaks) == input$sample_to_plot)]]

                   ppm_peaks         <- ppm[selsamp_peaks]
                   ppm_peak_diffs    <- abs(diff(ppm_peaks))
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

                   rv$feat_annots <- ROI_annots

                   plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                             list(annotations = c(rv$candidate_annots, rv$feat_annots)))

                 })

    observeEvent(c(input$metid_queryset, input$metid_det, input$metid_cust, input$metid_querytol),
                 {
                   req(xpmt_data())

                   if(input$metid_queryset == "det"){
                     query_line <- list(
                       type = "line",
                       line = list(color = "red",
                                   width = 4),
                       xref = "x",
                       yref = "y",
                       x0   = as.numeric(input$metid_det) - input$metid_querytol,
                       x1   = as.numeric(input$metid_det) + input$metid_querytol,
                       y0   = 0,
                       y1   = 0
                     )
                     plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                               list(shapes = query_line))
                   } else if(input$metid_queryset == "cust"){
                     query_line <- list(
                       type = "line",
                       line = list(color = "red",
                                   size = 4),
                       xref = "x",
                       yref = "y",
                       x0   = input$metid_cust - input$metid_querytol,
                       x1   = input$metid_cust + input$metid_querytol,
                       y0   = 0,
                       y1   = 0
                     )
                   }

                   plotly::plotlyProxyInvoke(metid_e_data_plot_proxy, "relayout",
                                             list(shapes = list(query_line)))
                 })


    # -------------------------------------------------------------------------
    # Module output

    reactive({
      rv$metids
    })

  })
}
