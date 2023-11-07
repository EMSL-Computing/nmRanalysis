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
                                                          htmlOutput(ns("segment_conversion"))
                                                   )

                                                   ),
                                                 fluidRow(
                                                   column(width = 6,
                                                          shinyWidgets::numericRangeInput(inputId  = ns("CWTscale"),
                                                                                          label    = "CWT scale factor range:",
                                                                                          value    = c(1, 16),
                                                                                          min      = 1,
                                                                                          max      = 64)
                                                   ),
                                                   column(width = 6,
                                                          numericInput(inputId  = ns("CWTscale_step"),
                                                                       label    = "CWT scale factor step size:",
                                                                       value    = 2)
                                                   )
                                                 ),
                                                 fluidRow(

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
        title = "Recommender Querying",
        h4(""),
        fluidRow(
          column(width = 3,
                 selectizeInput(ns("recommender_metquery"), label = "Select metabolites for probability computation:",
                                choices = NULL, multiple = TRUE)),
          column(width = 3,
                 actionButton(ns("recommender_compute"), htmltools::HTML("<b>Compute</b>")))
        ),
        DT::dataTableOutput(ns("recommender_metquery_table"))
      ),
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
          ),
          column(width = 2,
                 radioButtons(
                   inputId = ns("compute_probs"),
                   label = "Compute metabolite probabilities?",
                   choices = c("Yes", "No"),
                   selected = "No"
                 )
          )
        ),
        htmlOutput(ns("common_peaks_text")),
        h4(""),
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
            fluidRow(
              column(width = 6,
                     htmlOutput(ns("candidate_text"))),
              column(width = 6,
                     htmlOutput(ns("predprobs_text")))
            )
            # htmlOutput(ns("candidate_text")),
            # h4(""),
            # # uiOutput(ns("modfit_button_ui")),
            # htmlOutput(ns("predprobs_text"))
          )
        ),
        DT::dataTableOutput(ns("metid_query_table"))
      ),
      tabPanel(
        title = "Identified Metabolites",
        h4(""),
        tabsetPanel(
          id = ns("metid_identified_metabs"),
          tabPanel(
            title = "Current Session",
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
        ),
          tabPanel(
            title = "Previous Session",
            h4(""),
            fluidRow(
              column(width = 3,
                     selectizeInput(inputId = ns("metids_list_prev_sesh"),
                                 label   = "Previously Identified Metabolites",
                                 choices = NULL, multiple = TRUE)

                    ),
              column(width = 2,
                     actionButton(ns("use_prevsesh_metabs"), htmltools::HTML("<b>Use selected metabolites as reference</b>"))
                    )
                )
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
metid_Server <- function(id, xpmt_data, connec){
  stopifnot(is.reactive(xpmt_data))
  stopifnot(is.reactive(connec))
  moduleServer(id, function(input, output, session){

    rv <- reactiveValues(obs_show_subplot_suspend = TRUE,
                         subplot_dat = NULL,
                         predprobs = list())

    observe({

      # modfits_files <- list.files(path = "C:\\Users\\flor829\\local_projectdir\\NMR\\recommender_modeling\\Results\\model_fits")
      # modfits_files <- list.files(path = "/Users/lewi052/model_fits")
      # modfits_files <- list.files(path = "C:/Users/prym311/Documents/NMRdatabase/bmrb_metabolomics/nmRanalysisAppBaseImage/model_fits")
      modfits_files <- list.files(path = "/srv/shiny/model_fits")
      modfits_nameonly <- gsub("model_", "", modfits_files)
      modfits_nameonly <- gsub(".rds", "", modfits_nameonly)
      modfits_nameonly <- tolower(modfits_nameonly)

      temp <- bmse_associations %>%
        dplyr::select(-Entry_ID, -CASno) %>%
        dplyr::rename(`Metabolite` = .data$Solute) %>%
        dplyr::mutate(tempnames = tolower(make.names(Metabolite)),
                      `Probability Available` = ifelse(tempnames %in% modfits_nameonly, "Yes", "No")) %>%
        dplyr::select(-tempnames) %>%
        dplyr::filter(`Probability Available` == "Yes") %>%
        dplyr::select(Metabolite, `Probability Available`) %>%
        dplyr::distinct()

      shinyFeedback::feedbackDanger("recommender_metquery",
                                    nrow(temp) == 0,
                                    "No metabolite recommendations available.")

      rv$mets_w_probs_available <- unique(temp$Metabolite)

      req(nrow(temp) > 0)

      updateSelectizeInput(inputId = "recommender_metquery",
                           choices = c("All", unique(temp$Metabolite)))
    })

    output$segment_conversion <- renderUI({
      req(xpmt_data())
      req(input$nDivRange)

      temp <- round(median(abs(diff(xpmt_data()$e_data$PPM)))*input$nDivRange, 5)
      htmltools::HTML(paste0("The specified segment length is equivalent to ", temp, " ppm."))
    })

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

      metid.entry <- data.frame("Metabolite"=character(),
                             "user"=character(),
                             "session"=character(),
                             'proposal_number'=character(),
                             'PI_name'=character(),
                             'project_name'=character(),
                             stringsAsFactors=FALSE,
                             check.names = FALSE)

      user.name <- Sys.getenv(c("SHINYPROXY_USERNAME"))
      timestamp <- Sys.time()

      metid.entry[nrow(metid.entry)+1,] <- c(rv$metids, user.name, timestamp,
                                             attr(xpmt_data(), "session_info")$proposal_num,
                                             attr(xpmt_data(), "session_info")$PI_name,
                                             attr(xpmt_data(), "session_info")$project_name)

      append_table(db_connection= connec(), table_name="recommended_metabs", df_object=metid.entry)
    })

    observeEvent(input$metid_remove,{
      req(rv$metids)
      rmidx <- which(rv$metids == input$metids_list)
      rv$metids <- rv$metids[-rmidx]
      updateSelectInput(inputId = "metids_list", choices = rv$metids)

      #delete row with extracted project ID
      select_project_name <- attr(xpmt_data(), "session_info")$project_name
      delete_queries = paste0("DELETE FROM recommended_metabs WHERE (project_name = '", select_project_name, "');")
      DBI::dbExecute(connec(), delete_queries)

    })

    observe({
      #display previous sessions of identified metabs
      metid.query <- query_table(db_connection = connec(), table_name="recommended_metabs")
      project.name <- attr(xpmt_data(), "session_info")$project_name

      metabs.list <- (subset(metid.query, project_name == project.name))
      updateSelectizeInput(inputId = "metids_list_prev_sesh", choices = metabs.list$Metabolite)
    })

    observeEvent(input$use_prevsesh_metabs, {
      rv$metids <- input$metids_list_prev_sesh
    })


    output$recommender_metquery_table <- DT::renderDT({

      req(input$recommender_compute)

      isolate({
        req(xpmt_data())
        req(input$recommender_metquery)

        #Change to local file
        # modfits_files <- list.files(path = "C:\\Users\\flor829\\local_projectdir\\NMR\\recommender_modeling\\Results\\model_fits")
        # modfits_files <- list.files(path = "/Users/lewi052/model_fits")
        # modfits_files <- list.files(path = "C:/Users/prym311/Documents/NMRdatabase/bmrb_metabolomics/nmRanalysisAppBaseImage/model_fits")
        modfits_files <- list.files(path = "/srv/shiny/model_fits")
        modfits_nameonly <- gsub("model_", "", modfits_files)
        modfits_nameonly <- gsub(".rds", "", modfits_nameonly)
        modfits_nameonly <- tolower(modfits_nameonly)


        if("All" %in% input$recommender_metquery){

          querymets <- bmse_associations %>%
            dplyr::select(-Entry_ID, -CASno) %>%
            dplyr::rename(`Metabolite` = .data$Solute) %>%
            dplyr::filter(.data$`Metabolite` %in% rv$mets_w_probs_available) %>%
            dplyr::select(Metabolite) %>%
            dplyr::distinct() %>%
            dplyr::mutate(Probability = NA,
                          Model = "",
                          PR_AUC = NA,
                          ROC_AUC = NA)
        } else{

          querymets <- bmse_associations %>%
            dplyr::select(-Entry_ID, -CASno) %>%
            dplyr::rename(`Metabolite` = .data$Solute) %>%
            dplyr::filter(.data$`Metabolite` %in% input$recommender_metquery) %>%
            dplyr::select(Metabolite) %>%
            dplyr::distinct() %>%
            dplyr::mutate(Probability = NA,
                          Model = "",
                          PR_AUC = NA,
                          ROC_AUC = NA)
        }

        shinyFeedback::feedbackDanger("recommender_metquery",
                                      nrow(querymets) == 0,
                                      "No metabolite recommendations available.")

        req(nrow(querymets) > 0)

        shinyWidgets::progressSweetAlert(
          session = shiny::getDefaultReactiveDomain(),
          id = "computation_progress",
          value = 0, title = "",
          display_pct = TRUE, striped = TRUE, status = "info"
        )

        for(i in 1:nrow(querymets)){

          shinyWidgets::updateProgressBar(
            session = shiny::getDefaultReactiveDomain(),
            id      = "computation_progress",
            title   = paste0('Computing recommender probability for metabolite ', i, ' of ', nrow(querymets)),
            value   = trunc(i/nrow(querymets)*100)
          )

          fmat_selmet <- tolower(make.names(querymets$Metabolite[i]))
          modind <- which(modfits_nameonly == fmat_selmet)
          modfit_selmod_file <- modfits_files[modind]

          # modfit_selmod <- readRDS(paste0("C:\\Users\\flor829\\local_projectdir\\NMR\\recommender_modeling\\Results\\model_fits\\", modfit_selmod_file))
          # modfit_selmod <- list.files(path = paste0("/Users/lewi052/model_fits", modfit_selmod_file))
          # modfit_selmod <- readRDS(paste0("C:/Users/prym311/Documents/NMRdatabase/bmrb_metabolomics/nmRanalysisAppBaseImage/model_fits/", modfit_selmod_file))
          modfit_selmod <- readRDS(paste0("/srv/shiny/model_fits/", modfit_selmod_file))
          bw1_bins <- as.numeric(c("9.95", "9.85", "9.75", "9.65", "9.55", "9.45", "9.35", "9.25", "9.15",
                                   "9.05", "8.95", "8.85", "8.75", "8.65", "8.55", "8.45", "8.35", "8.25",
                                   "8.15", "8.05", "7.95", "7.85", "7.75", "7.65", "7.55", "7.45", "7.35",
                                   "7.25", "7.15", "7.05", "6.95", "6.85", "6.75", "6.65", "6.55", "6.45",
                                   "6.35", "6.25", "6.15", "6.05", "5.95", "5.85", "5.75", "5.65", "5.55",
                                   "5.45", "5.35", "5.25", "5.15", "5.05", "4.95", "4.85", "4.75", "4.65",
                                   "4.55", "4.45", "4.35", "4.25", "4.15", "4.05", "3.95", "3.85",
                                   "3.7500000000000004", "3.65", "3.55", "3.45", "3.35", "3.25", "3.15",
                                   "3.05", "2.95", "2.85", "2.75", "2.65", "2.55", "2.45", "2.35", "2.25",
                                   "2.15", "2.05", "1.95", "1.85", "1.75", "1.65", "1.55", "1.45", "1.35",
                                   "1.25", "1.15", "1.05", "0.95", "0.85", "0.75", "0.65", "0.55", "0.45",
                                   "0.35", "0.25", "0.15", "0.05"))
          bw_cp1 <- c(bw1_bins + 0.1/2, min(bw1_bins - 0.1/2))

          # Format data for prediction
          edat <- xpmt_data()$e_data %>%
            dplyr::filter(PPM >= min(bw_cp1) & PPM <= max(bw_cp1)) %>%
            dplyr::mutate(Bingrp = cut(PPM, bw_cp1),
                          binnum = 1) %>%
            dplyr::group_by(Bingrp) %>%
            dplyr::summarise_all(list(sum)) %>%
            dplyr::mutate(PPM = PPM/binnum) %>%
            dplyr::arrange(desc(PPM)) %>%
            dplyr::select(-binnum, -Bingrp) %>%
            dplyr::mutate(PPM = round(PPM, 2),
                          PPM = as.character(PPM),
                          PPM = ifelse(PPM == "3.75", "3.7500000000000004", PPM)) %>%
            dplyr::relocate(PPM) %>%
            as.data.frame()

          # check which bw1_bins are not in edat, and add them in if not present
          miss_ppms <- bw1_bins[!(bw1_bins %in% edat$PPM)]
          miss_ppms <- miss_ppms[miss_ppms != "3.75"]
          if(length(miss_ppms) > 0){
            edat[(nrow(edat)+1):(nrow(edat)+length(miss_ppms)), 1] <- miss_ppms
            edat[is.na(edat)] <- 0
            edat <- edat %>%
              dplyr::arrange(desc(PPM))
          }
          rownames(edat) <- edat$PPM

          edat <- edat %>%
            dplyr::select(-PPM) %>%
            t() %>%
            data.frame()
          edat <- edat[rownames(edat) == input$sample_to_plot,,drop = FALSE]

          predprob_pres <- parsnip::predict.model_fit(modfit_selmod$mod_fits, edat) %>%
            dplyr::bind_cols(parsnip::predict.model_fit(modfit_selmod$mod_fits, edat, type = "prob")) %>%
            dplyr::mutate(sampnames = rownames(edat))

          querymets$Probability[i] <- round(predprob_pres$.pred_Present,3)

          modperf_train <- modfit_selmod$train_perf %>%
            dplyr::mutate(modelname = dplyr::case_when(
              model == "rand_forest" ~ "Random Forest",
              model == "logistic_reg" ~ "Penalized Logistic Regression",
              model == "mlp" ~ "Neural Network",
              model == "svm_linear" ~ "SVM: Linear Basis Function",
              model == "svm_poly" ~ "SVM: Polynomial Basis Function",
              model == "svm_rbf" ~ "SVM: Radial Basis Function"
            ))

          # 8/26/23 Need to investigate why there may be multiple rows returned for some cases
          if(nrow(modperf_train) > 1){
            modperf_train <- modperf_train[1,]
          }

          querymets$Model[i] <- modperf_train$modelname

          querymets$PR_AUC[i] <- round(modfit_selmod$test_perf$pr_auc, 3)
          querymets$ROC_AUC[i] <- round(modfit_selmod$test_perf$roc_auc, 3)

        }
        shinyWidgets::closeSweetAlert(session = shiny::getDefaultReactiveDomain())


        querymets %>%
          DT::datatable(rownames = FALSE,
                        filter = "top",
                        selection = "single",
                        extensions = c("Buttons"),
                        options = exprToFunction(
                          list(dom = 'Bfrtip',
                               buttons = list(
                                 list(extend = 'csv', text = "Download Current Page (CSV)",
                                      filename =  paste0(lubridate::year(lubridate::now()), "-",
                                                         lubridate::month(lubridate::now()), "-",
                                                         lubridate::day(lubridate::now()), "_",
                                                         "Detailed_Model_Results_Filtered.csv"),
                                      exportOptions = list(
                                        modifier = list(page = "current")
                                      )),
                                 list(extend = 'csv', text = "Download Full Results (CSV)",
                                      filename =  paste0(lubridate::year(lubridate::now()), "-",
                                                         lubridate::month(lubridate::now()), "-",
                                                         lubridate::day(lubridate::now()), "_",
                                                         "Detailed_Model_Results_Full.csv"),
                                      exportOptions = list(
                                        modifier = list(page = "all")))),
                               scrollX = TRUE)),
                        class = 'display') %>%
          DT::formatRound(columns = c("Probability", "PR_AUC","ROC_AUC"),
                          digits = 3)
      })

    })

    output$metid_query_table <- DT::renderDT({

      req(xpmt_data())

      query_tol <- input$metid_querytol

      # modfits_files <- list.files(path = "C:\\Users\\flor829\\local_projectdir\\NMR\\recommender_modeling\\Results\\model_fits")
      # modfits_files <- list.files(path = "/Users/lewi052/model_fits")
      modfits_files <- list.files(path = "/srv/shiny/model_fits")
      modfits_nameonly <- gsub("model_", "", modfits_files)
      modfits_nameonly <- gsub(".rds", "", modfits_nameonly)
      modfits_nameonly <- tolower(modfits_nameonly)


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
                        .data$pH, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$Solvent) %>%
          dplyr::mutate(tempnames = tolower(make.names(Metabolite)),
                        `Probability Available` = ifelse(tempnames %in% modfits_nameonly, "Yes", "No")) %>%
          dplyr::select(-tempnames) %>%
          dplyr::mutate(Probability = NA) %>%
          dplyr::relocate(`Probability Available`, `Probability`, `Metabolite`)

        if(nrow(querymets) > 0 & input$compute_probs == "Yes"){

          for(i in 1:nrow(querymets)){
            if(querymets$`Probability Available`[i] == "Yes"){

              fmat_selmet <- tolower(make.names(querymets$Metabolite[i]))
              modind <- which(modfits_nameonly == fmat_selmet)
              modfit_selmod_file <- modfits_files[modind]

              if(!is.null(rv$predprobs[[modfit_selmod_file]])){

                predprob_pres <- rv$predprobs[[modfit_selmod_file]]$prob_present
                querymets$Probability[i] <- round(predprob_pres$.pred_Present[predprob_pres$sampnames == input$sample_to_plot],3)

              } else{
                # modfit_selmod <- readRDS(paste0("C:\\Users\\flor829\\local_projectdir\\NMR\\recommender_modeling\\Results\\model_fits\\", modfit_selmod_file))
                # modfit_selmod <- readRDS(paste0("/Users/lewi052/model_fits", modfit_selmod_file))
                # modfit_selmod <- readRDS(paste0("C:/Users/prym311/Documents/NMRdatabase/bmrb_metabolomics/nmRanalysisAppBaseImage/model_fits/", modfit_selmod_file))
                modfit_selmod <- readRDS(paste0("/srv/shiny/model_fits/", modfit_selmod_file))
                bw1_bins <- as.numeric(c("9.95", "9.85", "9.75", "9.65", "9.55", "9.45", "9.35", "9.25", "9.15",
                                         "9.05", "8.95", "8.85", "8.75", "8.65", "8.55", "8.45", "8.35", "8.25",
                                         "8.15", "8.05", "7.95", "7.85", "7.75", "7.65", "7.55", "7.45", "7.35",
                                         "7.25", "7.15", "7.05", "6.95", "6.85", "6.75", "6.65", "6.55", "6.45",
                                         "6.35", "6.25", "6.15", "6.05", "5.95", "5.85", "5.75", "5.65", "5.55",
                                         "5.45", "5.35", "5.25", "5.15", "5.05", "4.95", "4.85", "4.75", "4.65",
                                         "4.55", "4.45", "4.35", "4.25", "4.15", "4.05", "3.95", "3.85",
                                         "3.7500000000000004", "3.65", "3.55", "3.45", "3.35", "3.25", "3.15",
                                         "3.05", "2.95", "2.85", "2.75", "2.65", "2.55", "2.45", "2.35", "2.25",
                                         "2.15", "2.05", "1.95", "1.85", "1.75", "1.65", "1.55", "1.45", "1.35",
                                         "1.25", "1.15", "1.05", "0.95", "0.85", "0.75", "0.65", "0.55", "0.45",
                                         "0.35", "0.25", "0.15", "0.05"))
                bw_cp1 <- c(bw1_bins + 0.1/2, min(bw1_bins - 0.1/2))

                # Format data for prediction
                edat <- xpmt_data()$e_data %>%
                  dplyr::filter(PPM >= min(bw_cp1) & PPM <= max(bw_cp1)) %>%
                  dplyr::mutate(Bingrp = cut(PPM, bw_cp1),
                                binnum = 1) %>%
                  dplyr::group_by(Bingrp) %>%
                  dplyr::summarise_all(list(sum)) %>%
                  dplyr::mutate(PPM = PPM/binnum) %>%
                  dplyr::arrange(desc(PPM)) %>%
                  dplyr::select(-binnum, -Bingrp) %>%
                  dplyr::mutate(PPM = round(PPM, 2),
                                PPM = as.character(PPM),
                                PPM = ifelse(PPM == "3.75", "3.7500000000000004", PPM)) %>%
                  dplyr::relocate(PPM) %>%
                  as.data.frame()

                # check which bw1_bins are not in edat, and add them in if not present
                miss_ppms <- bw1_bins[!(bw1_bins %in% edat$PPM)]
                miss_ppms <- miss_ppms[miss_ppms != "3.75"]
                if(length(miss_ppms) > 0){
                  edat[(nrow(edat)+1):(nrow(edat)+length(miss_ppms)), 1] <- miss_ppms
                  edat[is.na(edat)] <- 0
                  edat <- edat %>%
                    dplyr::arrange(desc(PPM))
                }
                rownames(edat) <- edat$PPM

                edat <- edat %>%
                  dplyr::select(-PPM) %>%
                  t() %>%
                  data.frame()
                # edat <- edat[rownames(edat) == input$sample_to_plot,,drop = FALSE]

                predprob_pres <- parsnip::predict.model_fit(modfit_selmod$mod_fits, edat) %>%
                  dplyr::bind_cols(parsnip::predict.model_fit(modfit_selmod$mod_fits, edat, type = "prob")) %>%
                  dplyr::mutate(sampnames = rownames(edat))

                rv$predprobs[[modfit_selmod_file]] <- list(prob_present  = predprob_pres,
                                                           modinfo_train = modfit_selmod$train_perf,
                                                           modinfo_test  = modfit_selmod$test_perf)

                querymets$Probability[i] <- round(predprob_pres$.pred_Present[predprob_pres$sampnames == input$sample_to_plot],3)
              }
            }
          }
        }

        if(input$compute_probs == "Yes"){
          querymets <- querymets %>%
            dplyr::select(-`Probability Available`)
        }

        querymets %>%
          dplyr::rename(`Peak Location` = `Chemical shift(ppm)`) %>%
          DT::datatable(rownames = FALSE,
                        filter = "top",
                        extensions = c("Buttons"),
                        selection = "single",
                        options = exprToFunction(
                          list(dom = 'Bfrtip',
                               buttons = list(
                                 list(extend = 'csv', text = "Download Current Page (CSV)",
                                      filename =  paste0(lubridate::year(lubridate::now()), "-",
                                                         lubridate::month(lubridate::now()), "-",
                                                         lubridate::day(lubridate::now()), "_",
                                                         "Detailed_Model_Results_Filtered.csv"),
                                      exportOptions = list(
                                        modifier = list(page = "current")
                                      )),
                                 list(extend = 'csv', text = "Download Full Results (CSV)",
                                      filename =  paste0(lubridate::year(lubridate::now()), "-",
                                                         lubridate::month(lubridate::now()), "-",
                                                         lubridate::day(lubridate::now()), "_",
                                                         "Detailed_Model_Results_Full.csv"),
                                      exportOptions = list(
                                        modifier = list(page = "all")))),
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
                        .data$pH, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$Solvent) %>%
          dplyr::mutate(tempnames = tolower(make.names(Metabolite)),
                        `Probability Available` = ifelse(tempnames %in% modfits_nameonly, "Yes", "No")) %>%
          dplyr::select(-tempnames) %>%
          dplyr::mutate(Probability = NA) %>%
          dplyr::relocate(`Probability Available`, Probability, `Metabolite`)


        if(nrow(querymets) > 0 & input$compute_probs == "Yes"){
          for(i in 1:nrow(querymets)){
            if(querymets$`Probability Available`[i] == "Yes"){

              fmat_selmet <- tolower(make.names(querymets$Metabolite[i]))
              modind <- which(modfits_nameonly == fmat_selmet)
              modfit_selmod_file <- modfits_files[modind]

              if(!is.null(rv$predprobs[[modfit_selmod_file]])){

                predprob_pres <- rv$predprobs[[modfit_selmod_file]]$prob_present
                querymets$Probability[i] <- round(predprob_pres$.pred_Present[predprob_pres$sampnames == input$sample_to_plot],3)

              } else{
                # modfit_selmod <- readRDS(paste0("C:\\Users\\flor829\\local_projectdir\\NMR\\recommender_modeling\\Results\\model_fits\\", modfit_selmod_file))
                # modfit_selmod <- readRDS(paste0("/Users/lewi052/model_fits", modfit_selmod_file))
                # modfit_selmod <- readRDS(paste0("C:/Users/prym311/Documents/NMRdatabase/bmrb_metabolomics/nmRanalysisAppBaseImage/model_fits/", modfit_selmod_file))
                modfit_selmod <- readRDS(paste0("/srv/shiny/model_fits/", modfit_selmod_file))
                bw1_bins <- as.numeric(c("9.95", "9.85", "9.75", "9.65", "9.55", "9.45", "9.35", "9.25", "9.15",
                                         "9.05", "8.95", "8.85", "8.75", "8.65", "8.55", "8.45", "8.35", "8.25",
                                         "8.15", "8.05", "7.95", "7.85", "7.75", "7.65", "7.55", "7.45", "7.35",
                                         "7.25", "7.15", "7.05", "6.95", "6.85", "6.75", "6.65", "6.55", "6.45",
                                         "6.35", "6.25", "6.15", "6.05", "5.95", "5.85", "5.75", "5.65", "5.55",
                                         "5.45", "5.35", "5.25", "5.15", "5.05", "4.95", "4.85", "4.75", "4.65",
                                         "4.55", "4.45", "4.35", "4.25", "4.15", "4.05", "3.95", "3.85",
                                         "3.7500000000000004", "3.65", "3.55", "3.45", "3.35", "3.25", "3.15",
                                         "3.05", "2.95", "2.85", "2.75", "2.65", "2.55", "2.45", "2.35", "2.25",
                                         "2.15", "2.05", "1.95", "1.85", "1.75", "1.65", "1.55", "1.45", "1.35",
                                         "1.25", "1.15", "1.05", "0.95", "0.85", "0.75", "0.65", "0.55", "0.45",
                                         "0.35", "0.25", "0.15", "0.05"))
                bw_cp1 <- c(bw1_bins + 0.1/2, min(bw1_bins - 0.1/2))

                # Format data for prediction
                edat <- xpmt_data()$e_data %>%
                  dplyr::filter(PPM >= min(bw_cp1) & PPM <= max(bw_cp1)) %>%
                  dplyr::mutate(Bingrp = cut(PPM, bw_cp1),
                                binnum = 1) %>%
                  dplyr::group_by(Bingrp) %>%
                  dplyr::summarise_all(list(sum)) %>%
                  dplyr::mutate(PPM = PPM/binnum) %>%
                  dplyr::arrange(desc(PPM)) %>%
                  dplyr::select(-binnum, -Bingrp) %>%
                  dplyr::mutate(PPM = round(PPM, 2),
                                PPM = as.character(PPM),
                                PPM = ifelse(PPM == "3.75", "3.7500000000000004", PPM)) %>%
                  dplyr::relocate(PPM) %>%
                  as.data.frame()

                # check which bw1_bins are not in edat, and add them in if not present
                miss_ppms <- bw1_bins[!(bw1_bins %in% edat$PPM)]
                miss_ppms <- miss_ppms[miss_ppms != "3.75"]
                if(length(miss_ppms) > 0){
                  edat[(nrow(edat)+1):(nrow(edat)+length(miss_ppms)), 1] <- miss_ppms
                  edat[is.na(edat)] <- 0
                  edat <- edat %>%
                    dplyr::arrange(desc(PPM))
                }
                rownames(edat) <- edat$PPM

                edat <- edat %>%
                  dplyr::select(-PPM) %>%
                  t() %>%
                  data.frame()
                # edat <- edat[rownames(edat) == input$sample_to_plot,,drop = FALSE]

                predprob_pres <- parsnip::predict.model_fit(modfit_selmod$mod_fits, edat) %>%
                  dplyr::bind_cols(parsnip::predict.model_fit(modfit_selmod$mod_fits, edat, type = "prob")) %>%
                  dplyr::mutate(sampnames = rownames(edat))

                rv$predprobs[[modfit_selmod_file]] <- list(prob_present  = predprob_pres,
                                                           modinfo_train = modfit_selmod$train_perf,
                                                           modinfo_test  = modfit_selmod$test_perf)

                querymets$Probability[i] <- round(predprob_pres$.pred_Present[predprob_pres$sampnames == input$sample_to_plot],3)
              }
            }
          }
        }

        if(input$compute_probs == "Yes"){
          querymets <- querymets %>%
            dplyr::select(-`Probability Available`)
        }

        querymets %>%
          dplyr::rename(`Peak Location` = `Chemical shift(ppm)`) %>%
          DT::datatable(rownames = FALSE,
                        filter = "top",
                        extensions = c("Buttons"),
                        selection = "single",
                        options = exprToFunction(
                          list(dom = 'Bfrtip',
                               buttons = list(
                                 list(extend = 'csv', text = "Download Current Page (CSV)",
                                      filename =  paste0(lubridate::year(lubridate::now()), "-",
                                                         lubridate::month(lubridate::now()), "-",
                                                         lubridate::day(lubridate::now()), "_",
                                                         "Detailed_Model_Results_Filtered.csv"),
                                      exportOptions = list(
                                        modifier = list(page = "current")
                                      )),
                                 list(extend = 'csv', text = "Download Full Results (CSV)",
                                      filename =  paste0(lubridate::year(lubridate::now()), "-",
                                                         lubridate::month(lubridate::now()), "-",
                                                         lubridate::day(lubridate::now()), "_",
                                                         "Detailed_Model_Results_Full.csv"),
                                      exportOptions = list(
                                        modifier = list(page = "all")))),
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

        # (6/6/23) check modfits files for metabolite
        # modfits_files <- list.files(path = "C:\\Users\\flor829\\local_projectdir\\NMR\\recommender_modeling\\Results\\model_fits")
        # modfits_files <- list.files(path = "/Users/lewi052/model_fits")
        modfits_files <- list.files(path = "/srv/shiny/model_fits")
        modfits_nameonly <- gsub("model_", "", modfits_files)
        modfits_nameonly <- gsub(".rds", "", modfits_nameonly)
        modfits_nameonly <- tolower(modfits_nameonly)

        fmat_selmet <- tolower(make.names(unique(tempdat$Metabolite)))
        modind <- which(modfits_nameonly == fmat_selmet)
        if(length(modind) > 0){
          rv$modfit_selmod_file <- modfits_files[modind]
        } else{
          rv$modfit_selmod_file <- NULL
        }


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

        rv$modfit_selmod_file <- NULL

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

    output$predprobs_text <- renderUI({

      req(xpmt_data())
      req(input$metid_query_table_rows_selected)
      req(rv$entry_info)
      req(rv$modfit_selmod_file)
      req(rv$predprobs[[rv$modfit_selmod_file]])
      req(input$compute_probs == "Yes")

      predprobs <- rv$predprobs[[rv$modfit_selmod_file]]$prob_present
      modperf_train <- rv$predprobs[[rv$modfit_selmod_file]]$modinfo_train %>%
        dplyr::mutate(modelname = dplyr::case_when(
          model == "rand_forest" ~ "Random Forest",
          model == "logistic_reg" ~ "Penalized Logistic Regression",
          model == "mlp" ~ "Neural Network",
          model == "svm_linear" ~ "SVM: Linear Basis Function",
          model == "svm_poly" ~ "SVM: Polynomial Basis Function",
          model == "svm_rbf" ~ "SVM: Radial Basis Function"
        ))
      modperf_test <- rv$predprobs[[rv$modfit_selmod_file]]$modinfo_test

      htmltools::HTML(paste0("<b>Average Probability Across All Samples:</b> ", round(mean(predprobs$.pred_Present),4), "<br>",
                             "<br>",
                             "<b>Fitted Model Type:</b> ", modperf_train$modelname, "<br>",
                             "<br>",
                             "<b>Model Precision/Recall AUC:</b> ", round(modperf_test$pr_auc, 4), "<br>",
                             "<br>",
                             "<b>Model Sensitivity/Specificity AUC:</b> ", round(modperf_test$roc_auc, 4)))
    })

    # output$modfit_button_ui <- renderUI({
    #
    #   req(xpmt_data())
    #   req(input$metid_query_table_rows_selected)
    #   req(rv$entry_info)
    #   req(rv$modfit_selmod_file)
    #   req(is.null(rv$predprobs[[rv$modfit_selmod_file]]))
    #
    #   shinyWidgets::actionBttn(inputId = NS(id, "modfit_button"),
    #                            label = "Generate Predicted Probability",
    #                            style = "unite",
    #                            color = "primary",
    #                            size = "sm")
    #
    # })

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

    # observeEvent(c(input$modfit_button), {
    #   req(xpmt_data())
    #   req(input$metid_query_table_rows_selected)
    #   req(rv$entry_info)
    #   req(rv$modfit_selmod_file)
    #   req(input$modfit_button > 0)
    #
    #
    #   modfit_selmod <- readRDS(paste0("C:\\Users\\flor829\\local_projectdir\\NMR\\recommender_modeling\\Results\\model_fits\\", rv$modfit_selmod_file))
    #   bw1_bins <- as.numeric(c("9.95", "9.85", "9.75", "9.65", "9.55", "9.45", "9.35", "9.25", "9.15",
    #                            "9.05", "8.95", "8.85", "8.75", "8.65", "8.55", "8.45", "8.35", "8.25",
    #                            "8.15", "8.05", "7.95", "7.85", "7.75", "7.65", "7.55", "7.45", "7.35",
    #                            "7.25", "7.15", "7.05", "6.95", "6.85", "6.75", "6.65", "6.55", "6.45",
    #                            "6.35", "6.25", "6.15", "6.05", "5.95", "5.85", "5.75", "5.65", "5.55",
    #                            "5.45", "5.35", "5.25", "5.15", "5.05", "4.95", "4.85", "4.75", "4.65",
    #                            "4.55", "4.45", "4.35", "4.25", "4.15", "4.05", "3.95", "3.85",
    #                            "3.7500000000000004", "3.65", "3.55", "3.45", "3.35", "3.25", "3.15",
    #                            "3.05", "2.95", "2.85", "2.75", "2.65", "2.55", "2.45", "2.35", "2.25",
    #                            "2.15", "2.05", "1.95", "1.85", "1.75", "1.65", "1.55", "1.45", "1.35",
    #                            "1.25", "1.15", "1.05", "0.95", "0.85", "0.75", "0.65", "0.55", "0.45",
    #                            "0.35", "0.25", "0.15", "0.05"))
    #   bw_cp1 <- c(bw1_bins + 0.1/2, min(bw1_bins - 0.1/2))
    #
    #   # Format data for prediction
    #   edat <- xpmt_data()$e_data %>%
    #     dplyr::filter(PPM >= min(bw_cp1) & PPM <= max(bw_cp1)) %>%
    #     dplyr::mutate(Bingrp = cut(PPM, bw_cp1),
    #                   binnum = 1) %>%
    #     dplyr::group_by(Bingrp) %>%
    #     dplyr::summarise_all(list(sum)) %>%
    #     dplyr::mutate(PPM = PPM/binnum) %>%
    #     dplyr::arrange(desc(PPM)) %>%
    #     dplyr::select(-binnum, -Bingrp) %>%
    #     dplyr::mutate(PPM = round(PPM, 2),
    #                   PPM = as.character(PPM),
    #                   PPM = ifelse(PPM == "3.75", "3.7500000000000004", PPM)) %>%
    #     dplyr::relocate(PPM) %>%
    #     as.data.frame()
    #   rownames(edat) <- edat$PPM
    #   edat <- edat %>%
    #     dplyr::select(-PPM) %>%
    #     t() %>%
    #     data.frame()
    #
    #   predprob_pres <- parsnip::predict.model_fit(modfit_selmod$mod_fits, edat) %>%
    #     dplyr::bind_cols(parsnip::predict.model_fit(modfit_selmod$mod_fits, edat, type = "prob")) %>%
    #     dplyr::mutate(sampnames = rownames(edat))
    #
    #   rv$predprobs[[rv$modfit_selmod_file]] <- list(prob_present  = predprob_pres,
    #                                                 modinfo_train = modfit_selmod$train_perf,
    #                                                 modinfo_test  = modfit_selmod$test_perf)
    #
    # })


    # -------------------------------------------------------------------------
    # Module output

    reactive({
      rv$metids
    })

  })
}
