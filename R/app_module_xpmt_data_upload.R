#' Module: UI elements specific to experimental data uploading
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
#' @details This is the UI component for the module created to handle the uploading of experimental data.
#' The value provided for 'id' should be identical across xpmt_data_uploadUI() and xpmt_data_uploadServer().
#'
#' This module component provides the UI elements that allow users to:
#' 1) Upload the .csv file containing the experimental PPM and Intensity data for all samples. The first column of
#' this datafile should provide the PPM values and subsequent columns the corresponding intensity values for each
#' sample.
#' 2) Upload the .csv file containing experimental metadata. Currently, the only metadata processed are sample labels.
#' 3) Specify experimental pH, instrument strength, and the solvent used. The available choices for each of these
#' "experimental conditions" is constrained by the conditions available in our internal database.
#'
#' @import shiny
#'
xpmt_data_uploadUI <- function(id, ref_db){
  ns <- NS(id)
  tagList(
    shinyBS::bsCollapse(id = ns("experimental_data"), open = "Experimental Data",
                        shinyBS::bsCollapsePanel(title = "Experimental Data",
                                                 fluidRow(
                                                   column(width = 12,
                                                          fileInput(ns("uploaded_nmR_edata"),
                                                                    label = "Experimental Data File:")
                                                          )
                                                 ),
                                                 fileInput(ns("uploaded_nmR_fdata"),
                                                           label = "(Optional) Experimental Metadata File:"),
                                                 shinyWidgets::switchInput(
                                                   inputId = ns("align_spectra"),
                                                   label = "Align Spectra",
                                                   size = "small"),
                                                 htmlOutput(ns("align_note")),
                                                 style = "primary"
                                                 )
                        ),

    # Note that tooltips are not working for some unknown reason.
    shinyBS::bsCollapse(id = ns("experimental_params"), open = "Experimental Conditions",
                        shinyBS::bsCollapsePanel(title = "Experimental Conditions",

                                                 fluidRow(
                                                   column(width = 6,
                                                          textInput(inputId     = ns("temperature"),
                                                                    label       = "Temperature (K)",
                                                                    value       = "",
                                                                    placeholder = "e.g. 298")),

                                                   column(width = 6,
                                                          textInput(inputId     = ns("instrument_strength"),
                                                                    label       = "Spectrometer Frequency (MHz)",
                                                                    value       = "",
                                                                    placeholder = "e.g. 600"))

                                                 ),

                                                 fluidRow(
                                                   column(width = 6,
                                                          selectInput(ns("solvent"), "Solvent:",
                                                                      c('H2O' = "h2o",
                                                                        "D2O" = "d2o"))),
                                                   column(width = 6,
                                                          textInput(inputId     = ns("pH"),
                                                                    label       = "(Optional) pH:",
                                                                    value       = "",
                                                                    placeholder = "e.g. 7.4"))


                                                 ),

                                                 fluidRow(
                                                   column(width = 6,
                                                          textInput(inputId     = ns("concentration"),
                                                                    label       = "(Optional) Concentration (mM)",
                                                                    value       = "",
                                                                    placeholder = "e.g. 100"))
                                                 ),
                                                 style = "primary"
                        )),


    # clickable button
    shinyWidgets::actionBttn(inputId = ns("process_exp_inputs"),
                             label = "Process Data",
                             style = "unite",
                             color = "primary",
                             size = "sm")
  )
}

#' Module: Server functions specific to experimental data uploading
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
#' @details This is the UI component for the module created to handle the uploading of experimental data.
#' The value provided for 'id' should be identical across xpmt_data_uploadUI() and xpmt_data_uploadServer().
#'
#' This module component provides the back-end code that:
#' 1) Processes uploaded data based on the provided experimental conditions. Data are saved as an nmRanalysis ppmData object.
#'
#' @return A reactive object containing the uploaded experimental data and parameters saved as an nmRanalysis ppmData object.
#'
#' @import shiny
#'
xpmt_data_uploadServer <- function(id){
  moduleServer(id, function(input, output, session){

    # Output (in HTML format) to display a note about the effect of automatic alignment on processing time.
    output$align_note <- renderUI({

      req(input$align_spectra)

      htmltools::HTML("<strong>Note:</strong> Alignment may substantially increase initial processing time.")
    })

    # Define reactive object containing all user supplied experimental data
    # First argument of eventReactive() describes the dependencies of the reactive expression.
    # If any one of the specified objects changes,
    # the eventReactive() will be prompted to invalidate (i.e. re-execute).
    uploaded_xpmt_data <- eventReactive(c(input$process_exp_inputs),
                                        {
                                          req(input$process_exp_inputs > 0)

                                          shinyFeedback::feedbackDanger("uploaded_nmR_edata",
                                                                        is.null(input$uploaded_nmR_edata$datapath),
                                                                        text = "Please provide a datafile.")

                                          shinyFeedback::feedbackDanger("temperature",
                                                                        input$temperature == "" | is.na(as.numeric(input$temperature)),
                                                                        text = "Numeric value required.")

                                          shinyFeedback::feedbackDanger("instrument_strength",
                                                                        input$instrument_strength == "" | is.na(as.numeric(input$instrument_strength)),
                                                                        text = "Numeric value required.")

                                          # Will not evaluate unless edata and fdata are supplied.
                                          # as well as field strength, ph, and solvent
                                          req(input$uploaded_nmR_edata)
                                          req(input$instrument_strength)
                                          req(input$temperature)
                                          req(input$solvent)

                                          # Read in experimental data file
                                          xpmt.e_data <- load_file(path    = input$uploaded_nmR_edata$datapath,
                                                                   dataset = "experiment")

                                          xpmt_temp <- as.numeric(input$temperature)
                                          xpmt_freq <- as.numeric(input$instrument_strength)
                                          xpmt_ph   <- NA
                                          xpmt_conc <- NA

                                          if(input$pH != ""){
                                            xpmt_ph <- as.numeric(input$pH)
                                          }

                                          if(input$concentration != ""){
                                            xpmt_conc <- as.numeric(input$concentration)
                                          }

                                          # Create or read in experimental metadata file
                                          if(is.null(input$uploaded_nmR_fdata$datapath)){

                                            xpmt.f_data <- data.frame(Sample = colnames(xpmt.e_data)[-1]) %>%
                                              dplyr::mutate(Experiment    = dplyr::row_number(),
                                                            pH            = xpmt_ph,
                                                            Solvent       = input$solvent,
                                                            Frequency     = xpmt_freq,
                                                            Temperature   = xpmt_temp,
                                                            Concentration = xpmt_conc)

                                            shinyWidgets::show_alert(
                                              title = "Experimental metadata not provided.",
                                              text = "Default metadata will be automatically generated.",
                                              type = "warning"
                                            )

                                          } else{
                                            xpmt.f_data <- load_file(path    = input$uploaded_nmR_fdata$datapath,
                                                                     dataset = "experiment_metadata")
                                          }

                                          # Feed above into nmRanalysis function to create ppmData object
                                          user.data <- as.ppmData(e_data              = xpmt.e_data,
                                                                  f_data              = xpmt.f_data,
                                                                  edata_cname         = "PPM",
                                                                  fdata_cname         = "Sample",
                                                                  instrument_strength = xpmt_freq,
                                                                  ph                  = xpmt_ph,
                                                                  solvent             = input$solvent,
                                                                  temperature         = xpmt_temp,
                                                                  concentration       = xpmt_conc,
                                                                  align               = input$align_spectra)
                                          return(user.data)
                                        })
  })
}
