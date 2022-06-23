#' Module: UI elements specific to experimental data uploading
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
    h4("Experimental Data"),

    # buttons to allow upload functionality
    # first argument is the assigned name of the user input
    # second argument is the displayed label on the button
    fileInput(ns("uploaded_nmR_edata"),
              label = "NMR PPM Data:"),

    fileInput(ns("uploaded_nmR_fdata"),
              label = "NMR Metadata:"),

    h4("Experimental Conditions"),

    # Allows for user to type in a response.
    # First argument is assigned name of the user input
    # second argument is the label of the field
    # third arg is the initial value
    # fourth is the placeholder text
    # User responses are stored as character values
    selectInput(ns("ph"),
                "pH:", choices = sort.int(unique(stats::na.omit(as.numeric(ref_db$pH)))),
                selected = "7.4"),

    selectInput(ns("instrument_strength"),
                "Instrument Strength (MHz):", choices = sort(unique(ref_db$Field_strength)),
                selected = "600"),

    # Drop down menu selection
    selectInput(ns("solvent"), "Solvent:",
                c('H2O' = "h2o",
                  "D2O" = "d2o")),

    # clickable button
    actionButton(ns("process_exp_inputs"), label = "Process User Inputs")
  )
}

#' Module: Server functions specific to experimental data uploading
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

    # This bit of code will update the label on the action button from "Process User Inputs" to "Process New Data" after
    # the action button has been pressed.
    observeEvent(c(input$process_exp_inputs),
                 {
                   # Will not evaluate unless experimental data has been uploaded at least once
                   req(uploaded_xpmt_data())
                   req(input$process_exp_inputs > 0)

                   updateActionButton(session, "process_exp_inputs", label = "Process New Data")
                 })

    # Define reactive object containing all user supplied experimental data
    # First argument of eventReactive() describes the dependencies of the reactive expression.
    # If any one of the specified objects changes,
    # the eventReactive() will be prompted to invalidate (i.e. re-execute).
    uploaded_xpmt_data <- eventReactive(c(input$process_exp_inputs),
                                        {
                                          # Will not evaluate unless edata and fdata are supplied.
                                          req(input$uploaded_nmR_edata)
                                          req(input$uploaded_nmR_fdata)

                                          # Read in experimental data file
                                          xpmt.e_data <- load_file(path    = input$uploaded_nmR_edata$datapath,
                                                                   dataset = "experiment")

                                          # Read in experimental metadata file
                                          xpmt.f_data <- load_file(path    = input$uploaded_nmR_fdata$datapath,
                                                                   dataset = "experiment_metadata")

                                          # Feed above into nmRanalysis function to create ppmData object
                                          user.data <- as.ppmData(e_data             = xpmt.e_data,
                                                                 f_data              = xpmt.f_data,
                                                                 edata_cname         = "PPM",
                                                                 fdata_cname         = "Sample",
                                                                 instrument_strength = as.numeric(input$instrument_strength),
                                                                 ph                  = as.numeric(input$ph),
                                                                 solvent             = input$solvent)
                                          return(user.data)
                                        })
  })
}
