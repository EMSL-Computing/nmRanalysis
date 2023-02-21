#' Main server function of nmRapp
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
#' @param input A list-like object containing all input data sent from the browser. Elements of this list are
#' named according to their input ID.
#' @param output A list-like object containing all output data, Elements of this list are named according to
#' their output ID.
#' @param session A new instance of the shiny app.
#'
#' @details This main server function references each of the defined server module components that handle various
#' aspects of the NMR data processing pipeline specific to targeted metabolite profiling. Refer to the documentation
#' of each referenced module for details on its specific function. A list of referenced modules is provided below:
#' 1) xpmt_data_uploadServer()
#' 2) xpmt_data_vizServer()
#' 3) xpmt_data_modifyServer()
#' 4) ref_data_uploadServer()
#' 5) ref_data_editingServer
#'
#' Outside of calls to the above modules, this main server function handles the dynamic generation of UI elements that
#' are external to each module (e.g. global tabs) and data processing dependent on output from multiple independent modules.
#'
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom plyr .
#'
nmRapp_server <- function(input, output, session) {

  ### Tab: Data Processing ----------------------------------------------------

  # The code for the xpmt_data_* modules are found in ./R/xpmt_data_*.R

  # Stores the returned reactive value from the uploadServer module. The returned reactive
  # value is the output of as.ppmData() given the user specified experimental
  # conditions

  #get your user name based on 'Sys.getenv' (static object)
  #user.name <- paste0("Logged in as: ",Sys.getenv(c("SHINYPROXY_USERNAME")))
  user.name <- paste0("Logged in as: ",Sys.getenv(c("USER")))
  #test user.name easy by printing on console
  print(user.name)

  connec <- reactive({

    connect_db()

  })

  xpmt_data       <- xpmt_data_uploadServer(id = "xpmt")

  observe({
    req(xpmt_data())
    updateTabsetPanel(inputId = "xpmtdat_tab1", selected = "xpmt_tab1_show")
  })

  # Stores experimental data after any filters/modifications applied
  mod_xpmt_data <- xpmt_data_vizServer(id = "xpmt_viz", xpmt_data = xpmt_data)

  # Wizard button to navigate from experimental data upload page to reference data editing page
  output$wizard_exptoref_ui <- renderUI({
    req(mod_xpmt_data())
    tagList(
      h4(""),
      fluidRow(
        column(5, offset = 7,
               shinyWidgets::actionBttn(
                 inputId = "wizard_exptoref",
                 label = "Reference Data Editing",
                 style = "minimal",
                 color = "primary",
                 icon = icon("arrow-right"),
                 size = "sm"
               )
        )
      )
    )
  })

  observeEvent(input$wizard_exptoref, {
    req(input$wizard_exptoref > 0)

    updateTabsetPanel(session, "AllTabs",
                      selected = "RefMetTab")
  })

  ### Tab: Reference Metabolites ----------------------------------------------

  # Handle initial upload of reference metabolite datafile and/or initial specification of reference metabolites
  # The code for the ref_data_uploadServer() module is found in ./R/ref_data_upload.R
  ref_data <- ref_data_uploadServer(id        = "ref_data_init",
                                    xpmt_data = mod_xpmt_data,
                                    ref_db    = bmse_associations,
                                    connec = connec)

  # Output a warning if any of uploaded reference metabolites not contained within database
  observe({
    req(ref_data())
    refwarn <- ref_data()$casno_not_in_db$CASno

    if(length(refwarn) != 0){
      removeTab(inputId = "refout_tabs", target = "Warnings")

      appendTab(inputId = "refout_tabs",
                tabPanel("Warnings",
                         h4("The following entries were not found in our database:"),
                         DT::dataTableOutput("user_refmet_nomatch"),
                         h4("Please visit the Add/Remove Metabolites tab to manually search our database.")))
    } else if(length(refwarn) == 0){
      removeTab(inputId = "refout_tabs", target = "Warnings")
    }
  })

  observe({
    req(ref_data())

    updateTabsetPanel(inputId = "refdat_tab1", selected = "tab1_show")
    updateTabsetPanel(inputId = "refdat_tab2", selected = "tab2_show")
  })

  # Creates a datatable that displays entries not found in reference database
  output$user_refmet_nomatch <- DT::renderDataTable({
    req(ref_data())
    req(ref_data()$casno_not_in_db$CASno)

    ref_data()$casno_not_in_db$table %>%
      DT::datatable(rownames   = FALSE,
                    extensions = "Responsive")
  })

  # Handles editing of uploaded reference metabolite data
  # The code for the ref_data_editingServer() module is found in ./R/module_ref_data_modify.R
  mod_ref_data <- ref_data_editingServer(id        = "ref_data_edits",
                                         xpmt_data = mod_xpmt_data,
                                         ref_data  = ref_data,
                                         ref_db    = bmse_associations,
                                         connec = connec)

  # Wizard buttons to navigate from reference data editing page to experimental data upload page OR from
  # reference data editing page to profiling page
  output$wizard_reftoprof_ui <- renderUI({
    req(mod_ref_data())

    shinyWidgets::actionBttn(
      inputId = "wizard_reftoprof",
      label = "Profiling",
      style = "minimal",
      color = "primary",
      icon = icon("arrow-right"),
      size = "sm")
  })

  observeEvent(input$wizard_reftoexp, {
    req(input$wizard_reftoexp > 0)

    updateTabsetPanel(session, "AllTabs",
                      selected = "UploadTab")
  })

  observeEvent(input$wizard_reftoprof, {
    req(input$wizard_reftoprof > 0)

    updateTabsetPanel(session, "AllTabs",
                      selected = "ProfilingTab")
  })


  ### Tab: Profiling Data ----------------------------------------------------------

  profiling_results <- profilingServer(id = "profiling",
                                       xpmt_data = mod_xpmt_data,
                                       ref_data = mod_ref_data,
                                       connec = connec)

  observeEvent(input$wizard_proftoref, {
    req(input$wizard_proftoref > 0)

    updateTabsetPanel(session, "AllTabs",
                      selected = "RefMetTab")
  })

  observe({
    req(profiling_results())
    updateTabsetPanel(inputId = "sigview_tab1", selected = "sigview_show")
    updateTabsetPanel(inputId = "metview_tab1", selected = "metview_show")
    updateTabsetPanel(inputId = "profout_tabs", selected = "Signal View")
  })


}
