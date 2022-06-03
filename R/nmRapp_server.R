#' Main server function of nmRapp
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
  # value is the output of nmRanalysis::as.ppmData() given the user specified experimental
  # conditions
  xpmt_data       <- xpmt_data_uploadServer(id = "xpmt")

  observe({
    req(xpmt_data())
    updateTabsetPanel(inputId = "xpmtdat_tab1", selected = "xpmt_tab1_show")
  })

  # Stores experimental data after any filters/modifications applied
  mod_xpmt_data <- xpmt_data_vizServer(id = "xpmt_viz", xpmt_data = xpmt_data)

  ### Tab: Reference Metabolites ----------------------------------------------

  # observer to show reference metabolite editing tab after experimental data has been uploaded
  # The code for the ref_data_uploadUI() module is found in ./R/module_ref_data_upload.R
  # The code for the ref_data_profileUI() module is found in ./R/module_ref_data_modify.R
  # The code for the ref_data_ROIeditingUI() module is found in ./R/module_ref_data_modify.R
  # The code for the ref_data_add_delUI() module is found in ./R/module_ref_data_modify.R
  # The code for the ref_data_quantTab() module is found in ./R/module_ref_data_modify.R
  observeEvent(xpmt_data(), ignoreNULL = TRUE,
               {
                 req(xpmt_data())
                 removeTab(inputId = "AllTabs",
                           target  = "RefMetTab")
                 appendTab(inputId = "AllTabs",
                           tabPanel(
                             "Reference Metabolites",
                             value = "RefMetTab",
                             sidebarLayout(
                               sidebarPanel(
                                 ref_data_uploadUI(id = "ref_data_init", ref_db = refmets),
                                 ref_data_profileUI(id = "ref_data_edits")
                               ),
                               mainPanel(
                                 tabsetPanel(
                                   id = "refout_tabs",
                                   tabPanel(
                                     title = "Reference Data Editing",
                                     tabsetPanel(
                                       id = "refdat_tab1",
                                       type = "hidden",
                                       selected = "tab1_hide",
                                       tabPanelBody(
                                         value = "tab1_show",
                                         ref_data_ROIeditingUI(id = "ref_data_edits")
                                       ),
                                       tabPanelBody(
                                         value = "tab1_hide"
                                       )
                                     )
                                   ),
                                   tabPanel(
                                     title = "Add/Remove Metabolites",
                                     tabsetPanel(
                                       id = "refdat_tab2",
                                       type = "hidden",
                                       selected = "tab2_hide",
                                       tabPanelBody(
                                         value = "tab2_show",
                                         ref_data_add_delUI(id = "ref_data_edits")
                                       ),
                                       tabPanelBody(
                                         value = "tab2_hide"
                                       )
                                     )
                                   ),
                                   tabPanel(
                                     title = "Reference Data for Quantification",
                                     tabsetPanel(
                                       id = "refdat_tab3",
                                       type = "hidden",
                                       selected = "tab3_hide",
                                       tabPanelBody(
                                         value = "tab3_show",
                                         ref_data_quantTab(id = "ref_data_edits")
                                       ),
                                       tabPanelBody(
                                         value = "tab3_hide"
                                       )
                                     )
                                   )
                                 )
                               )
                             )
                           ))
               })

  # Handle initial upload of reference metabolite datafile and/or initial specification of reference metabolites
  # The code for the ref_data_uploadServer() module is found in ./R/ref_data_upload.R
  ref_data <- ref_data_uploadServer(id        = "ref_data_init",
                                    xpmt_data = mod_xpmt_data,
                                    ref_db    = refmets)

  # Output a warning if any of uploaded reference metabolites not contained within database
  observe({
    req(ref_data())
    refwarn <-ref_data()$casno_not_in_db

    if(!is.null(refwarn)){
      removeTab(inputId = "refout_tabs", target = "Warnings")

      appendTab(inputId = "refout_tabs",
                tabPanel("Warnings",
                         h4("The following entries were not found in our database:"),
                         DT::dataTableOutput("user_refmet_nomatch"),
                         h4("Please visit the Add/Remove Metabolites tab to manually search our database.")))
    } else if(is.null(refwarn)){
      removeTab(inputId = "refout_tabs", target = "Warnings")
    }
  })

  observe({
    req(ref_data())

    updateTabsetPanel(inputId = "refdat_tab1", selected = "tab1_show")
    updateTabsetPanel(inputId = "refdat_tab2", selected = "tab2_show")
    updateTabsetPanel(inputId = "refdat_tab3", selected = "tab3_show")
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
                                         ref_db    = refmets)


  ### Tab: Profiling Data ----------------------------------------------------------

  profiling_results <- profilingServer(id = "profiling",
                                       xpmt_data = mod_xpmt_data,
                                       ref_data = mod_ref_data)

  observeEvent(profiling_results(),
               {
                 req(mod_xpmt_data())
                 req(mod_ref_data()$profile_confirm == TRUE)

                 removeTab(inputId = "AllTabs",
                           target  = "ProfilingTab")
                 appendTab(inputId = "AllTabs",
                           tabPanel(
                             title = "Profile Data",
                             value = "ProfilingTab",
                             tabsetPanel(
                               tabPanel(
                                 title = "Detailed View",
                                 profiling_completeviewUI(id = "profiling")
                               ),
                               tabPanel(
                                 title = "Full View",
                                 profiling_detailedviewUI(id = "profiling")
                               )
                             )
                           )
                        )
                 updateTabsetPanel(session, "AllTabs",
                                   selected = "ProfilingTab")
               })

  # observeEvent(profiling_results(),
  #              {
  #                req(mod_xpmt_data())
  #                req(mod_ref_data()$profile_confirm == TRUE)
  #
  #                removeTab(inputId = "AllTabs",
  #                          target  = "ProfilingTab")
  #                appendTab(inputId = "AllTabs",
  #                          tabPanel(
  #                            title = "Profile Data",
  #                            value = "ProfilingTab",
  #                            sidebarLayout(
  #                              sidebarPanel(
  #                                profiling_controlsUI(id = "profiling",
  #                                                     xpmt_data = mod_xpmt_data,
  #                                                     ref_data = mod_ref_data)
  #                              ),
  #
  #                              mainPanel(
  #                                tabsetPanel(
  #                                  tabPanel(
  #                                    title = "Plot View",
  #                                    profiling_trelliscopeUI("profiling")
  #                                  ),
  #                                  tabPanel(
  #                                    title = "Quantification",
  #                                    profiling_quantificationUI("profiling")
  #                                  ),
  #                                  tabPanel(
  #                                    title = "Profile Summary",
  #                                    profiling_summaryUI("profiling")
  #                                  )
  #                                )
  #                              )
  #                            )
  #                          )
  #
  #                )
  #                updateTabsetPanel(session, "AllTabs",
  #                                  selected = "ProfilingTab")
  #              })

}
