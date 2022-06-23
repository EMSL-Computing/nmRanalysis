#' Main UI function of nmRapp
#'
#' @param request default parameter necessary for specific UI as a function.
#'
#' @details This main UI function references the UI components of modules specific to the processing and visualization
#' of experimental data and metadata. Refer to the documentation of each referenced module for details on its specific function.
#' A list of referenced modules is provided below:
#' 1) xpmt_data_uploadUI()
#' 2) xpmt_data_modifyUI()
#' 3) xpmt_data_vizUI()
#' 4) xpmt_metadata_vizUI()
#'
#' This main UI function also creates the global UI format, i.e. navbarPage(), under which several different global tabs corresponding
#' to different stages of the NMR metabolite profiling workflow are made available. This environment is initialized with only
#' the experimental data upload and processing tab. The main server function, nmRapp_server(), dynamically updates the environment
#' with additional global tabs depending on the completion of the current tab requirements. For example, a "target metabolite editing"
#' tab is made available through nmRapp_server() only after the user has uploaded and processed their experimental data and metadata.
#'
#' @import shiny
#'
nmRapp_ui <- function(request){
  # navBarPage specifies a template for the general page layout of the UI
  # This template consists of several panels accessible via tabs
  navbarPage(
    "nmRanalysis GUI",
    id = "AllTabs", # unique identifier for navbarPage

    # The first panel/tab of the UI
    tabPanel(
      "Data Processing", # Name of panel
      value = "UploadTab", # unique identifier for panel

      shinyWidgets::useSweetAlert(), # needed with progressSweetAlert (applies for entire app)
      shinyFeedback::useShinyFeedback(), # set up shinyFeedback (applies for entire app)

      # sidebarLayout specifies a template for the layout of the panel.
      # This template is comprised of a sidebar panel and main panel.
      sidebarLayout(
        # Specify the elements contained in the sidebarPanel
        sidebarPanel(
          xpmt_data_uploadUI(id = "xpmt", ref_db = bmse_associations)
        ),

        # Specify elements to include in main panel
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "Experimental Data",
              tabsetPanel(
                id = "xpmtdat_tab1",
                type = "hidden",
                selected = "xpmt_tab1_hide",
                tabPanelBody(
                  value = "xpmt_tab1_show",
                  xpmt_data_vizoptionsUI(id = "xpmt_viz"),
                  xpmt_data_vizUI(id = "xpmt_viz")
                ),
                tabPanelBody(
                  value = "xpmt_tab1_hide"
                )
              )
            ),
            tabPanel(
              "Experimental Metadata",
              xpmt_metadata_vizUI(id = "xpmt_viz")
            )
          )
        )
      )
    )
  )
}




