#' Main UI function of nmRapp
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
          xpmt_data_uploadUI(id = "xpmt", ref_db = bmse_associations),
          uiOutput("wizard_exptoref_ui")
        ),

        # Specify elements to include in main panel
        mainPanel(
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
        )
      )
    ),

    # The second tab of the UI
    tabPanel(
      "Reference Metabolites",
      value = "RefMetTab",
      sidebarLayout(
        sidebarPanel(
          ref_data_uploadUI(id = "ref_data_init", ref_db = bmse_associations),
          h4(""),
          fluidRow(
            column(6,
                   shinyWidgets::actionBttn(
                     inputId = "wizard_reftoexp",
                     label = "Experimental Data Upload",
                     style = "minimal",
                     color = "primary",
                     icon = icon("arrow-left"),
                     size = "sm")
                   ),
            column(3, offset = 3,
                   uiOutput("wizard_reftoprof_ui")
                   )
          )
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
    )

  )
}




