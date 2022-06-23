#' Semi-Automated Targeted Metabolite NMR Profiling
#'
#' @param ... additional arguments to be passed to shinyApp().
#'
#' @details Calls to this function create a new instance of a shiny app developed for semi-automated
#' targeted metabolite NMR profiling.
#'
#' @export
#'
#' @import shiny
#'
nmRapp <- function(...){

  # Option specifies the max datafile size that may be loaded into shiny
  options(shiny.maxRequestSize=30*1024^2)

  ui <- nmRapp_ui
  server <- nmRapp_server
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE), ...)
}
