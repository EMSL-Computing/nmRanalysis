#' Semi-Automated Targeted Metabolite NMR Profiling
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
  shinyApp(ui = ui, server = server, options = list(launch.browser = FALSE), ...)
}
