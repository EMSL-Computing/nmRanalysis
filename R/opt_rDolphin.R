#' Optimization check for rDolphin
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
#'
#' @param imported_data an rDolphin object, created using \code{\link{ppmData_to_rDolphin}}
#'
#' @author Allison Thompson
#'
#' @importFrom magrittr %>%
#'
#' @export
opt_rDolphin <- function(imported_data){

  if(!inherits(imported_data, "rDolphin")){
    stop("'imported_data' must be 'rDolphin' object, created using `ppmData_to_rDolphin`")
  }

  # create exemplars plot
  ep <- rDolphin::exemplars_plot(imported_data) %>% plotly::layout(title = 'Exemplars Plot')
  print(ep)
  #plotly::orca(ep, file=paste(dir, "exemplars_plot_", Sys.Date(), ".png", sep=""))

  # create median plot
  mp <- rDolphin::median_plot(imported_data) %>% plotly::layout(title = 'Median Plot')
  print(mp)
  #plotly::orca(mp, file=paste(dir, "median_plot_", Sys.Date(), ".png", sep=""))

  # profiling model
  profiling_model <- rDolphin::profile_model_spectrum(imported_data, imported_data$ROI_data)
  pm              <- profiling_model$p %>% plotly::layout(title = 'Profiling Model')
  print(pm)
  #plotly::orca(pm, file=paste(dir, "profiling_plot_", Sys.Date(), ".png", sep=""))

  return(list(total_signals_parameters = profiling_model$total_signals_parameters, # a data frame with parameters of fitting and of quality of fitting (e.g.  the fitting error) for every quantified signal.
              ROI_data                 = profiling_model$ROI_data)) # a data frame of the ROI data used during the process.
}
