#' Create Trelliscope plot
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
#' @param plot.data Resultant object from \code{\link{format_plotting}}
#'
#' @return Plot object for Trelliscope Plotting
#'
#' @author Allison Thompson
#'
#' @export

trel_plot <- function(plot.data){

  plot.data %>%
    tidyr::nest(data = !tidyselect::one_of(c("Sample", "Metabolite"))) %>%
    dplyr::mutate(Quantification    = purrr::map_dbl(data, ~ unique(.$Quantification)),
                  Signal_Area_Ratio = purrr::map_dbl(data, ~ unique(.$Signal_Area_Ratio)),
                  Fitting_Error     = purrr::map_dbl(data, ~ unique(.$Fitting_Error)),
                  Chemical_Shift    = purrr::map_dbl(data, ~ unique(.$Chemical_Shift)),
                  Intensity         = purrr::map_dbl(data, ~ unique(.$Intensity)),
                  Half_Bandwidth    = purrr::map_dbl(data, ~ unique(.$Half_Bandwidth)),
                  panel             = trelliscopejs::map_plot(data, function(x){
                    ggplot2::ggplot(data               = subset(x, x$variable != "Quantified Signal"),
                                    ggplot2::aes(x     = .data$Xdata,
                                                 y     = .data$value,
                                                 color = .data$variable)) +
                      ggplot2::geom_line() +
                      ggplot2::geom_line(data = subset(x, x$variable == "Quantified Signal"),
                                         ggplot2::aes(x     = .data$Xdata,
                                                      y     = .data$value,
                                                      color = .data$variable),
                                         alpha = 0.5) +
                      ggplot2::xlab('PPM') +
                      ggplot2::ylab('Intensity')
                    })
                  )
}
