#' Create Trelliscope plot
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
