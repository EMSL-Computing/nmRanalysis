#' Optimization check for rDolphin
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
