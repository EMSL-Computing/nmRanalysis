#' Wrapper for running rDolphin
#'
#' Runs rDolphin using imported_data (the output from \code{\link{ppmData_to_rDolphin}}) and \code{\link{opt_rDolphin}}
#'
#' @param imported_data an rDolphin data object, often created by \code{ppmData_to_rDolphin}
#' @param write_results logical, if true, results written to specified directory. Default is FALSE.
#' @param dir Directory where to save results, default is NA, which will create a folder in your working directory.
#' @param optimization Logical, should profiling quality be maximized through analysis of signal parameters. Default is TRUE.
#' @param spectra_to_profile A vector of spectrum indices. Defaults to NULL, profiling all spectra.
#'
#' @author Allison Thompson
#'
#' @export
run_rDolphin <- function(imported_data, write_results = FALSE, dir = NULL, optimization = TRUE, spectra_to_profile = NULL){

  # check parameter inputs
  if(!inherits(imported_data, "rDolphin")){
    stop("'imported_data' must be 'rDolphin' object, created using `ppmData_to_rDolphin`")
  }
  if(!is.null(dir)) {
    if(!dir.exists(dir)){
      stop(paste0("Directory ", dir, " does not exist"))
    }
  }
  if(!is.logical(optimization)){
    stop("'optimization' must be logical value")
  }
  if(!is.null(spectra_to_profile)) {
    if(!is.numeric(spectra_to_profile)){
      stop("'spectra_to_profile' must be numeric vector containing indices of selected spectra in `imported_data$dataset`")
    }
  }

  # Run rDolphin
  profiling_data <- rDolphin::automatic_profiling(imported_data = imported_data,
                                                  ROI_data = imported_data$ROI_data,
                                                  optimization = optimization,
                                                  spectra_to_profile = spectra_to_profile)

  # export results
  if(write_results == TRUE) {
    if(is.null(dir)){
      rDolphin::write_info(export_path  = paste(getwd(), "/results_", Sys.Date(), sep=""),
                           final_output = profiling_data$final_output,
                           ROI_data     = imported_data$ROI_data)
      cat("Writing results output to ", paste(getwd(), "/results_", Sys.Date(), sep= ""), ".\n")
    } else{
      rDolphin::write_info(dir, profiling_data$final_output, imported_data$ROI_data)
      cat("Writing results output to ", dir, ".\n")
    }
  }

  return(profiling_data)
}
