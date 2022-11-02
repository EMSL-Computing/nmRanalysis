#' rDolphin: profiling_func
#'
#' @param spectrum_index Index of sample spectrum to be fitted
#' @param signals_codes Signal indices to be fitted
#' @param imported_data Data object containing all experimental data, metadata, and program parameters
#' @param ROI_buckets Indices for the ROI
#' @param fitting_type One of "Clean Sum", "Baseline Sum", "Clean Fitting", or "Baseline Fitting".
#' @param program_parameters List of parameters relevant to fitting algorithm.
#' @param Xdata Dataframe of sample chemical shift (ppm) values.
#' @param Ydata Dataframe of intensity values.
#' @param final_output Dataframe for final quantifications
#' @param reproducibility_data Dataframe for all fitting information
#' @param ROI_profile ROI fitting parameter data
#' @param baselinedataset Dataframe of baseline intensities
#' @param signals_to_quantify Indices of signals that were quantified
#' @param pb progress bar object
#' @param reimplementation Whether this is a subsequent implementation of algorithm
#' @param max_shift Only applicable if reimplementation. Dataframe of max chemical shift values
#' @param min_shift Only applicable if reimplementation. Dataframe of min chemical shift values
#' @param max_intensity Only applicable if reimplementation. Dataframe of max intensities
#' @param min_intensity Only applicable if reimplementation. Dataframe of min intensities
#' @param max_width Only applicable if reimplementation. Dataframe of max linewidth
#' @param min_width Only applicable if reimplementation. Dataframe of min linewidth
#' @param signal_index Dataframe of max linewidth
#'
#' @details This function is based on code that was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This is one of the
#' main functions used by rDolphin to perform automatic profiling. Modifications have been made to the original function to allow for
#' the fitting of double-doublets. Generally speaking, this involves the addition of new fields (a second J-coupling field and second
#' roof effect field) containing parameters necessary to fit this new pattern.
#'
profiling_func <- function(spectrum_index, signals_codes, imported_data,ROI_buckets,
                           fitting_type, program_parameters, Xdata, Ydata, final_output,
                           reproducibility_data, ROI_profile, baselinedataset, signals_to_quantify,
                           pb = NULL, reimplementation = F, max_shift = NULL, min_shift = NULL, max_intensity = NULL,
                           min_intensity = NULL, max_width = NULL, min_width = NULL, signal_index = NULL){
  #Preparation of necessary variables to store figures and information of the fitting
  Ydata <- as.numeric(imported_data$dataset[spectrum_index, ROI_buckets])

  #If the quantification is through integration with or without baseline
  if (fitting_type == "Clean Sum" || fitting_type == "Baseline Sum") {

    dummy <- integration(program_parameters$clean_fit, Xdata, Ydata, program_parameters$buck_step)

    results_to_save <- dummy$results_to_save
    #Generation of useful variables specific of every quantification
    reproducibility_data[[spectrum_index]][[signals_codes]]$ROI_profile     <- ROI_profile
    reproducibility_data[[spectrum_index]][[signals_codes]]$plot_data       <- dummy$plot_data
    reproducibility_data[[spectrum_index]][[signals_codes]]$Xdata           <- Xdata
    reproducibility_data[[spectrum_index]][[signals_codes]]$Ydata           <- Ydata
    reproducibility_data[[spectrum_index]][[signals_codes]]$results_to_save <- results_to_save
    reproducibility_data[[spectrum_index]][[signals_codes]]$error1          <- results_to_save$fitting_error

    #If the quantification is through fitting with or without baseline
  } else if (fitting_type == "Clean Fitting" || fitting_type == "Baseline Fitting") {

    #Adaptation of the info of the parameters into a single matrix and preparation (if necessary) of the background signals that will conform the baseline
    if (reimplementation == F){
      FeaturesMatrix <- fitting_prep(Xdata                  = Xdata,
                                     Ydata                  = Ydata,
                                     # modified to include J coupling 2 and roof effect 2
                                     initial_fit_parameters = ROI_profile[, 5:13,drop=F],
                                     program_parameters     = program_parameters,
                                     created_baseline       = baselinedataset[spectrum_index,ROI_buckets])
    }

    if (reimplementation == T){
      FeaturesMatrix <- fitting_prep_2(Xdata                  = Xdata,
                                       Ydata                  = Ydata,
                                       # modified to include J coupling 2 and roof effect 2
                                       initial_fit_parameters = ROI_profile[, 5:13,drop=F],
                                       program_parameters     = program_parameters,
                                       created_baseline       = baselinedataset[spectrum_index, ROI_buckets],
                                       max_shift              = max_shift,
                                       min_shift              = min_shift,
                                       max_intensity          = max_intensity,
                                       min_intensity          = min_intensity,
                                       max_width              = max_width,
                                       min_width              = min_width,
                                       spectrum_index         = spectrum_index,
                                       signal_index           = signal_index)

    }
    #Calculation of the parameters that will achieve the best fitting
    dummy <- fittingloop(FeaturesMatrix     = FeaturesMatrix,
                         Xdata              = Xdata,
                         Ydata              = Ydata,
                         program_parameters = program_parameters)
    signals_parameters <- dummy$signals_parameters
    Xdata_2            <- imported_data$ppm
    Ydata_2            <- as.numeric(imported_data$dataset[spectrum_index, ])

    #Fitting of the signals
    # modified to include J coupling 2 and roof effect 2
    multiplicities <- c(FeaturesMatrix[, 11], rep("1", (length(signals_parameters) / 6) - dim(FeaturesMatrix)[1]))
    roof_effect    <- c(FeaturesMatrix[, 12], rep(0, (length(signals_parameters) / 6) - dim(FeaturesMatrix)[1]))

    # modified to include J coupling 2 and roof effect 2
    roof_effect2 <- c(FeaturesMatrix[, 15], rep(0, (length(signals_parameters) / 6) - dim(FeaturesMatrix)[1]))

    # modified to include J coupling 2 and roof effect 2
    # Note that j coupling 2 is baked into parS argument
    fitted_signals <- signal_fitting(parS           = signals_parameters,
                                     Xdata          = Xdata_2,
                                     multiplicities = multiplicities,
                                     roof_effect    = roof_effect,
                                     roof_effect2   = roof_effect2,
                                     freq           = program_parameters$freq)
    dim(signals_parameters)      <- c(6, length(signals_parameters) / 6)
    rownames(signals_parameters) <- c('intensity', '$chemical_shift', 'half_bandwidth', 'gaussian', 'J_coupling', 'J_coupling2')
    signals_parameters           <- rbind(signals_parameters, multiplicities, roof_effect, roof_effect2)

    #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
    dummy <- output_generator(signals_to_quantify = signals_to_quantify,
                              fitted_signals      = fitted_signals,
                              Ydata               = Ydata_2,
                              Xdata               = Xdata_2,
                              signals_parameters  = signals_parameters,
                              multiplicities      = multiplicities,
                              buck_step           = program_parameters$buck_step)
    output_data <- dummy$output_data
    error1      <- ifelse(is.nan(dummy$error1), 3000, dummy$error1)

    #Generation of the dataframe with the final output variables
    results_to_save <- data.frame(chemical_shift    = output_data$chemical_shift,
                                  quantification    = output_data$quantification,
                                  signal_area_ratio = output_data$signal_area_ratio,
                                  fitting_error     = output_data$fitting_error,
                                  intensity         = output_data$intensity,
                                  half_bandwidth    = output_data$half_bandwidth)

    #Generation of the figure data
    plot_data <- rbind(output_data$signals_sum, output_data$baseline_sum, output_data$fitted_sum, output_data$signals)
    plot_data <- plot_data[, ROI_buckets]

    rownames(plot_data) <- c("signals_sum", "baseline_sum", "fitted_sum", make.names(paste(ROI_profile[, 4], ROI_profile[, 5], sep = '_')), rep('additional signal', dim(plot_data)[1] - length(ROI_profile[, 4]) - 3))

    #Generation of useful variables specific of every quantification
    for (i in seq_along(signals_codes)) {
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$ROI_profile        <- ROI_profile
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$program_parameters <- program_parameters
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$plot_data          <- plot_data
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$error1             <- error1
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$FeaturesMatrix     <- FeaturesMatrix
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$signals_parameters <- signals_parameters
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$Xdata              <- Xdata
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$Ydata              <- Ydata
      reproducibility_data[[spectrum_index]][[signals_codes[i]]]$results_to_save    <- results_to_save
    }
  }

  #Generation of output variables specific of every quantification
  final_output <- save_output(spectrum_index  = spectrum_index,
                              signals_codes   = signals_codes,
                              results_to_save = results_to_save,
                              buck_step       = imported_data$buck_step,
                              final_output    = final_output)

  if (!is.null(pb)){
    setTxtProgressBar(pb, spectrum_index)
  }

  profiling_data <- list(final_output         = final_output,
                         reproducibility_data = reproducibility_data)

  return(profiling_data)
}
