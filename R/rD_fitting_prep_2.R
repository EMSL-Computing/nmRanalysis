#' rDolphin: fitting_prep_2
#'
#' @param Xdata dataframe of chemical shift values.
#' @param Ydata dataframe of intensity values.
#' @param initial_fit_parameters dataframe of signal fitting parameters
#' @param program_parameters list of global profiling parameters
#' @param created_baseline dataframe of baseline intensity values
#' @param max_shift dataframe of max chemical shift values
#' @param min_shift dataframe of min chemical shift values
#' @param max_intensity dataframe of max intensity values
#' @param min_intensity dataframe of min intensity values
#' @param max_width dataframe of max half bandwidth values
#' @param min_width dataframe of min half bandwidth values
#' @param spectrum_index index of sample spectrum to be fitted
#' @param signal_index index of signal to be fitted
#'
#' @details This function was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This is function is used to collate
#' the data necessary for the downstream fitting algorithm and associated functions, assuming that profiling has occurred at least once before.
#'
fitting_prep_2 <- function(Xdata, Ydata, initial_fit_parameters, program_parameters, created_baseline, max_shift,
                           min_shift, max_intensity,
                           min_intensity, max_width, min_width, spectrum_index, signal_index){

  Ydata[Ydata<0]=0
  min_intensity[spectrum_index,signal_index][is.na(min_intensity[spectrum_index,signal_index])]=0
  max_intensity[spectrum_index,signal_index][is.na(max_intensity[spectrum_index,signal_index])]=max(Ydata)

  colnames(initial_fit_parameters) = c(
    "quantification_or_not",
    "positions",
    "shift_tolerance",
    "widths",
    "multiplicities",
    "Jcoupling",
    "roof_effect",
    "Jcoupling2",
    "roof_effect2"
  )
  signals_to_fit = length(initial_fit_parameters$positions)
  ROIlength = length(Xdata)


  #Calculation of number of background signals, if baseline fitting is performed
  BGSigNum = ifelse(program_parameters$clean_fit == 'N', max(round(abs(Xdata[1] -
                                                                         Xdata[ROIlength]) * program_parameters$BGdensity), 3), 0)

  #Preallocation of parameters to optimize into a matrix of features
  FeaturesMatrix = matrix(NA, (signals_to_fit + BGSigNum), 15)

  # 6/6/22 modification by Javier E. Flores to allow for character-valued multiplicities
  FeaturesMatrix <- as.data.frame(FeaturesMatrix)

  colnames(FeaturesMatrix) = c(
    'minimum_intensity',
    'maximum_intensity',
    'shift_left_limit',
    'shift_right_limit',
    'minimum_width',
    'maximum_width',
    'minimum_gaussian',
    'maximum_gaussian',
    'minimum_J_coupling',
    'maximum_J_coupling',
    'multiplicities',
    'roof_effect',
    'minimum_J_coupling2',
    'maximum_J_coupling2',
    'roof_effect2'
  )

  #Parameters of signals to fit
  FeaturesMatrix[1:signals_to_fit, 1] = min_intensity[spectrum_index,signal_index]
  FeaturesMatrix[1:signals_to_fit, 2] = max_intensity[spectrum_index,signal_index]
  FeaturesMatrix[1:signals_to_fit, 3] = min_shift[spectrum_index,signal_index]
  FeaturesMatrix[1:signals_to_fit, 4] = max_shift[spectrum_index,signal_index]
  FeaturesMatrix[1:signals_to_fit, 5] = min_width[spectrum_index,signal_index]
  FeaturesMatrix[1:signals_to_fit, 6] = max_width[spectrum_index,signal_index]
  FeaturesMatrix[1:signals_to_fit, 7] = 0
  FeaturesMatrix[1:signals_to_fit, 8] = program_parameters$gaussian
  FeaturesMatrix[1:signals_to_fit, 9] = initial_fit_parameters$Jcoupling -
    program_parameters$j_coupling_variation
  FeaturesMatrix[1:signals_to_fit, 10] = initial_fit_parameters$Jcoupling +
    program_parameters$j_coupling_variation
  FeaturesMatrix[1:signals_to_fit, 11] = initial_fit_parameters$multiplicities
  FeaturesMatrix[1:signals_to_fit, 12] = initial_fit_parameters$roof_effect
  # 6/7/22 modification by Javier E. Flores to accommodate second J coupling value and second roof effect (for dd only)
  FeaturesMatrix[1:signals_to_fit, 13] = initial_fit_parameters$Jcoupling2 -
    program_parameters$j_coupling_variation
  FeaturesMatrix[1:signals_to_fit, 14] = initial_fit_parameters$Jcoupling2 +
    program_parameters$j_coupling_variation
  FeaturesMatrix[1:signals_to_fit, 15] = initial_fit_parameters$roof_effect2

  # 6/6/22 modification by Javier E. Flores to adapt to character-valued multiplicities
  # If the multiplicity is 1 or "s" for singlet, there is no j-coupling.
  FeaturesMatrix[initial_fit_parameters$multiplicities %in% c("1", "s"), 9:10] = 0

  #Finding of maximum intensity and chemical shift tolerance of every background signal
  if (BGSigNum>0) {
    BGSigrightlimits = seq(Xdata[1]-0.005, Xdata[ROIlength]+0.005, length = BGSigNum) -
      0.005
    BGSigleftlimits = BGSigrightlimits + 0.01

    peaks = peakdet(Ydata, program_parameters$peakdet_minimum*max(1e-10,max(Ydata)))
    left = which(peaks$mintab$pos < ROIlength / 5)
    right = which(peaks$mintab$pos > 4 * ROIlength / 5)
    dummy = round(seq(1, ROIlength, length = 2 * BGSigNum - 1))
    BGleftlimits = dummy[c(1, seq(2, length(dummy) - 1, 2))]
    BGrightlimits = dummy[c(seq(2, length(dummy) - 1, 2), length(dummy))]
    BGSig_maximums = replicate(BGSigNum, NA)
    for (ss in 1:BGSigNum)
      BGSig_maximums[ss] = min(Ydata[BGleftlimits[ss]:BGrightlimits[ss]])

    # Note that "BG_width_factor" is never defined. Therefore, modify original code:
    # BG_width=max(min(initial_fit_parameters$widths,na.rm=T)*program_parameters$BG_width_factor,program_parameters$BG_width)
    BG_width=max(min(initial_fit_parameters$widths,na.rm=T),program_parameters$BG_width)
    #Parameters of background signals
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 1] = 0
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 2] = BGSig_maximums
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 3] = BGSigrightlimits
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 4] = BGSigleftlimits
    # FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 5] = (1.5 /
    #                                                                     program_parameters$freq) * 10
    # FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 6] = (1.5 /
    #                                                                      program_parameters$freq) * 15
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 5] = BG_width-program_parameters$BG_width_tolerance #(modified from original code)
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 6] = BG_width+program_parameters$BG_width_tolerance #(modified from original code)
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 7] = 0
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 8] = program_parameters$BG_gaussian_percentage
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 9] = 0
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 10] = 0 #j coupling makes no sense with backgorund signals
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 11] = "0" #arbitrary value used to signal later background signals
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 12] = 0
    # 6/7/22 modification by Javier E. Flores to accommodate second J coupling value and second roof effect (for dd only)
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 13] = 0
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 14] = 0 #j coupling2 makes no sense with backgorund signals
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 15] = 0

    # optimization of baseline parameters , to be sure that the algorithm doesn ot try ti fot spurious signals as basleine
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix),2] = fittingloop_bg(FeaturesMatrix     = FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix),],
                                                                                 Xdata              = Xdata,
                                                                                 Ydata              = created_baseline,
                                                                                 program_parameters = program_parameters)$BG_intensities


  }


  return(FeaturesMatrix)
}
