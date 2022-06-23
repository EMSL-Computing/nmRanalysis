#' rDolphin: fitting_prep
#'
#' @param Xdata 
#' @param Ydata 
#' @param initial_fit_parameters 
#' @param program_parameters 
#' @param created_baseline 
#'
#' @details This function was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This is function is used to collate 
#' the data necessary for the downstream fitting algorithm and associated functions. Modifications have been made to the original function to allow for
#' the fitting of double-doublets. Generally speaking, this involves the addition of new fields (a second J-coupling field and second
#' roof effect field) containing parameters necessary to fit this new pattern.
#'
fitting_prep <- function(Xdata, Ydata, initial_fit_parameters, program_parameters, created_baseline){
  
  #Created by Daniel Canueto 30/08/2016
  #Preparation of parameters to optimize to achieve the best fitting
  
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
  Ydata[Ydata<0]=0
  signals_to_fit = length(initial_fit_parameters$positions)
  ROIlength = length(Xdata)
  
  
  #Calculation of number of background signals, if baseline fitting is performed
  BGSigNum = ifelse(program_parameters$clean_fit == 'N', max(round(abs(Xdata[1] -
                                                                         Xdata[ROIlength]) * program_parameters$BGdensity), 3), 0)
  
  #Preallocation of parameters to optimize into a matrix of features
  FeaturesMatrix = matrix(NA, (signals_to_fit + BGSigNum), 15)
  
  # 6/6/22 modification by Javier E. Flores to allow for character-valued multiplicities and Jcoupling2 and roof effect 2
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
  FeaturesMatrix[1:signals_to_fit, 1] = 0
  FeaturesMatrix[1:signals_to_fit, 2] = max(Ydata)
  FeaturesMatrix[1:signals_to_fit, 3] = initial_fit_parameters$positions -
    initial_fit_parameters$shift_tolerance
  FeaturesMatrix[1:signals_to_fit, 4] = initial_fit_parameters$positions +
    initial_fit_parameters$shift_tolerance
  FeaturesMatrix[1:signals_to_fit, 5] = initial_fit_parameters$widths- initial_fit_parameters$widths * program_parameters$widthtolerance
  FeaturesMatrix[1:signals_to_fit, 6] = initial_fit_parameters$widths + initial_fit_parameters$widths * program_parameters$widthtolerance
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
  
  #Finding of maximum intensity and $chemical_shift tolerance of every background signal
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
    
    BG_width=max(min(initial_fit_parameters$widths,na.rm=T)*program_parameters$BG_width_factor,program_parameters$BG_width)
    #Parameters of background signals
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 1] = 0
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 2] = BGSig_maximums
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 3] = BGSigrightlimits
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 4] = BGSigleftlimits
    # FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 5] = (1.5 /
    #                                                                     program_parameters$freq) * 10
    # FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 6] = (1.5 /
    #                                                                      program_parameters$freq) * 15
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 5] = BG_width*(1-program_parameters$BG_width_tolerance)
    FeaturesMatrix[(signals_to_fit + 1):nrow(FeaturesMatrix), 6] = BG_width*(1+program_parameters$BG_width_tolerance)
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