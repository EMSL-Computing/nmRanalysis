#' rDolphin: fittingloop_bg
#'
#' @param FeaturesMatrix Dataframe containing signal parameter information (e.g. multiplicity, roof effects)
#' @param Xdata Dataframe of sample chemical shift (ppm) values.
#' @param Ydata Dataframe of intensity values.
#' @param program_parameters List of parameters relevant to fitting algorithm.
#'
#' @details This function is based on code that was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This function is responsible
#' for fitting background signals to the observed data. Modifications have been made to the original function to allow for
#' the fitting of double-doublets. Generally speaking, this involves the addition of new fields (a second J-coupling field and second
#' roof effect field) containing parameters necessary to fit this new pattern.
#'
fittingloop_bg <- function(FeaturesMatrix, Xdata, Ydata, program_parameters){

  # Modification allow for additional J coupling value and roof effect value (for dd case)
  # Note that the additional J coupling value is baked into par (parS) so that it may be optimized along with the other
  # j coupling value.
  residFun <- function(par, observed, xx, multiplicities, roof_effect, roof_effect2, freq){
    observed - colSums(signal_fitting(parS           = par,
                                      Xdata          = xx,
                                      multiplicities = multiplicities,
                                      roof_effect    = roof_effect,
                                      roof_effect2   = roof_effect2,
                                      freq           = freq))
  }


  # Loop to control if additional signals are incorporated, until a maximum of iterations specified bt fitting_maxiterrep.
  # If at the last fitting the improvement was lesser than 25% respective to the previous fitting,
  # iterrep becomes equal to fitting_maxiterrep and the loop is stooped
  lb <- as.vector(t(FeaturesMatrix[, c(seq(1, 9, 2), 13), drop = F])) # modified to allow for additional J coupling value (for dd case)
  ub <- as.vector(t(FeaturesMatrix[, c(seq(2, 10, 2), 14), drop = F])) # modified to allow for additional J coupling value (for dd case)

  multiplicities <- FeaturesMatrix[, 11]
  roof_effect    <- FeaturesMatrix[, 12]
  roof_effect2   <- FeaturesMatrix[, 15]

  s0 <- lb + (ub - lb) * stats::runif(length(ub))

  nls.out <- minpack.lm::nls.lm(par            = s0, # modified to allow for additional J coupling value (for dd case)
                                fn             = residFun,
                                observed       = Ydata,
                                xx             = Xdata,
                                multiplicities = multiplicities,
                                roof_effect    = roof_effect,
                                roof_effect2   = roof_effect2,
                                lower          = lb,
                                upper          = ub,
                                freq           = program_parameters$freq,
                                control        = minpack.lm::nls.lm.control(factor  = program_parameters$factor,
                                                                            maxiter = program_parameters$nls_lm_maxiter,
                                                                            ftol    = program_parameters$ftol,
                                                                            ptol    = program_parameters$ptol
                                                                            )
                                )

  # modified to allow for additional J coupling value (for dd case)
  dummy <- list(BG_intensities = stats::coef(nls.out)[which(seq(length(stats::coef(nls.out))) %% 6 == 1)],
                baseline       = Ydata - nls.out$fvec)

  return(dummy)
}
