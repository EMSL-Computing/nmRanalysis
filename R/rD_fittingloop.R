#' rDolphin: fittingloop
#'
#' @param FeaturesMatrix Dataframe containing signal parameter information (e.g. multiplicity, roof effects)
#' @param Xdata Dataframe of sample chemical shift (ppm) values.
#' @param Ydata Dataframe of intensity values.
#' @param program_parameters List of parameters relevant to fitting algorithm.
#'
#' @details This function is based on code that was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This is the function
#' responsible for optimizing the fit of the signals of interest. Modifications have been made to the original function to allow for
#' the fitting of double-doublets. Generally speaking, this involves the addition of new fields (a second J-coupling field and second
#' roof effect field) containing parameters necessary to fit this new pattern.
#'
fittingloop <- function(FeaturesMatrix, Xdata, Ydata, program_parameters){

  #Preallocation of output and setting of necessary variables for loop
  # modified to allow for second J-coupling value in optimization
  signals_parameters <- rep(0, length(as.vector(t(FeaturesMatrix[, c(seq(1, 9, 2), 13), drop = F]))))

  iterrep            <- 0
  fitting_maxiterrep <- program_parameters$fitting_maxiterrep
  signals_to_fit     <- which(FeaturesMatrix[, 11] != "0")
  paramprov          <- rep(0, nrow(FeaturesMatrix) * 6) # modified to allow for second J-coupling value in optimization





  #Necessary information to incorporate additional signals if necessary
  range_ind <- round(program_parameters$additional_signal_ppm_distance / program_parameters$buck_step)



  #Function where to find a minimum
  # modified to allow for additional J coupling value and roof effect (for dd case)
  # Note that the additional J coupling value is baked into par (parS) so that it may be optimized along with the other
  # j coupling value.
  residFun <- function(par, observed, xx, multiplicities, roof_effect, roof_effect2, freq, bins){
    observed[bins] - colSums(signal_fitting(parS           = par,
                                            Xdata          = xx,
                                            multiplicities = multiplicities,
                                            roof_effect    = roof_effect,
                                            roof_effect2   = roof_effect2,
                                            freq           = freq))[bins]
  }


  # Loop to control if additional signals are incorporated, until a maximum of iterations specified bt fitting_maxiterrep.
  # If at the last fitting the improvement was lesser than 25% respective to the previous fitting,
  # iterrep becomes equal to fitting_maxiterrep and the loop is stooped
  while (iterrep <= fitting_maxiterrep) {

    iter               <- 0
    errorprov = error1 <- 3000
    worsterror         <- 0
    dummy = error2     <- 3000
    multiplicities     <- FeaturesMatrix[, 11]
    roof_effect        <- FeaturesMatrix[, 12]
    # modified to allow for additional J coupling value (for dd case)
    # Note that the second J coupling is included in parS
    roof_effect2       <- FeaturesMatrix[, 15]
    fitted_signals     <- signal_fitting(parS           = as.vector(t(FeaturesMatrix[, c(2,3,6,7,9,13)])),
                                         Xdata          = Xdata,
                                         multiplicities = multiplicities,
                                         roof_effect    = roof_effect,
                                         roof_effect2   = roof_effect2,
                                         freq           = program_parameters$freq)
    bins <- c()
    for (i in signals_to_fit) {
      sorted_bins <- sort(fitted_signals[i,] / sum(fitted_signals[i,]), decreasing = T, index.return = T)

      if(length(sorted_bins$x) > 0){
        # This section may need to be edited to accommodate second J-coupling value
        # since the quantities seem to depend on the existing J coupling value. However, the purpose
        # and/or function of this code chunk is unclear so any necessary modifications are unclear.
        bins2 <- sorted_bins$ix[1:which.min(abs(cumsum(sorted_bins$x) - 0.9))]

        # modified to adapt to FeaturesMatrix as a dataframe
        distance  <- diff(as.numeric(FeaturesMatrix[i, 3:4])) / program_parameters$buck_step
        distance2 <- round(diff(as.numeric(FeaturesMatrix[i, 9:10])) / program_parameters$buck_step / program_parameters$freq)

        bins2 <- unique(as.vector(sapply(seq(distance), function(x){bins2 - x})))
        bins2 <- unique(bins2, min(bins2) - distance2, max(bins2) + distance2)
        bins2 <- bins2[bins2 > 0 & bins2 < length(Xdata)]

        # aa=peakdet(fitted_signals[i,],0.00001)$maxtab$pos
        # aa=as.vector(sapply(seq(distance),function(x)aa-x))
        # #
        #  lol=sapply(seq(distance),function(x)min(Ydata[bins2]-fitted_signals[i,(bins2-x)]))
        # FeaturesMatrix[i,2]=FeaturesMatrix[i,2]+lol

        bins <- unique(c(bins, bins2))
      }}

    #Depending on the complexity of the ROI, more or less iterations are performed
    if (is.numeric(program_parameters$fitting_maxiter)) {
      fitting_maxiter <- program_parameters$fitting_maxiter

    } else {
      if (nrow(FeaturesMatrix) > 8 | any(FeaturesMatrix[, 4] - FeaturesMatrix[, 3] > 0.01)) {
        fitting_maxiter <- 20

      } else if ((nrow(FeaturesMatrix) > 5 && nrow(FeaturesMatrix) < 9)) {
        fitting_maxiter <- 14

      } else {
        fitting_maxiter <- 8
      }
    }


    #Conditions to keep the loop:
    # -The error is bigger than the specified in program_parameters
    # -There is no fitting with more than 66.7% improvement from the worst solution
    # -The loop has not arrived the specified maximum of iterations
    while (error1 > program_parameters$errorprov & error1 > (1 / 3 * worsterror) & iter < fitting_maxiter) {
      #Initialization of parameters to optimize. In every iteration the initialization will be different
      # modified to accommodate optimization of second J-coupling value.
      lb <- as.vector(t(FeaturesMatrix[, c(seq(1, 9, 2), 13), drop = F]))
      ub <- as.vector(t(FeaturesMatrix[, c(seq(2, 10, 2), 14), drop = F]))

      set.seed(iter)
      s0     <- lb + (ub - lb) * stats::runif(length(ub))
      order1 <- order(rowMeans(FeaturesMatrix[signals_to_fit, 3:4, drop=F])[signals_to_fit])

      # aaa=iter%%3/3
      # bbb=ifelse((iter+1)%%3/3==0,1,(iter+1)%%3/3)
      # s0[which(seq_along(s0)%%5==2)]=lb[which(seq_along(s0)%%5==2)] + (ub[which(seq_along(s0)%%5==2)] - lb[which(seq_along(s0)%%5==2)]) * runif(1,min=aaa,max=bbb)

      #During the first two iterations, find the peaks on the region of the spectrum. If the number of peaks is the same that the expected on the ROI and the location is similar, the signals are located where there are the peaks with minimum $chemical_shift tolerance.
      peaks_xdata <- peakdet(v     = c(Ydata[1], diff(Ydata)),
                             delta = program_parameters$peakdet_minimum * 0.1 * max(1e-10, max(Ydata)),
                             x     = Xdata)

      if (iter < 4 & length(peaks_xdata$maxtab$val) > 0) {
        peaks_bindata <- peakdet(v     = c(Ydata[1], diff(Ydata)),
                                 delta = program_parameters$peakdet_minimum * 0.1 * max(1e-10, max(Ydata)))

        # modified to adapt to multiplicity as character-valued
        multiplicity_total <- 0
        for(i in length(signals_to_fit)){
          if(multiplicities[i] %in% c("1", "s")){
            multiplicity_total <- multiplicity_total + 1

          } else if(multiplicities[i] %in% c("2", "d")){
            multiplicity_total <- multiplicity_total + 2

          } else if(multiplicities[i] %in% c("3", "t")){
            multiplicity_total <- multiplicity_total + 3

          } else if(multiplicities[i] %in% c("4", "q", "dd")){
            multiplicity_total <- multiplicity_total + 4

          } else if(!is.na(as.numeric(multiplicities[i]))){
            multiplicity_total <- multiplicity_total + as.numeric(multiplicities[i])

          }
        }

        # modified to adapt to multiplicity as character-valued
        peaks         <- peaks_xdata$maxtab$pos[sort(peaks_xdata$maxtab$val, decreasing = T, index.return = T)$ix[1:multiplicity_total]]
        peaks_compare <- rowMeans(FeaturesMatrix[signals_to_fit, 3:4, drop = F])
        for (i in 1:length(peaks_compare)) {
          # modified to adapt to multiplicity as character-valued
          if(multiplicities[i] %in% c("1", "s")){
            multiplicity_num <- 1

          } else if(multiplicities[i] %in% c("2", "d")){
            multiplicity_num <- 2

          } else if(multiplicities[i] %in% c("3", "t")){
            multiplicity_num <- 3

          } else if(multiplicities[i] %in% c("4", "q", "dd")){
            multiplicity_num <- 4

          } else if(!is.na(as.numeric(multiplicities[i]))){
            multiplicity_num <- as.numeric(multiplicities[i])

          } else{
            multiplicity_num <- 0

          }

          ind <- sort(abs(peaks - peaks_compare[i]), index.return = T)$ix[1:multiplicity_num]

          if (!is.na(mean(peaks[ind])) && mean(peaks[ind]) > FeaturesMatrix[i, 3] && mean(peaks[ind]) < FeaturesMatrix[i, 4]) {
            s0[which(seq_along(s0) %% 5 == 2)[i]] <- mean(peaks[ind])
            lb[which(seq_along(s0) %% 5 == 2)[i]] <- mean(peaks[ind]) - 0.001
            ub[which(seq_along(s0) %% 5 == 2)[i]] <- mean(peaks[ind]) + 0.001

          }
        }

        #Main optimization
        set.seed(iter);nls.out <- minpack.lm::nls.lm(par            = s0, # modified to allow for additional J coupling value and roof effect (for dd case)
                                                     fn             = residFun,
                                                     observed       = Ydata,
                                                     xx             = Xdata,
                                                     multiplicities = multiplicities,
                                                     roof_effect    = roof_effect,
                                                     roof_effect2   = roof_effect2,
                                                     freq           = program_parameters$freq,
                                                     lower          = lb,
                                                     upper          = ub,
                                                     control        = minpack.lm::nls.lm.control(factor  = program_parameters$factor,
                                                                                                 maxiter = program_parameters$nls_lm_maxiter,
                                                                                                 ftol    = program_parameters$ftol,
                                                                                                 ptol    = program_parameters$ptol
                                                                                                 )
                                                     )

        # #Procedure to calculate the fititng error in all the ROI
        #An adapted MSE error is calculated, and the parameters of the optimization with less MSE are stored
        iter   <- iter + 1
        # modified to account for additional J coupling value and roof effect (for dd case)
        order2 <- order(stats::coef(nls.out)[which(seq_along(stats::coef(nls.out)) %% 6 == 2)][signals_to_fit])

        errorprov <- (sqrt(nls.out$deviance / length(Ydata))) * 100 / (max(Ydata) - min(Ydata))
        if (is.nan(errorprov) || is.na(errorprov)){
          errorprov = error1
        }
        if (errorprov < error1 && identical(order1,order2)) {
          error1    <- errorprov
          paramprov <- stats::coef(nls.out)

        } else if (errorprov > worsterror) {
          worsterror <- errorprov
        }
      } else {
        #If in the first two iterations the procedure of finding peaks is not effective enough, the irignal chemical $chemical_shift and chemical $chemical_shift tolerance of every signal is maintained
        set.seed(iter)
        nls.out <- minpack.lm::nls.lm(par            = s0, # modified to allow for additional J coupling value and roof effect (for dd case)
                                      fn             = residFun,
                                      observed       = Ydata,
                                      xx             = Xdata,
                                      multiplicities = multiplicities,
                                      roof_effect    = roof_effect,
                                      roof_effect2   = roof_effect2,
                                      freq           = program_parameters$freq,
                                      lower          = lb,
                                      upper          = ub,
                                      control = minpack.lm::nls.lm.control(factor  = program_parameters$factor,
                                                                           maxiter = program_parameters$nls_lm_maxiter,
                                                                           ftol    = program_parameters$ftol,
                                                                           ptol    = program_parameters$ptol
                                                                           )
                                      )

        iter <- iter + 1

        # modified to account for second J coupling value
        order2 <- order(stats::coef(nls.out)[which(seq_along(stats::coef(nls.out)) %% 6 == 2)][signals_to_fit])

        # #Procedure to calculate the fititng error in all the ROI
        #An adapted MSE error is calculated, and the parameters of the optimization with less MSE are stored
        errorprov <- (sqrt(nls.out$deviance / length(Ydata))) * 100 / (max(Ydata) - min(Ydata))
        if (is.nan(errorprov) || is.na(errorprov)){
          errorprov <- error1
        }

        if (errorprov < error1 && identical(order1, order2)) {
          error1 <- errorprov
          paramprov <- stats::coef(nls.out)

        } else if (errorprov > worsterror) {
          worsterror <- errorprov

        }
      }}
    signals_parameters <- paramprov

    # modified to allow for second J coupling value
    # Note that it is baked into parS
    fitted_signals <- signal_fitting(parS           = signals_parameters,
                                     Xdata          = Xdata,
                                     multiplicities = multiplicities,
                                     roof_effect    = roof_effect,
                                     roof_effect2   = roof_effect2,
                                     freq           = program_parameters$freq)

    bins <- c()
    for (ind in signals_to_fit) {
      sorted_bins <- sort(fitted_signals[ind,] / sum(fitted_signals[ind, ]), decreasing = T, index.return = T)
      if(length(sorted_bins$x) > 0){
        bins <- sorted_bins$ix[1:which.min(abs(cumsum(sorted_bins$x) - 0.75))]
      }
    }
    if (length(bins) == 0){
      bins <- seq_along(Ydata)
    }

    #Correction of half_bandwidth and j-coupling
    iter      <- 0
    error22   <- error2 <- error1
    errorprov <- error1 <- 3000

    #Only half_bandwidth and j-coupling will have different lower und upper bounds.
    # modified to allow for J coupling 2 optimization
    change_indexes     <- which(seq_along(lb) %% 6 != 3 & seq_along(lb) %% 6 != 4 & seq_along(lb) %% 6 != 5 & seq_along(lb) %% 6 != 0)
    lb[change_indexes] <- ub[change_indexes] <- signals_parameters[change_indexes]
    while (iter < 3) {
      set.seed(iter)
      s0 <- lb + (ub - lb) * stats::runif(length(ub))
      nls.out <- minpack.lm::nls.lm(par            = s0, # modified to allow for additional J coupling value and roof effect (for dd case)
                                    fn             = residFun,
                                    observed       = Ydata,
                                    xx             = Xdata,
                                    multiplicities = multiplicities,
                                    roof_effect    = roof_effect,
                                    roof_effect2   = roof_effect2,
                                    freq           = program_parameters$freq,
                                    lower          = lb,
                                    upper          = ub,
                                    control        = minpack.lm::nls.lm.control(factor  = program_parameters$factor,
                                                                                maxiter = program_parameters$nls_lm_maxiter,
                                                                                ftol    = program_parameters$ftol,
                                                                                ptol    = program_parameters$ptol
                                                                                )
                                    )

      iter <- iter + 1
      # #Procedure to calculate the fititng error in all the ROI
      #An adapted MSE error is calculated, and the parameters of the optimization with less MSE are stored
      errorprov <- (sqrt(nls.out$deviance / length(Ydata))) * 100 / (max(Ydata) - min(Ydata))
      if (is.nan(errorprov) || is.na(errorprov)){
        errorprov <- error1
      }

      if (errorprov < error1) {
        error1    <- errorprov
        paramprov <- stats::coef(nls.out)

      } else if (errorprov > worsterror) {
        worsterror <- errorprov

      }
      if (error1 < error2) {
        error2             <- error1
        signals_parameters <- paramprov

      }
    }

    #If half_bandwidth and j-coup change improves fitting


    iterrep <- iterrep + 1

    # modified to adapt to multiplicity as character-valued
    multiplicity_total <- 0
    for(i in length(signals_to_fit)){
      if(multiplicities[i] %in% c("1", "s")){
        multiplicity_total <- multiplicity_total + 1

      } else if(multiplicities[i] %in% c("2", "d")){
        multiplicity_total <- multiplicity_total + 2

      } else if(multiplicities[i] %in% c("3", "t")){
        multiplicity_total <- multiplicity_total + 3

      } else if(multiplicities[i] %in% c("4", "q", "dd")){
        multiplicity_total <- multiplicity_total + 4

      } else if(!is.na(as.numeric(multiplicities[i]))){
        multiplicity_total <- multiplicity_total + as.numeric(multiplicities[i])

      }
    }

    #If the fitting seems to be still clearly improvable through the addition of signals
    if (iterrep <= fitting_maxiterrep & error22 < (program_parameters$additional_signal_improvement * dummy) &
        (error22 > program_parameters$additional_signal_percentage_limit) & length(peaks_xdata$maxtab$pos) > multiplicity_total) {
      # print('Trying to improve initial fit adding peaks')

      #I find peaks on the residuals
      residual_peaks <- tryCatch(peakdet(v     = c(nls.out$fvec[1], diff(nls.out$fvec)),
                                         delta = program_parameters$peakdet_minimum * max(1e-10, max(Ydata))),
                                 error = function(e){
                                   dummy <- list(signals_parameters = signals_parameters,
                                                 error1             = error1)
                                   return(dummy)
                                   }
                                 )

      if (is.null(residual_peaks$maxtab) == F) {
        #Preparation of information of where signals of interest are located

        # modified to adapt to multiplicity as character-valued
        multiplicity_num <- vector("numeric", length = length(multiplicities))
        for(i in 1:length(multiplicity_num)){
          if(multiplicities[i] %in% c("1", "s")){
            multiplicity_num[i] <- 1

          } else if(multiplicities[i] %in% c("2", "d")){
            multiplicity_num[i] <- 2

          } else if(multiplicities[i] %in% c("3", "t")){
            multiplicity_num[i] <- 3

          } else if(multiplicities[i] %in% c("4", "q", "dd")){
            multiplicity_num[i] <- 4

          } else if(!is.na(as.numeric(multiplicities[i]))){
            multiplicity_num[i] <- as.numeric(multiplicities[i])

          } else {
            multiplicity_num[i] <- 0

          }
        }

        # modified to adapt to multiplicity as character-valued
        dummy             <- multiplicity_num[signals_to_fit] %% 2
        dummy[dummy == 0] <- 2
        # modified to account for j coupling 2
        additional_signal_matrix <- matrix(paramprov, nrow(FeaturesMatrix), 6, byrow = TRUE)
        points_to_avoid          <- abs(rbind(matrix(Xdata, length(signals_to_fit), length(Xdata), byrow = TRUE) -
                                                matrix(additional_signal_matrix[signals_to_fit, 2] - (additional_signal_matrix[signals_to_fit, 5] / dummy) / program_parameters$freq,
                                                       length(signals_to_fit), length(Xdata)),
                                              matrix(Xdata, length(signals_to_fit), length(Xdata), byrow = TRUE) -
                                                matrix(additional_signal_matrix[signals_to_fit, 2] + (additional_signal_matrix[signals_to_fit, 5] / dummy) / program_parameters$freq, length(signals_to_fit), length(Xdata))))
        points_to_avoid          <- apply(points_to_avoid, 1, which.min)
        seq_range                <- c()
        for (i in (-range_ind):range_ind){
          seq_range <- append(seq_range, points_to_avoid - i)
        }

        #Finding of posible additional signals to incorporate if there are not in zones where the signals o interest are located
        residual_peaks       <- cbind(residual_peaks$maxtab$pos, residual_peaks$maxtab$val)[residual_peaks$maxtab$pos %in% peaks_bindata$maxtab$pos,, drop=F]
        valid_residual_peaks <- matrix(NA, 0, 2)
        if (nrow(residual_peaks) > 0) {
          for (i in seq(nrow(residual_peaks))) {
            if (any(abs(points_to_avoid - residual_peaks[i, 1]) < range_ind) == F){
              valid_residual_peaks <- rbind(valid_residual_peaks, residual_peaks[i, ])
            }
          }
          #Selection of more intense additional signals
          if (nrow(valid_residual_peaks) > program_parameters$signals_to_add) {
            ad                   <- sort(valid_residual_peaks[, 2], decreasing = T, index.return = T)$ix
            valid_residual_peaks <- valid_residual_peaks[ad[1:min(program_parameters$signals_to_add, length(ad))], , drop = F]
          }

          #Creation of rows to incorporate to FeaturesMatrix
          if (nrow(valid_residual_peaks) > 0) {
            dummy                        <- t(replicate(nrow(valid_residual_peaks), FeaturesMatrix[1,]))
            dummy[, 2]                   <- Ydata[valid_residual_peaks[, 1]]
            dummy[, 3]                   <- Xdata[valid_residual_peaks[, 1]] - 0.001
            dummy[, 4]                   <- Xdata[valid_residual_peaks[, 1]] + 0.001
            dummy[, 5]                   <- min(FeaturesMatrix[, 5])
            dummy[, 6]                   <- min(FeaturesMatrix[, 6])
            dummy[, c(9,10,12,13,14,15)] <- rep(0, nrow(valid_residual_peaks))
            dummy[, 11]                  <- rep("1", nrow(valid_residual_peaks))
            FeaturesMatrix               <- rbind(FeaturesMatrix, dummy)

          } else {
            iterrep <- fitting_maxiterrep + 1

          }
        } else  {
          iterrep <- fitting_maxiterrep + 1

        }}
    } else {
      iterrep <- fitting_maxiterrep + 1

    }

  }
  optim_parameters <- list(signals_parameters = signals_parameters,
                           error1             = error1)
  return(optim_parameters)
}
