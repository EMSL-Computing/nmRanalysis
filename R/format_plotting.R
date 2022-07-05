#' Format results for plotting
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
#' @param profiling_data Resultant object from \code{\link{run_rDolphin}}
#' @param signals_to_plot Vector of indexes of signals in ROI data to plot. By default, NA and all figures are outputted.
#'
#' @return Data object for Trelliscope Plotting
#'
#' @importFrom magrittr %>%
#'
#' @author Allison Thompson
#'
#' @export
format_plotting <- function(profiling_data, signals_to_plot = NULL) {
  # create plots directory
  #dir.create(file.path(export_path,'plots'))

  # plot all metabolites where there's at least one non-missing value
  if (is.null(signals_to_plot)){
    signals_to_plot <- which(apply(profiling_data$final_output$quantification, 2, function(x){all(is.na(x))}) == F)
  }

  # list of number of samples
  p <- vector(mode = "list", length = nrow(profiling_data$final_output$quantification))

  # progress bar
  if (max(signals_to_plot) > 1){
    pb <- utils::txtProgressBar(1, max(signals_to_plot), style=3)
  }

  plotdataall.out <- list()

  # loops for plotting
  # for each metabolite
  for (ind2 in signals_to_plot) {

    plotdataall.in <- list()

    # for each sample
    for (ind in 1:nrow(profiling_data$final_output$quantification)) {

      # x-axis data
      Xdata <- profiling_data$reproducibility_data[[ind]][[ind2]]$Xdata

      # if no data, move on
      if(is.null(Xdata)){
        next
      }

      # y-axis data
      Ydata <- profiling_data$reproducibility_data[[ind]][[ind2]]$Ydata

      # plot data
      plot_data <- profiling_data$reproducibility_data[[ind]][[ind2]]$plot_data

      # ROI profile
      ROI_profile <- profiling_data$reproducibility_data[[ind]][[ind2]]$ROI_profile

      # combine
      plotdata2 <- data.frame(Xdata        = Xdata,
                              Ydata        = Ydata,
                              fitted_sum   = plot_data[3, ] , # fitted_sum
                              baseline_sum = plot_data[2, ]) # baseline_sum

      # make long
      plotdata3 <- reshape2::melt(plotdata2, id = "Xdata")

      # specify variables
      plotdata3$variable <- c(
        rep('Original Spectrum', length(Ydata)),
        rep('Generated Spectrum', length(Ydata)),
        rep('Generated Background', length(Ydata))
      )

      plotdata4 <- data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F])))

      colnames(plotdata4) <- c('Xdata', rownames(plot_data)[-c(1, 2, 3)])

      plotdata5 <- reshape2::melt(plotdata4, id = "Xdata")

      r <- which(make.names(paste(ROI_profile[,4],ROI_profile[,5],sep='_')) == colnames(profiling_data$final_output$quantification)[ind2])
      if(length(r) == 0){
        next
      }
      plotdata <- data.frame(Xdata, signals = plot_data[3 + r,] )

      # format plotdata3
      plotdata3$variable2 <- plotdata3$variable

      # format plotdata5
      plotdata5$variable2 <- plotdata5$variable
      plotdata5$variable  <- "Surrounding Signals"

      # format plotdata
      colnames(plotdata)[which(colnames(plotdata) == "signals")] <- "value"
      plotdata$variable  <- "Quantified Signal"
      plotdata$variable2 <- "Quantified Signal"

      # combine
      temp                  <- rbind(rbind(plotdata, plotdata5), plotdata3)
      temp$Sample           <- rownames(profiling_data$final_output$quantification)[ind]
      plotdataall.in[[ind]] <- temp

    }

    temp2                   <- do.call(rbind, plotdataall.in)
    temp2$Metabolite        <- names(signals_to_plot)[ind2]
    plotdataall.out[[ind2]] <- temp2
  }

  plotdataall <- do.call(rbind, plotdataall.out)

  # Add Quantification, Signal to Area Ratio, Fitting Error, Chemical Shift, Intensity, Half Bandwidth
  plotdataall2 <- plotdataall %>% dplyr::full_join(
    data.frame(Sample = rownames(profiling_data$final_output$quantification), profiling_data$final_output$quantification) %>%
    tidyr::pivot_longer(-.data$Sample, names_to = "Metabolite", values_to = "Quantification"),
    by = c("Sample", "Metabolite")) %>%
    dplyr::full_join(data.frame(Sample = rownames(profiling_data$final_output$signal_area_ratio), profiling_data$final_output$signal_area_ratio) %>%
      tidyr::pivot_longer(-.data$Sample, names_to = "Metabolite", values_to = "Signal_Area_Ratio"), by=c("Sample", "Metabolite")) %>%
    dplyr::full_join(data.frame(Sample = rownames(profiling_data$final_output$fitting_error), profiling_data$final_output$fitting_error) %>%
      tidyr::pivot_longer(-.data$Sample, names_to = "Metabolite", values_to = "Fitting_Error"), by=c("Sample", "Metabolite")) %>%
    dplyr::full_join(data.frame(Sample = rownames(profiling_data$final_output$chemical_shift), profiling_data$final_output$chemical_shift) %>%
      tidyr::pivot_longer(-.data$Sample, names_to = "Metabolite", values_to = "Chemical_Shift"), by=c("Sample", "Metabolite")) %>%
    dplyr::full_join(data.frame(Sample = rownames(profiling_data$final_output$intensity), profiling_data$final_output$intensity) %>%
      tidyr::pivot_longer(-.data$Sample, names_to = "Metabolite", values_to = "Intensity"), by=c("Sample", "Metabolite")) %>%
    dplyr::full_join(data.frame(Sample = rownames(profiling_data$final_output$half_bandwidth), profiling_data$final_output$half_bandwidth) %>%
      tidyr::pivot_longer(-.data$Sample, names_to = "Metabolite", values_to = "Half_Bandwidth"), by=c("Sample", "Metabolite"))

  return(plotdataall2)
}
