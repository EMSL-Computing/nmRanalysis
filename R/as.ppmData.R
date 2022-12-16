#' Convert Data to Appropriate NMR Class
#'
#' @description Converts a list object or several data.frames of ppm-level data to an object of the class 'ppmData'. Objects of the class 'ppmData' are lists with two obligatory components \code{e_data} and \code{f_data}.
#'
#'  Copyright (C) 2022 Battelle Memorial Institute
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
#' @param e_data a \eqn{p \times n + 1} data.frame of expression data, where \eqn{p} is the number of ppm bins observed and \eqn{n} is the number of samples (an additional identifier/name column should also be present anywhere in the data.frame). Each row corresponds to data for each ppm. One column specifying a unique identifier for each row must be present.
#' @param f_data a data.frame with \eqn{n} rows. Each row corresponds to a sample with one column giving the unique sample identifiers found in e_data column names and other columns providing qualitative and/or quantitative traits of each sample.
#'
#' @param edata_cname character string specifying the name of the column containing the ppm identifiers in \code{e_data} and \code{e_meta} (if applicable).
#' @param fdata_cname character string specifying the name of the column containing the sample identifiers in \code{f_data}.
#'
#' @param align logical, defaults to FALSE. If TRUE, aligns e_data using the CluPA spectrum alignment algorithm from the `speaq` R package
#'
#' @param instrument_strength numeric value specifying the strength (in MHz) of the NMR instrument samples were run on.
#' @param ph numeric value specifying the pH samples were run at.
#' @param solvent character string defining solvent used. Possible solvents are 'h2o' and 'd2o'.
#' @param temperature numeric value specifying the temperature (in K) of the experiment
#' @param concentration numeric value specifying the concentration (in mM) of the standard (e.g. DSS) used in the experiment
#'
#' @param ... further arguments
#'
#' @details Objects of class 'ppmData' contain some attributes that are referenced by downstream functions. These attributes can be changed from their default value by manual specification. A list of these attributes as well as their default values are as follows:
#' \tabular{ll}{
#' check.names \tab Logical defaults to TRUE. Indicates whether 'check.names' attribute of returned omicsData object is TRUE or FALSE. \cr
#' }
#' Computed values included in the \code{data_info} attribute are as follows:
#' \tabular{ll}{
#' num_miss_obs \tab The number of missing observations.\cr
#' \tab \cr
#' prop_missing \tab The proportion of \code{e_data} values that are NA. \cr
#' \tab \cr
#' num_samps \tab The number of samples that make up the columns of \code{e_data}.\cr
#' \tab \cr
#' }
#'
#' @author Allison Thompson
#'
#' @export
as.ppmData <- function(e_data, f_data, edata_cname, fdata_cname, align = FALSE, instrument_strength, ph = NULL, solvent, temperature, concentration = NULL, ...){
  .as.ppmData(e_data, f_data, edata_cname, fdata_cname, align, instrument_strength, ph = ph, solvent, temperature, concentration = concentration, ...)
}

## ppm data ##
.as.ppmData <- function(e_data, f_data, edata_cname, fdata_cname, align,
                        instrument_strength, ph = NULL, solvent, temperature, concentration = NULL,
                        check.names = TRUE){

  # initial checks #

  # check that e_data and f_data are data.frames #
  if(!inherits(e_data, "data.frame")){
    stop("e_data must be of the class 'data.frame'")
  }

  if(!inherits(f_data, "data.frame")){
    stop("f_data must be of the class 'data.frame'")
  }

  # check that the ppm column exists in e_data and e_meta (if applicable) #
  if(!(edata_cname %in% names(e_data))){
    stop(paste("ppm column ", edata_cname," not found in e_data. See details of as.ppmData for specifying column names.", sep = ""))
  }

  # check that the Sample column name is in f_data column names #
  if(!(fdata_cname %in% names(f_data))){
    stop(paste("Sample column ", fdata_cname, " not found in f_data. See details of as.ppmData for specifying column names.", sep = ""))
  }

  # check that all samples in e_data are present in f_data #
  edat_sampid <- which(names(e_data) == edata_cname)
  samps.miss  <- sum(!(names(e_data[,-edat_sampid]) %in% f_data[,fdata_cname]))
  if( samps.miss > 0){
    stop(paste( samps.miss, " samples from e_data not found in f_data", sep = ""))
  }

  # check for any extra samples in f_data than in e_data #
  if(any(!(f_data[,fdata_cname] %in% names(e_data)))){
    f_data <- f_data[-which(!(f_data[,fdata_cname] %in% names(e_data))),]
    warning("Extra samples were found in f_data that were not in e_data. These have been removed from f_data.")
  }

  # check that f_data has at least 2 columns #
  if(ncol(f_data) < 2){
    stop("f_data must contain at least 2 columns")
  }

  # check that e_data has unique rows #
  # if(nrow(e_data) == length(unique(e_data[, edata_cname]))){
  #   e_data <- e_data
  # }else{
  #   e_data_unique <- unique(e_data)
  #   if(nrow(e_data_unique) == length(unique(e_data_unique[, edata_cname]))){
  #     e_data <- e_data_unique
  #   }else{
  #     stop("The 'edata_cname' identifier is non-unique.")
  #   }
  # }

  # check that align is logical #
  if(class(align) != "logical"){
    stop("align must be either TRUE or FALSE")
  }

  # check that instrument strength is numeric #
  if(class(instrument_strength) != "numeric"){
    stop("instrument_strength must be a numeric value of the MHz strength of the instrument")
  }

  # check that temperature is numeric #
  if(class(temperature) != "numeric"){
    stop("temperature must be a numeric value indicating the temperature of the sample")
  }

  if(!is.null(ph)){
    # check that ph is numeric #
    if(class(ph) != "numeric"){
      stop("ph must be a numeric value indicating the pH of the sample")
    }
  }

  if(!is.null(concentration)){
    # check that concentration is numeric #
    if(class(concentration) != "numeric"){
      stop("concentration must be a numeric value indicating the sample concentration")
    }
  }

  # check that solvent is appropriate #
  if(!(solvent %in% c("h2o","d2o"))){
    stop("Solvent must be one of 'h2o' or 'd2o'")
  }

  if(align == TRUE){
    e_data <- peak_alignment(e_data = e_data)
  }

  # store results #
  res <- list(e_data = e_data, f_data = f_data)

  # set column name attributes #
  attr(res, "cnames") <- list(edata_cname = edata_cname, fdata_cname = fdata_cname)

  # count missing values in e_data #
  num_miss_obs <- sum(is.na(e_data[,-which(names(e_data) == edata_cname)]))
  prop_missing <- mean(is.na(e_data[,-which(names(e_data) == edata_cname)]))

  # number of unique ppms #
  num_edata <- length(unique(e_data[, edata_cname]))

  # number of samples #
  num_samps <- ncol(e_data) - 1

  # set data information attributes #
  attr(res, "data_info") <- list(num_edata = num_edata, num_miss_obs = num_miss_obs, prop_missing = prop_missing, num_samps = num_samps)

  # set aligned attribute #
  if (align == TRUE){
    attr(res, "aligned") <- TRUE
  }

  # set sample set information attributes #
  attr(res, "exp_info") <- list(instrument_strength = instrument_strength,
                                ph = ph,
                                solvent = solvent,
                                temperature = temperature,
                                concentration = concentration)

  #set check.names attribute #
  attr(res, "check.names") <- check.names

  # set group dataframe attribute to NULL, will be filled in after running group_designation function #
  attr(res, "group_DF") <- NULL

  # set filters attributes #
  if(!is.null(attr(res, "filters"))){
    attr(res, "filters") <- attr(res, "filters")
  } else{
    attr(res, "filters") <- list()
  }

  # set class of list #
  class(res) <- "ppmData"

  return(res)
}

#' Spectrum Alignment Pre-processing
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
#' @param e_data a data.frame of expression data, first component of a ppmData object
#' @param max_shift integer value, optional. Defaults to NULL. If NULL, automatically detects optimal maxShift value.
#' @description This function takes the data.frame `e_data` and runs the CluPA spectrum alignment algorithm from the `speaq` R package
#' @return a data.frame containing aligned spectra with same format as `e_data`
#' @author Natalie Winans
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
peak_alignment <- function(e_data, max_shift = NULL) {
  data <- as.matrix(t(e_data[,-1]))
  label <- colnames(e_data)[-1]

  X <- list(data = data, label = label)

  # Peak detection - uses MassSpecWavelet package
  peak_list <- speaq::detectSpecPeaks(X$data, verbose = FALSE)

  # Find reference spectrum
  ref_index <- speaq::findRef(peak_list)$refInd

  # Align using CluPA algorithm
  Y <- speaq::dohCluster(X = X$data,
                         peakList = peak_list,
                         refInd = ref_index,
                         maxShift = max_shift,
                         verbose = FALSE)

  # Format output
  res <- as.data.frame(Y) %>%
    `rownames<-`(X$label) %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-.data$rowname) %>%
    tidyr::pivot_wider(names_from = .data$rowname, values_from = .data$value) %>%
    dplyr::mutate(PPM = e_data$PPM) %>%
    dplyr::select(-.data$name) %>%
    dplyr::select(.data$PPM, tidyselect::everything())

  return(res)
}
