#' Convert Data to rDolphin input type
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
#' Converts a 'ppmData' object into input files for rDolphin
#'
#' @param ppmData a \code{ppmData} object
#' @param metabs metabolite information, from \code{get_reference_data} for example
#' @param normalization Numeric indicating which normalization to perform - 0 = No normalization; 1 = Eretic; 2 = TSP; 3 = Creatinine; 4 = Spectra Sum; 5 = PQN. Default is 0.
#' @param alignment Numeric indicating which alignment to perform - 0 = No alignment; 1 = Glucose; 2 = TSP; 3 = Formate. Default is 0.
#' @param nmr_folder_path Folder where all sample NMR folders are stored, if spectra are generated from Bruker files; default is "", meaning nothing is imported.
#' @param data_index_1d Name of 1D data folder inside each sample NMR folder (usually along the lines of 11, 12, 13...), if spectra are generated from Bruker files. Default is "", meaning nothing is imported.
#' @param proc_no Name of the proc_no folder inside each 1D data folder (usually this is 1), if spectra are generated from Bruker files. Default is "", meaning nothing is imported.
#' @param default_suppressions Spectrum regions suppressed, with the format 9.5-6.1;5.1-4.5. Default is "", no suppressions.
#' @param bucket_resolution PPM for bucket resolutions; default is ""
#' @param specific_params Path of file to change rDolphin internal parameters if desired
#'
#' @details Objects of class 'ppmData' contain some attributes that are referenced by downstream functions, namely rDolphin. These attributes can be changed from their default value by manual specification. A list of these attributes as well as their default values are as follows:
#' \tabular{ll}{
#' nmr_folder_path \tab Folder where all sample NMR folders are stored, if spectra are generated from Bruker files; default is "", meaning nothing is imported. \cr
#' \tab \cr
#' data_index_1d \tab Name of 1D data folder inside each sample NMR folder (usually along the lines of 11, 12, 13...), if spectra are generated from Bruker files. Default is "", meaning nothing is imported. \cr
#' \tab \cr
#' proc_no \tab Name of the proc_no folder inside each 1D data folder (usually this is 1), if spectra are generated from Bruker files. Default is "", meaning nothing is imported. \cr
#' \tab \cr
#' normalization \tab Numeric indicating which normalization to perform - 0 = No normalization; 1 = Eretic; 2 = TSP; 3 = Creatinine; 4 = Spectra Sum; 5 = PQN. Default is 0. \cr
#' \tab \cr
#' alignment \tab Numeric indicating which alignment to perform - 0 = No alignment; 1 = Glucose; 2 = TSP; 3 = Formate. Default is 0. \cr
#' \tab \cr
#' default_suppressions \tab Spectrum regions suppressed, with the format 9.5-6.1;5.1-4.5. Default is "", no suppressions. \cr
#' \tab \cr
#' bucket_resolution \tab PPM for bucket resolutions; default is "". \cr
#' \tab \cr
#' specific_params \tab Path of file to change rDolphin internal parameters if desired. \cr
#' \tab \cr
#' }
#'
#' @return Returns an rDolphin data object
#'
#' @author Allison Thompson
#'
#' @importFrom utils write.csv
#' @export
ppmData_to_rDolphin <- function(ppmData, metabs,
                                normalization        = 0,
                                alignment            = 0,
                                nmr_folder_path      = "",
                                data_index_1d        = "",
                                proc_no              = "",
                                default_suppressions = "",
                                bucket_resolution    = "",
                                specific_params      = ""){


  # initial checks #

  # check that normalization is 0-5
  if(!(normalization %in% 0:5 | normalization %in% c("0","1","2","3","4","5"))){
    stop("Normalization must be one of 0 (No normalization), 1 (Eretic), 2 (TSP), 3  (Creatinine), 4 (Spectra Sum), or 5 (PQN).")
  }

  # check that alignment is 0-3
  if(!(alignment %in% 0:3 | alignment %in% c("0","1","2","3"))){
    stop("Alignment must be one of 0 (No alignment), 1 (Glucose), 2 (TSP), or 3 (Formate).")
  }

  # Note that much of the code here is redudant in the sense that dataframes are converted to readable files, and then
  # corresponding file paths are noted and fed to rDolphin::import_data(), which then reads those files to obtain the same data
  # used to generate them in the first place. In response to this, the following if/else statement is implemented. If all default
  # arguments are used, then blocks of code pulled from rDolphin::import_data() are used to generate the same end object, but without
  # the unnecessary steps of converting data to files and then reading in the data in those created files. If non-default arguments
  # are specified, then the function proceeds as originally coded. It may be that we can avoid the data -> file -> data inefficiency
  # altogether if we later find a way of accounting for nondefault arguments in the extracted blocks.
  if(normalization == 0 & alignment == 0 & nmr_folder_path == "" & data_index_1d == "" &
     proc_no == "" & default_suppressions == "" & bucket_resolution == "" & specific_params == ""){

    # List of parameters to use to create the dataset
    params <- list()

    # Read in metadata and extract sample (i.e. experiment) name
    Experiments <- ppmData$f_data$Sample
    Metadata    <- ppmData$f_data

    # Read in ROI data
    ROI_data <- data.frame(metabs)
    rownames(ROI_data) <- 1:nrow(ROI_data)

    # Extract names of ROIs
    signals_names <- make.names(paste(ROI_data[which(!is.na(ROI_data[, 1])),4],
                                      ROI_data[which(!is.na(ROI_data[, 1])),5], sep='_'))

    freq      <- attr(ppmData, "exp_info")$instrument_strength # this is the instrument strength
    biofluid  <- attr(ppmData, "exp_info")$solvent # this is the solvent
    jres_path <- ""

    # Extract all of the default global fitting parameters from rDolphin
    prog_par_path <- file.path(system.file(package = "rDolphin"),"extdata","fitting_variables.csv")

    temp <- read.csv(prog_par_path, stringsAsFactors = F)
    program_parameters <- as.list(suppressWarnings(as.numeric(temp$Value)))
    names(program_parameters) <- temp$Varaible
    program_parameters$automatic_removal <- "Y" # Default
    program_parameters$fitting_maxiter <- as.character(program_parameters$fitting_maxiter) # Default (yes I know this is strange)
    rm(temp)

    params$left_spectral_edge  <- program_parameters$left_spectral_edge
    params$right_spectral_edge <- program_parameters$right_spectral_edge


    #Creation of repository adapted to biofluid
    repository <- data.frame(data.table::fread(file.path(system.file(package = "rDolphin"),"extdata","HMDB_Repository.csv")))
    biofluid_column <- which(gsub('.times', '', colnames(repository)) == biofluid)
    if (length(biofluid_column) == 0) {
      times <- rowSums(repository[,25:36])
      repository <- cbind(repository[order(times, decreasing = T), c(1:3, 5:7)],
                          rep(NA, length(times)),
                          times[order(times, decreasing = T)])
      colnames(repository)[c(7,8)] <- c('Conc', 'Times')
    } else {
      repository <- repository[repository[, biofluid_column] != 0,]
      repository <- repository[order(repository[, biofluid_column], decreasing = T), c(1:3, 5:7, biofluid_column + c(-12,0))]
    }

    # Normalization (Assumes the default of 0)
    normalization <- 0
    pqn <- 'N'

    params$norm_AREA <- 'N'
    params$norm_PEAK <- 'N'
    params$norm_left_ppm <- params$left_spectral_edge
    params$norm_right_ppm <- params$right_spectral_edge

    # Alignment (Assumes the default of 0)
    alignment = 0
    params$glucose_alignment = 'N'
    params$tsp_alignment = 'N'
    params$peak_alignment = 'N'
    params$ref_peak_pos = 8.452

    # Suppresion regions (Assumes the default of "")
    suppression <- ""
    if (suppression == '') {
      params$disol_suppression = 'N' # Important - Uses default
    }

    imported_data <- list()
    imported_data$dataset <- t(ppmData$e_data[,-1, drop = FALSE])
    imported_data$dataset[is.na(imported_data$dataset)] <- 0 # sets NA values to 0
    imported_data$ppm <- round(ppmData$e_data$PPM, 4)
    names(imported_data$ppm) <- paste0("V", 1:length(imported_data$ppm))
    # Checks if ppm values are in ascending order. If so, change to descending
    if (imported_data$ppm[1] < imported_data$ppm[2]) {
      imported_data$dataset <- t(apply(imported_data$dataset, 1, rev))
      imported_data$ppm     <- rev(imported_data$ppm)
    }
    colnames(imported_data$dataset) <- imported_data$ppm
    rownames(imported_data$dataset) <- Experiments

    # Assumes default
    params$buck_step = as.numeric(abs(imported_data$ppm[1] - imported_data$ppm[length(imported_data$ppm)]) / length(imported_data$ppm))

    # Assumes Default of 1
    norm_factor <- rep(1, nrow(imported_data$dataset))

    imported_data$dataset <- imported_data$dataset/norm_factor

    #Storage of parameters needed to perform the fit in a single variable to return.

    imported_data$buck_step          <- params$buck_step
    imported_data$Experiments        <- Experiments
    imported_data$ROI_data           <- ROI_data
    imported_data$freq               <- freq
    imported_data$Metadata           <- Metadata
    imported_data$repository         <- repository
    imported_data$jres_path          <- jres_path
    imported_data$program_parameters <- program_parameters
    imported_data$norm_factor        <- norm_factor


    #creation of list with the different final outputs
    dummy <- matrix(NaN, nrow(imported_data$dataset),length(signals_names),
                    dimnames = list(imported_data$Experiments, signals_names))
    imported_data$final_output <- list(quantification    = dummy,
                                        signal_area_ratio = dummy,
                                        fitting_error     = dummy,
                                        chemical_shift    = dummy,
                                        intensity         = dummy,
                                        half_bandwidth    = dummy)

    #creation of list of necessary parameters to load quantifications and evaluate quality of them
    imported_data$reproducibility_data <- vector('list', length(imported_data$Experiments))
    for (i in seq_along(imported_data$reproducibility_data)){
      imported_data$reproducibility_data[[i]] <- vector('list', length(signals_names))
    }
    for (i in seq_along(imported_data$reproducibility_data)) {
      for (j in seq_along(imported_data$reproducibility_data[[i]])) {
        imported_data$reproducibility_data[[i]][[j]] <- list(Ydata              = NULL,
                                                              Xdata              = NULL,
                                                              ROI_profile        = imported_data$ROI_data[j,],
                                                              program_parameters = NULL,
                                                              plot_data          = NULL,
                                                              FeaturesMatrix     = NULL,
                                                              signals_parameters = NULL,
                                                              results_to_save    = NULL,
                                                              error1             = 1000000)
      }}

  } else{

    # create files #

    # temporary directory
    dir <- tempdir()
    dir <- gsub("\\\\", "/", dir)

    # dataset file #
    dataset           <- ppmData$e_data
    #rownames(dataset) <- dataset[,attr(ppmData, "cnames")$edata_cname]
    dataset           <- dataset[,-which(colnames(dataset) == attr(ppmData, "cnames")$edata_cname)]
    dataset           <- t(dataset)
    colnames(dataset) <- ppmData$e_data[,which(colnames(ppmData$e_data) == attr(ppmData, "cnames")$edata_cname)]
    write.csv(dataset, file=paste(dir, "/Dataset_", Sys.Date(), ".csv", sep=""), row.names = FALSE)

    # metadata file
    metadata           <- ppmData$f_data
    rownames(metadata) <- metadata[,which(colnames(metadata) == attr(ppmData, "cnames")$fdata_cname)]
    write.csv(metadata, file=paste(dir, "/Metadata_", Sys.Date(), ".csv", sep=""))

    # ROI patterns file
    write.csv(metabs, file=paste(dir, "/ROI_profiles_", Sys.Date(), ".csv", sep=""), row.names = FALSE)

    # parameters file
    params <- list(`nmr folder path`                                                          = nmr_folder_path,
                   `1D data index`                                                            = data_index_1d,
                   `proc_no`                                                                  = proc_no,
                   `spectra dataset path`                                                     = paste(dir, "/Dataset_", Sys.Date(), ".csv", sep=""),
                   `Metadata path (csv format)`                                               = paste(dir, "/Metadata_", Sys.Date(), ".csv", sep=""),
                   `ROI patterns file`                                                        = paste(dir, "/ROI_profiles_", Sys.Date(), ".csv", sep=""),
                   `Normalization (0=No;1=Eretic; 2=TSP; 3=Creatinine; 4=Spectra Sum; 5=PQN)` = normalization,
                   `Alignment (0=No;1=Glucose; 2=TSP; 3=Formate)`                             = alignment,
                   `Default Suppressions`                                                     = default_suppressions,
                   `Spectometer Frequency (MHz)`                                              = attr(ppmData, "exp_info")$instrument_strength,
                   `Bucket resolution`                                                        = bucket_resolution,
                   `Biofluid`                                                                 = attr(ppmData, "exp_info")$solvent,
                   #`2D-Path`                                                                 = data_path_2d,
                   `Specific dataset parameters`                                              = specific_params)

    params <- data.frame(Parameter = names(params),
                         Value     = unlist(params))
    write.csv(params, file = paste(dir, "/Parameters_", Sys.Date(), ".csv", sep=""), row.names = FALSE)

    # attr(ppmData, "rdolphin_files") <- list(Parameters = paste(dir, "Parameters_", Sys.Date(), ".csv", sep=""),
    #                                         Dataset = paste(dir, "Dataset_", Sys.Date(), ".csv", sep=""),
    #                                         Metadata = paste(dir, "Dataset_", Sys.Date(), ".csv", sep=""),
    #                                         ROI_profiles = paste(dir, "ROI_profiles_", Sys.Date(), ".csv", sep=""),
    #                                         dir = dir)

    # import data
    imported_data <- rDolphin::import_data(paste(dir, "/Parameters_", Sys.Date(), ".csv", sep=""))

  }

  class(imported_data) <- c("rDolphin", class(imported_data))

  return(imported_data)


}
