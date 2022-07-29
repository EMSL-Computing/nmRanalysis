#' Verifying an entry exists for a given CAS number and solvent type
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
#'@param casno_list list of CAS registry numbers written as characters
#'@param return_metabs string, must be one of "exact_match" or "all". Defaults to "all". If "exact_match", returns metabolites that exactly match specified experimental conditions (solvent type, pH, and instrument strength). If "all", returns all entries corresponding to supplied CAS numbers, experimental conditions ignored.
#'@param solvent_type choose from available solvents 'D2O', 'H2O', ...
#'@param ph numeric value specifying pH of experimental conditions
#'@param instrument_strength numeric value specifying experimental instrument strength
#'@param temperature numeric value specifying the temperature
#'@param concentration numeric value specifying the concentration
#'
#'@return a list of BMSE database entry numbers
#'
#'@importFrom utils data
#'
#'@export as.bmseList
#'
#'@examples
#'# test <- as.bmseList(list("56-41-7","2613-02-7"), solvent_type = 'D2O', ph = 7.4)
#'
as.bmseList <- function(casno_list,
                        return_metabs = "all",
                        solvent_type = NULL,
                        ph = NULL,
                        instrument_strength = NULL,
                        temperature         = NULL,
                        concentration       = NULL){

  # Initial Checks
  if(!inherits(casno_list, "list")){
    stop("List of CAS numbers must be of the class 'list'.")
  }

  if(!(return_metabs %in% c("exact_match", "all"))){
    stop('return_metabs must be one of "exact_match" or "all"')
  }

  if(return_metabs == "exact_match" & (is.null(solvent_type) | is.null(ph) | is.null(instrument_strength))){
    stop('Solvent type, ph, and spectrometer frequency must be specified if return_metabs = "exact_match"')
  }

  # if(return_metabs == "nearest_match" & (is.null(solvent_type) | is.null(ph))){
  #   stop('Solvent type and ph must be specified if return_metabs = "nearest_match"')
  # }

  # warn if one casno isn't in the db
  for (item in casno_list){
    if (!(item %in% bmse_associations$CASno)){
      warning(sprintf("%s is not a recognized CAS registry number", item))
    }
  }

  misscheck <- vector("numeric", length = length(casno_list))
  if (return_metabs == "all") {
    #create list without filtering by exp conditions
    bmse_list <- list()
    for (i in 1:length(casno_list)){
      subset    <- bmse_associations[bmse_associations$CASno == casno_list[[i]], ]
      bmse_val  <- subset$Entry_ID
      bmse_list <- append(bmse_list, bmse_val)
      misscheck[i] <- ifelse(length(bmse_val) > 0, 0, 1)
    }

  } else if (return_metabs == "exact_match") {
    # The code chunk below was commented out because it forced all solvent types to D20.
    # #check solvent types, convert to acceptable types
    # if(solvent_type != "D2O"){
    #   solvent_type <- "D2O"
    # }

    #create list filtered by solvent type and pH
    bmse_list <- list()
    for (i in 1:length(casno_list)){
      subset <- bmse_associations %>% dplyr::filter(.data$CASno == casno_list[[i]],
                                                    .data$Solvent == solvent_type,
                                                    .data$pH == ph)

      if(!is.na(temperature)){
        subset <- subset %>% dplyr::filter(.data$Temperature == temperature)
      }

      if(!is.na(concentration)){
        subset <- subset %>% dplyr::filter(.data$Concentration == concentration)
      }

      bmse_val  <- subset$Entry_ID
      bmse_list <- append(bmse_list, bmse_val)
    }
  }

  if (sum(misscheck) > 0 & return_metabs == "all"){
    message("Data not available for all provided CAS numbers.")
  }

  if (sum(misscheck) > 0 & return_metabs == "exact_match"){
    message("Data not available for all provided CAS numbers at the specified conditions.")
  }

  return(bmse_list)
}


#' Verifying an entry exists for a given metabolite name
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
#'@param name_list list of metabolite names
#'@param return_metabs string, must be one of "exact_match" or "all". Defaults to "all". If "exact_match", returns metabolites that exactly match specified experimental conditions (solvent type, pH, and instrument strength). If "all", returns all entries corresponding to supplied CAS numbers, experimental conditions ignored.
#'@param solvent_type choose from available solvents 'D2O', 'H2O', ...
#'@param ph numerical value specifying pH of the experimental conditions
#'@param instrument_strength numeric value specifying experimental instrument strength
#'@param temperature numeric value specifying the temperature
#'@param concentration numeric value specifying the concentration
#'
#'@return a list of BMSE database entry numbers
#'@importFrom utils data
#'@export as.bmseListFromName
#'@examples
#'# test <- as.bmseListFromName(list("ATP","Maltose" "Oxalate"), solvent_type = 'D2O', ph = 7.4)
#'
as.bmseListFromName <- function(name_list,
                                return_metabs = "all",
                                solvent_type = NULL,
                                ph = NULL,
                                instrument_strength,
                                temperature         = NULL,
                                concentration       = NULL){

  # Initial Checks
  if(!inherits(name_list, "list")){
    stop("List of metabolite names must be of the class 'list'.")
  }

  if(!(return_metabs %in% c("exact_match", "all"))){
    stop('return_metabs must be either "exact_match" or "all"')
  }

  if(return_metabs == "exact_match" & (is.null(solvent_type) | is.null(ph) | is.null(instrument_strength))){
    stop('Solvent type, ph, and spectrometer frequency must be specified if return_metabs = "exact_match"')
  }

  # fail check if one metabolite name is not in the db
  for (item in name_list){
    if (!(item %in% bmse_associations$Solute)){
      warning(sprintf("%s is not a recognized metabolite name", item))
    }
  }

  misscheck <- vector("numeric", length = length(name_list))
  if (return_metabs == "all") {
    #create list without filtering by exp conditions
    bmse_list <- list()

    for (i in 1:length(name_list)){
      subset    <- bmse_associations[bmse_associations$Solute == name_list[[i]], ]
      bmse_val  <- subset$Entry_ID
      bmse_list <- append(bmse_list, bmse_val)
      misscheck[i] <- ifelse(length(bmse_val) > 0, 0, 1)
    }
  } else if (return_metabs == "exact_match") {
    # The code chunk below was commented out because it forced all solvent types to D20.
    # #check solvent types, convert to acceptable types
    # if(solvent_type != "D2O"){
    #   solvent_type <- "D2O"
    # }

    #create list
    bmse_list <- list()
    for (i in 1:length(name_list)){
      subset <- bmse_associations %>% dplyr::filter(.data$Solute == name_list[[i]],
                                                    .data$Solvent == solvent_type,
                                                    .data$pH == ph)
      if(!is.na(temperature)){
        subset <- subset %>% dplyr::filter(.data$Temperature == temperature)
      }

      if(!is.na(concentration)){
        subset <- subset %>% dplyr::filter(.data$Concentration == concentration)
      }

      bmse_val  <- subset$Entry_ID
      bmse_list <- append(bmse_list, bmse_val)
      misscheck[i] <- ifelse(length(bmse_val) > 0, 0, 1)
    }
  }

  bmse_list <- unique(bmse_list)


  if (sum(misscheck) > 0 & return_metabs == "all"){
    message("Data not available for all metabolites.")
  }

  if (sum(misscheck) > 0 & return_metabs == "exact_match"){
    message("Data not available for all metabolites at the specified conditions.")
  }

  return(bmse_list)
}

#'Imports whole STAR file for a given entry id from a locally maintained respository of JSON formated BMRM-STAR files
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
#'@param ID single BMRB ID; For metabolomics entries it is just a number with bmrb prefix (example: 'bmse000028');
#'@return R list that contains STAR formatted data for a given entry ID
#'@export show_file
#'@examples
#'# file <- show_file("path/to/json/","bmse000028")
#'# Downloads entire STAR file from BMRB metabolomics database
#'@seealso \code{\link{get_spectra_data}}
show_file <- function(ID){

  #If the list of IDs entries is greater than one, exit the function
  if (length(ID) > 1){
    stop("'show_file()' takes only one ID, not a list of IDs")
  }

  # create ultimate path to the select BMSE json file
  file_location <- system.file("json_star", paste0(ID,".json"),
                               package = "nmRanalysis")

  #import the BMSE entry as a JSON
  spectra_file <- rjson::fromJSON(file = file_location)

  return(spectra_file)
}

#'Pulls spectra character data for a given entry id from a locally maintained respository of JSON formated BMRM-STAR files
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
#'@param ID_list list of BMRB IDs; For metabolomics entries it is just a number with BMRB prefix (example: 'bmse000028')
#'@return R data frame that contains  Spectral_transition_char data for a given entry ID
#'@export get_spectra_data
#'@examples
#'# df <- get_spectra_data("path/to/json/", as.bmseList(list("56-41-7","2613-02-7"), 400, 'D2O'))
#'# Downloads data from BMRB metabolomics database
#'@seealso \code{\link{show_file}}
#'@importFrom magrittr %>%
#'@importFrom plyr .
get_spectra_data <- function(ID_list){

  # set header and empty df for the table of peak IDs to be used later
  header                 <- c("Val", "Entry_ID", "COUNT", "quant_sig", "multiplicity", "Jcoupling", "Metabolite")
  spectra_data           <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(spectra_data) <- header

  # parse over the list of IDs taken from as.bmseList return
  for (ID in ID_list){
    spectra_data_subset           <- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(spectra_data_subset) <- header

    # import a JSON object for an entire STAR file per entry ID
    file <- show_file(ID = ID)

    # select the sub-list that contains the data in the STAR file
    y <- file[2][["saveframes"]]

    # store the types of the data objects in the STAR file
    argList <- lapply(y, function(x){names(x)})

    # store the names and categories of all the types of the data in an object
    namesL      <- unlist(lapply(1:length(y), function(pos){y[[pos]]$name}))
    categoriesL <- unlist(lapply(1:length(y), function(pos){y[[pos]]$category}))

    # extract the loops for Assigned chemical shift lists
    cs_loops <- y[[match("assigned_chemical_shifts", namesL)]]$loops

    #get metabolite name
    entry_info <- y[[match("entry_information", namesL)]]$tags
    metabolite_name <-unlist(entry_info[4])[2]

    # store the names of the data tables in the Assigned chemical shift lists
    cs_categories <- unlist(lapply(1:length(cs_loops), function(pos){cs_loops[[pos]]$category}))

    # store the "Assigned_peak_chem_shift" loop data in an object
    cs <- match('_Atom_chem_shift', cs_categories)


    ### Chemical shifts table ##################################################
    # print a message if no "Assigned_peak_chem_shift" loop is found in this STAR file
    if (is.na(cs)){
      message(sprintf("Message: No chemical shifts table found for entry %s", ID))
      next
    } else{
      z                <- cs_loops[[cs]]
      csdata           <- data.table::as.data.table(z$data)
      cstags           <- as.data.frame(data.table::as.data.table(z$tags))$V1
      csdata           <- as.data.frame(data.table::data.table(t(csdata)))
      colnames(csdata) <- cstags

      #set character "Val" column to numeric
      csdata <- csdata %>%
        dplyr::mutate(Val = as.numeric(.data$Val)) %>%
        dplyr::filter(.data$Val <= 11) #contain only H atoms

      #count up the peak counts by grouping unique values (not ideal)
      peak_quant <- csdata %>%
        dplyr::group_by(.data$Val) %>%
        dplyr::mutate(COUNT = dplyr::n())

      select_df               <- peak_quant[, names(peak_quant) %in% names(spectra_data_subset)]
      select_unique           <- select_df %>% dplyr::distinct() %>% dplyr::arrange(dplyr::desc(.data$Val)) # Potentially may affect downstream addition to dataset, but not likely.
      select_unique$quant_sig <- seq.int(nrow(select_unique))
      spectra_data_subset     <- rbind(spectra_data_subset,select_unique)

    }

    ############################################################################

    # Field strength extraction
    spectrometer_info_index <- grep("NMR_spectrometer", categoriesL)[1]
    instrument              <- y[[spectrometer_info_index]]$tags
    fs_index                <- which(sapply(instrument, function(d){"Field_strength" %in% d}))
    instrument_strength     <- as.numeric(instrument[[fs_index]][2])

    spectra_data_subset$instrument_strength <- instrument_strength
    spectra_data_subset$Metabolite <- metabolite_name

    ########## coupling and multiplicity extraction from supplement table #######
    # extract the loops for 1H NMR spectra from all possible data types in STAR file
    peak_loops <- y[[match("spectral_peak_1H", namesL)]]$loops

    # store the names of the data tables in the 1H spectra data
    peak_categories <- unlist(lapply(1:length(peak_loops), function(pos){peak_loops[[pos]]$category}))

    # store the peaks location and heights loop data
    peaks   <- match('_Spectral_transition_char', peak_categories)
    heights <- match('_Spectral_transition_general_char', peak_categories)

    # print an error if no peak location loop is found in this STAR file
    if (is.na(peaks)){
      (cat("Message: No supplemental peak table found for entry ", ID, "\n"))
      next
    } else{

      w                   <- peak_loops[[peaks]]
      peaksdata           <- data.table::as.data.table(w$data)
      peakstags           <- as.data.frame(data.table::as.data.table(w$tags))$V1
      peaksdata           <- as.data.frame(data.table::data.table(t(peaksdata)))
      colnames(peaksdata) <- peakstags

      v                         <- peak_loops[[heights]]
      heightsdata               <- data.table::as.data.table(v$data)
      heightstags               <- as.data.frame(data.table::as.data.table(v$tags))$V1
      heightsdata               <- as.data.frame(data.table::data.table(t(heightsdata)))
      colnames(heightsdata)     <- heightstags
      heightsdata$Intensity_val <- as.numeric(heightsdata$Intensity_val)

      # check to see if ordering is consistent with ordering of spectra_data_subset
      if(as.numeric(peaksdata$Chem_shift_val[1]) - as.numeric(peaksdata$Chem_shift_val[length(peaksdata$Chem_shift_val)]) < 0){
        peaksdata <- peaksdata %>% dplyr::arrange(dplyr::desc(as.numeric(.data$Spectral_transition_ID)))
        heightsdata <- heightsdata %>% dplyr::arrange(dplyr::desc(as.numeric(.data$Spectral_transition_ID)))
      }


      if(ID == "bmse000119"){ # need to fix
        x                <- as.numeric(peaksdata$Chem_shift_val)
        peak_groups      <- list(x[1:2],
                                 x[3:4],
                                 x[5:7],
                                 x[8:15],
                                 x[16:18],
                                 x[19:21],
                                 x[22],
                                 x[23:26],
                                 x[27:29])
        peak_groups_idxs <- list(c(1:2),
                                 c(3:4),
                                 c(5:7),
                                 c(8:15),
                                 c(16:18),
                                 c(19:21),
                                 c(22),
                                 c(23:26),
                                 c(27:29))
      } else if(ID == "HMDB00001264"){
        x                <- as.numeric(peaksdata$Chem_shift_val)
        peak_groups      <- list(x[1:7],
                                 x[8:11],
                                 x[12:15],
                                 x[16:25],
                                 x[26:35])
        peak_groups_idxs <- list(c(1:7),
                                 c(8:11),
                                 c(12:15),
                                 c(16:25),
                                 c(26:35))
      } else{
        # use the chemical shift column to get coupling values
        # Check over a range of tolerance values, take the max tolerance such that
        # the number of groupings is equal to the number of peak centers (i.e. nrow(spectra_data_s))
        tol_candidates <- seq(0.01, 0.1, by = 0.001)
        tempind <- sapply(tol_candidates, function(tol){
          x           <- as.numeric(peaksdata$Chem_shift_val)
          split_idxs  <- which(abs(diff(x)) > tol) + 1
          peak_groups <- split(x, cumsum(seq_along(x) %in% split_idxs))

          return(length(peak_groups) == nrow(spectra_data_subset))
        })

        tol              <- max(tol_candidates[tempind]) #ppm
        x                <- as.numeric(peaksdata$Chem_shift_val)
        split_idxs       <- which(abs(diff(x)) > tol) + 1
        peak_groups      <- split(x, cumsum(seq_along(x) %in% split_idxs))
        peak_groups_idxs <- split(1:length(x), cumsum(seq_along(x) %in% split_idxs))
      }

      multiplicity_values <- lapply(peak_groups, function(x){length(x)})

      ####### midpoint calc #######
      midpoints <- lapply(peak_groups, mean)

      #calculate J-coupling
      Jcoupling <- vector("numeric", length = length(peak_groups_idxs))
      for(i in 1:length(Jcoupling)){
        pg <- peak_groups_idxs[[i]]

        if(length(pg) == 1){
          #singlet
          Jcoupling[i] <- 0 # J-coupling should be 0 if a singlet
        }

        if(length(pg) == 2){
          # doublet
          ratio <- (heightsdata$Intensity_val[pg[1]])/(heightsdata$Intensity_val[pg[2]])
          ratio <- round(ratio, digits=1)

          Jcoupling[i] <- min(abs(diff(x[pg])))
        }

        if (length(pg) == 3){
          #triplet
          ratio <- (heightsdata$Intensity_val[pg[2]])/(heightsdata$Intensity_val[pg[1]])
          ratio <- round(ratio, digits = 1)

          Jcoupling[i] <- min(abs(diff(x[pg])))
        }

        if (length(pg) == 4){
          #quartet
          ratio <- (heightsdata$Intensity_val[pg[2]])/(heightsdata$Intensity_val[pg[1]])
          ratio <- (heightsdata$Intensity_val[pg[3]])/(heightsdata$Intensity_val[pg[4]])
          ratio <- round(ratio, digits=1)

          Jcoupling[i] <- min(abs(diff(x[pg])))
        }

        if(length(pg) > 4){
          #multiplet
          Jcoupling[i] <- min(abs(diff(x[pg])))
        }
      }
    }

    if (length(multiplicity_values) == nrow(spectra_data_subset)){
      spectra_data_subset$multiplicity <- multiplicity_values
    } else{ #Flag
      spectra_data_subset$multiplicity <- rep(0, nrow(spectra_data_subset))
    }

    if (length(Jcoupling) == nrow(spectra_data_subset)){
      spectra_data_subset$Jcoupling <- Jcoupling
    } else{ #Flag
      spectra_data_subset$Jcoupling <- rep(0, nrow(spectra_data_subset))
    }

    spectra_data_subset <- as.data.frame(spectra_data_subset)

    # append df of each ID to the main DF
    spectra_data <- rbind(spectra_data, spectra_data_subset)
  }


  return(spectra_data)
}


#' Format spectra data into a proper rDolphin ROI reference file
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
#' @param spectra_df data frame of the spectra from get_spectra_data() function
#' @param half_bandwidth This will be set to 1.4 unless otherwise specified
#' @param roi_tol The chemical shift tolerance, tolerance for the location of the peak center. Default is 0.005.
#'
#' @return a data frame formatted for r dolphin use to be exported as excel sheet
#' @export export_roi_file
export_roi_file <- function(spectra_df,
                            half_bandwidth = 1.4,
                            roi_tol        = 0.005){

  # (Note that HMDB_code is removed and NOT returned)

  # create column names vector
  header <- c('ROI left edge (ppm)', 'ROI right edge (ppm)', 'Quantification Mode',
              'Metabolite',	'Quantification Signal', 'Chemical shift(ppm)',	'Chemical shift tolerance (ppm)',
              'Half bandwidth (Hz)', 'Multiplicity', 'J coupling (Hz)',	'Roof effect', 'J coupling 2 (Hz)',
              'Roof effect 2', 'Frequency (MHz)', 'pH', 'Concentration (mM)', 'Temperature (K)', 'Solvent')


  # reference the entry ID to the bmse metadata
  metab_name <- unique(merge(x = spectra_df, y = bmse_associations[, c("Entry_ID", "Solute", "Solvent", "pH", "Temperature", "Concentration")], by="Entry_ID", all.x=TRUE))

  #set default constants for columns (quantification mode, roof effect, and tolerance)
  q_mode       <- rep("Baseline Fitting", nrow(metab_name))
  roof_effect  <- rep(0, nrow(metab_name))
  roof_effect2 <- roof_effect
  jcoupl2      <- rep(0, nrow(metab_name))
  tolerance    <- rep(roi_tol, nrow(metab_name))

  # if the user did not provide a custom bandwidth, set default
  if (missing(half_bandwidth)){
    half_bandwidth_col <- rep(1.4, nrow(metab_name))
  } else{ #else use the user input
    half_bandwidth_col <- rep(half_bandwidth, nrow(metab_name))
  }

  roi_left  <- unlist(metab_name$Val) + roi_tol
  roi_right <- unlist(metab_name$Val) - roi_tol

  # Coupling
  coupling <- as.numeric(metab_name$Jcoupling) * as.numeric(metab_name$instrument_strength)

  # combine all the columns
  roi_df <- cbind.data.frame(as.numeric(roi_left),
                             as.numeric(roi_right),
                             q_mode,
                             metab_name$Solute,
                             as.numeric(metab_name$quant_sig),
                             as.numeric(metab_name$Val),
                             as.numeric(tolerance),
                             as.numeric(half_bandwidth_col),
                             as.character(metab_name$multiplicity),
                             as.numeric(coupling),
                             as.numeric(roof_effect),
                             as.numeric(jcoupl2),
                             as.numeric(roof_effect2),
                             metab_name$instrument_strength,
                             metab_name$pH,
                             metab_name$Concentration,
                             metab_name$Temperature,
                             metab_name$Solvent)
  colnames(roi_df) <- header

  # Average duplicate entries by experimental conditions
  avg_roi_df <- roi_df %>%
    dplyr::group_by(.data$`Quantification Mode`, .data$`Metabolite`, .data$`Quantification Signal`, .data$`Frequency (MHz)`,
                    .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$`Solvent`) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(c('ROI left edge (ppm)', 'ROI right edge (ppm)', 'Chemical shift(ppm)',	'Chemical shift tolerance (ppm)',
                                                   'Half bandwidth (Hz)', 'J coupling (Hz)',	'Roof effect', 'J coupling 2 (Hz)',
                                                   'Roof effect 2')), mean, na.rm = TRUE),
                     dplyr::across(dplyr::all_of(c('Multiplicity')), getmode, useNA = "no")) %>%
    dplyr::select(.data$`ROI left edge (ppm)`, .data$`ROI right edge (ppm)`, .data$`Quantification Mode`,
                  .data$`Metabolite`,	.data$`Quantification Signal`, .data$`Chemical shift(ppm)`,
                  .data$`Chemical shift tolerance (ppm)`, .data$`Half bandwidth (Hz)`, .data$`Multiplicity`,
                  .data$`J coupling (Hz)`,	.data$`Roof effect`, .data$`J coupling 2 (Hz)`, .data$`Roof effect 2`,
                  .data$`Frequency (MHz)`, .data$`pH`, .data$`Concentration (mM)`, .data$`Temperature (K)`, .data$`Solvent`) %>%
    dplyr::arrange(.data$`ROI left edge (ppm)`)

  return(avg_roi_df)
}

#' Wrap the generating and exporting functions into one
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
#' @param name_list list of metabolite names if cas numbers are not provided
#' @param cas_list list of CAS registry numbers written as characters if metabolite names are not provided
#' @param return_metabs string, must be one of "exact_match" or "all". Defaults to "all". If "exact_match", returns metabolites that exactly match specified experimental conditions (solvent type, pH, and instrument strength). If "all", returns all entries corresponding to supplied CAS numbers, experimental conditions ignored.
#' @param ph the experimental pH
#' @param solvent_type the experimental solvent, choose from available solvents 'D2O', 'H2O', ...
#' @param half_bandwidth This will be set to 1.4 unless otherwise specified
#' @param roi_tol The chemical shift tolerance, tolerance for the location of the peak center. Default is 0.005.
#' @param instrument_strength the field strength of the instrument used to collect the data
#' @param temperature numeric value specifying the temperature
#' @param concentration numeric value specifying the concentration
#'
#' @return a data frame formatted for rDolphin use to be exported as excel sheet
#' @export roi_ref_export
roi_ref_export <- function(name_list           = NULL,
                           cas_list            = NULL,
                           return_metabs       = "all",
                           solvent_type        = NULL,
                           ph                  = NULL,
                           half_bandwidth      = 1.4,
                           roi_tol             = 0.005,
                           instrument_strength = NULL,
                           temperature         = NULL,
                           concentration       = NULL){

  # set ID list object
  id_list <- NULL
  if (!(is.null(name_list))) {
    id_list <- as.bmseListFromName(name_list           = name_list,
                                   return_metabs       = return_metabs,
                                   solvent_type        = solvent_type,
                                   ph                  = ph,
                                   instrument_strength = instrument_strength,
                                   temperature         = temperature,
                                   concentration       = concentration)
  } else{
    id_list <- as.bmseList(casno_list          = cas_list,
                           return_metabs       = return_metabs,
                           solvent_type        = solvent_type,
                           ph                  = ph,
                           instrument_strength = instrument_strength,
                           temperature         = temperature,
                           concentration       = concentration)
  }

  # get spectra data
  saveframe <- get_spectra_data(ID_list = id_list)

  # return the ROI file formatted object and export CSV
  roi_df <- export_roi_file(spectra_df          = saveframe,
                            half_bandwidth      = half_bandwidth,
                            roi_tol             = roi_tol)

  return(roi_df)
}

#' Return nearest match metabolites, sorted by Euclidean distance
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
#' @param roi_df data.frame, output of \code{roi_ref_export} with parameter \code{return_all = TRUE}
#' @param pH single numeric value specifying experimental pH
#' @param instrument_strength single numeric value specifying experimental instrument strength
#' @return data.frame
#' @export
nearest_match_metabs <- function(roi_df, pH, instrument_strength) {

  # use HMDB code as unique ID for specific solute/pH/field strength combination
  metab_id <- roi_df$HMDB_code

  # get experimental parameters associated w/each metab_id
  metab_ph <- vector()
  metab_FS <- vector()
  dist <- vector()
  for (i in 1:length(metab_id)) {
    metab_ph[i] <- bmse_associations %>%
      dplyr::filter(.data$Entry_ID == metab_id[i]) %>%
      dplyr::pull(pH) %>%
      unique() %>%
      as.numeric()

    metab_FS[i] <- bmse_associations %>%
      dplyr::filter(.data$Entry_ID == metab_id[i]) %>%
      dplyr::pull(.data$Field_strength) %>%
      unique() %>%
      as.numeric()

    # calculate Euclidean distance
    dist[i] <- sqrt((pH - metab_ph[i])^2 + (instrument_strength - metab_FS[i])^2)
  }

  metabs <- as.data.frame(cbind(metab_id, metab_ph, metab_FS, as.numeric(dist))) %>%
    dplyr::arrange(dist)

  return(metabs)
}

