#' Search Parameters to Edit
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
#' This function acquires the parameters from the reference table to specify which field of which entry needs to be edited. Also negates the need to open external reference files outside of nmrAnalysis package.
#'
#' @param roi_df data frame returned from roi_ref_export() that contains the rDolphin reference data
#' @param metabolite_name NULL by default, may be set to a character length of one or set to a character vector of specified length
#' @param ppm the chemical shift of the peak that does not have a good fit
#' @param tolerance chemical shift tolerance to search for a peak within, default 0.05 ppm
#'
#' @return Table of current parameter values with one row per metabolite that meets criteria
#'
#' @author Allison Thompson
#'
#' @export search_edit_params
search_edit_params <- function(roi_df, metabolite_name = NULL, ppm, tolerance = 0.05){

  #make sure roi_df is a data frame
  roi_df <- as.data.frame(roi_df)

  #check ppm is numeric
  if(class(ppm) != "numeric"){
    stop("'ppm' must be numeric")
  }

  # set ppm search ranges to use later
  lower_range <- ppm - tolerance
  upper_range <- ppm + tolerance

  #subset the roi data frame based on ppm ranges
  subset_df <- roi_df %>%
    tibble::rownames_to_column(var = "Index") %>%
    dplyr::filter(.data$`Chemical.shift.ppm.` > lower_range & .data$`Chemical.shift.ppm.` < upper_range)

  # if no metabolite name specified
  if (is.null(metabolite_name)) {
    #returns a data frame object with all editable parameter options
    return(subset_df)
  } else{ #use metabolite name to get parameters in addition to ppm

    #subset the roi data frame based on ppm ranges
    name_subset_df <- subset(subset_df, subset_df$Metabolite == metabolite_name)
    # name_subset_df <- subset_df %>%
    #   dplyr::filter(.data$Metabolite == metabolite_name)

    #returns a data frame object with all editable parameter options
    return(name_subset_df)
  }
}


#' Edit Reference Data
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
#' @param roi_df data frame returned from roi_ref_export() that contains the rDolphin reference data
#' @param metabolite_name character vector with one or more entries
#' @param ppm numeric vector with one or more entries; chemical shift of the parameter to be changed
#' @param tolerance chemical shift tolerance to search for a peak within; default is 0.05
#' @param parameter_name character vector with one or more entries; parameters to be edited
#' @param parameter_value character vector with one or more entries; the new value a parameter will be set to with the specified identifiers
#'
#' @return an edited data.frame formatted for rDolphin use
#'
#' @export edit_reference_data
edit_reference_data <- function(roi_df, metabolite_name, ppm, tolerance=0.05, parameter_name, parameter_value){

  # for only one value edit
  if (length(parameter_value) == 1){

    # get row names of the subset of data
    subset_df <- search_edit_params(roi_df, metabolite_name, ppm, tolerance)
    row_names <- subset_df$Index

    if (length(row_names) == 1){

      #get number of the edit row
      r_num <- as.numeric(row_names)

      #make edit
      roi_df[r_num, parameter_name] <- parameter_value

      #track edit
      edit_num                        <- length(attributes(roi_df)$edits) + 1
      edit_note                       <- sprintf("row: %s, column: %s, value: %s", row_names, parameter_name, parameter_value)
      attr(roi_df, "edits")[edit_num] <- edit_note
    }
  }

  #for multiple value edits
  if(length(parameter_value) > 1){

    #if a vector of length > 1 is provided for any argument,
    #check all lengths of all parameters match, else, error
    if (!(identical(length(metabolite_name), length(ppm), length(parameter_name), length(parameter_value)))){
      stop("Input parameters not of equal length")
    }

    #iterate over the lists of input parameters
    for (i in 1:length(parameter_value)){
      n  <- metabolite_name[i]
      p  <- ppm[i]
      pn <- parameter_name[i]
      pv <- parameter_value[i]

      # get row names of the subset of data
      subset_df <- search_edit_params(roi_df, n, p, tolerance)
      row_names <- subset_df$Index

      #get number of the edit row
      r_num <- as.numeric(row_names)

      #make edit
      roi_df[which(rownames(roi_df) == r_num), pn] <- pv

      #track edit
      edit_num                        <- length(attributes(roi_df)$edits) + 1
      edit_note                       <- sprintf("row: %s, column: %s, value: %s", row_names, pn, pv)
      attr(roi_df, "edits")[edit_num] <- edit_note
    }

    #print message that multiple edits were made
    message("Multiple changes were made to input data frame.")
  }

  #edit_attributes are returned within the edit_rof object AS ATTRIBUTES
  return(roi_df)
}
