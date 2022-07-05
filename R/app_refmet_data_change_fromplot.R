#' Record any changes made to target reference metabolite data based on plot edits
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
#' @param dspedt_refmet_data A dataframe containing fitting parameter information specific to a single target metabolite. See details.
#' @param event_data plotly event data generated from user changes to plotly figure shapes. See details.
#' @param x0_fields A string denoting the left endpoint ID of the plotly figure shape edited by the user. See details.
#' @param x1_fields A string denoting the right endpoint ID of the plotly figure shape edited by the user. See details.
#' @param round_num An integer specifying the number of decimals to round generated values to.
#' @param rv optional reactiveValues list containing the element "subplot_idx". Only applicable when making changes from generated subplots.
#'
#' The dspedt_refmet_data dataframe should have the following named columns, in the following order: "ROI left edge (ppm)",
#' "ROI right edge (ppm)", "Quantification Mode", "Metabolite", "Quantification Signal", "Chemical shift(ppm)",
#' "Chemical shift tolerance (ppm)", "Half bandwidth (Hz)", "Multiplicity", "J coupling (Hz)", "Roof effect",
#' "Quantify", "HMDB_code", and "rowid". Only the peak centers of a single, user-selected metabolite should be represented
#' within this dataframe.
#' The event data used by this function contains the specific coordinate changes of plotly plot shapes representative of
#' metabolite peak regions of interest (ROI).
#' The x0_fields and x1_fields strings are used to select the endpoint of the plotly plot shape that was edited. In the case of multiple shapes
#' (i.e. ROI lines) per target metabolite, the ID indicated by each is an integer ordered by the rank of the endpoint relative
#' to those of other ROIs, starting from 0. For example, if a plot has three ROIs - (x0 = 1, x1 = 2), (x0 = 3, x1 = 4), (x0 = 5, x1 = 6) - then
#' the ID for the second ROI would be 1 since it has the second largest endpoints (i.e. 1 < 3 < 5 and 2 < 4 < 6).
#'
#' @return A dataframe containing fitting parameter information specific to a single target metabolite, updated to reflect the
#' plot-induced changes
#'
refmet_data_change_fromplot <- function(dspedt_refmet_data, event_data, x0_fields, x1_fields, round_num = 3, rv = NULL){

  if(!is.null(rv)){
    ROI_idx_adjustment <- min(which(rv$subplot_idx))
  } else{
    ROI_idx_adjustment <- 1
  }

  if(!any(is.na(c(x0_fields, x1_fields)))){

    # Get row number where edits apply
    changed_row <-
      as.numeric(gsub("[^0-9.-]", "",
                      regmatches(x0_fields,
                                 gregexpr("\\[.*?\\]", x0_fields))[[1]])) + ROI_idx_adjustment

    # Get actual changes made
    relevant_changes_x1 <- event_data[[x1_fields]]
    relevant_changes_x0 <- event_data[[x0_fields]]
    new_chemshift       <- (relevant_changes_x0 + relevant_changes_x1)/2

    # Update values in dspedt, but values not saved unless user clicks save button
    dspedt_refmet_data$"ROI left edge (ppm)"[changed_row]  <- round(relevant_changes_x0, round_num)
    dspedt_refmet_data$"ROI right edge (ppm)"[changed_row] <- round(relevant_changes_x1, round_num)
    dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]  <- round(new_chemshift, round_num)

  } else if(is.na(x0_fields)){

    changed_row <-
      as.numeric(gsub("[^0-9.-]", "",
                      regmatches(x1_fields,
                                 gregexpr("\\[.*?\\]", x1_fields))[[1]])) + ROI_idx_adjustment

    relevant_changes_x1 <- event_data[[x1_fields]]

    diff1 <- relevant_changes_x1 - dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]
    diff2 <- dspedt_refmet_data$"ROI left edge (ppm)"[changed_row] -
      dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]

    dist <- ifelse(relevant_changes_x1 < dspedt_refmet_data$"ROI right edge (ppm)"[changed_row],
                   max(abs(diff1), abs(diff2)),
                   min(abs(diff1), abs(diff2)))

    dspedt_refmet_data$"ROI right edge (ppm)"[changed_row]  <-
      round(dspedt_refmet_data$"Chemical shift(ppm)"[changed_row] - dist, round_num)
    dspedt_refmet_data$"ROI left edge (ppm)"[changed_row]  <-
      round(dspedt_refmet_data$"Chemical shift(ppm)"[changed_row] + dist, round_num)
    dspedt_refmet_data$"Chemical shift tolerance (ppm)"[changed_row] <- round(min(dist/2, 0.005), round_num)

  } else if(is.na(x1_fields)){

    # See above comments
    changed_row <-
      as.numeric(gsub("[^0-9.-]", "",
                      regmatches(x0_fields,
                                 gregexpr("\\[.*?\\]", x0_fields))[[1]])) + ROI_idx_adjustment

    relevant_changes_x0 <- event_data[[x0_fields]]

    diff1 <- relevant_changes_x0 - dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]
    diff2 <- dspedt_refmet_data$"ROI right edge (ppm)"[changed_row] -
      dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]

    dist <- ifelse(relevant_changes_x0 < dspedt_refmet_data$"ROI left edge (ppm)"[changed_row],
                   min(abs(diff1), abs(diff2)),
                   max(abs(diff1), abs(diff2)))

    dspedt_refmet_data$"ROI right edge (ppm)"[changed_row]  <-
      round(dspedt_refmet_data$"Chemical shift(ppm)"[changed_row] - dist, round_num)
    dspedt_refmet_data$"ROI left edge (ppm)"[changed_row]  <-
      round(dspedt_refmet_data$"Chemical shift(ppm)"[changed_row] + dist, round_num)
    dspedt_refmet_data$"Chemical shift tolerance (ppm)"[changed_row] <- round(min(dist/2, 0.005), round_num)
  }

  return(dspedt_refmet_data)
}
