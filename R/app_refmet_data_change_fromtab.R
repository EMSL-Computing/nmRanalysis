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
#' @param changed_row An integer denoting the row of the table edit.
#' @param change The updated value. May be either a string or numeric depending on the table column edited.
#' @param edtd_colname A string denoting the name of the column of the table edit
#' @param round_num An integer specifying the number of decimals to round changed (numeric) values to.
#'
#' @details The dspedt_refmet_data dataframe should have the following named columns, in the following order: "ROI left edge (ppm)",
#' "ROI right edge (ppm)", "Quantification Mode", "Metabolite", "Quantification Signal", "Chemical shift(ppm)",
#' "Chemical shift tolerance (ppm)", "Half bandwidth (Hz)", "Multiplicity", "J coupling (Hz)", "Roof effect",
#' "Quantify", "HMDB_code", and "rowid". Only the peak centers of a single, user-selected metabolite should be represented
#' within this dataframe.
#'
#' @return A dataframe containing fitting parameter information specific to a single target metabolite, updated to reflect the
#' table-induced changes
#'
refmet_data_change_fromtab <- function(dspedt_refmet_data, changed_row, change, edtd_colname, round_num = 3){

  if(edtd_colname == "Chemical shift(ppm)"){

    dist <- (dspedt_refmet_data$"ROI left edge (ppm)"[changed_row] -
               dspedt_refmet_data$"ROI right edge (ppm)"[changed_row])/2

    new_chemshift <- change

    dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]  <- new_chemshift
    dspedt_refmet_data$"ROI left edge (ppm)"[changed_row]  <- new_chemshift + dist
    dspedt_refmet_data$"ROI right edge (ppm)"[changed_row] <- new_chemshift - dist
    dspedt_refmet_data$"Chemical shift tolerance (ppm)"[changed_row] <- round(min(dist/2, 0.005), round_num)

  } else if(edtd_colname == "Chemical shift tolerance (ppm)") {

    new_tol   <- change
    chemshift <- dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]

    dspedt_refmet_data$"ROI left edge (ppm)"[changed_row]  <- chemshift + round(new_tol, round_num)
    dspedt_refmet_data$"ROI right edge (ppm)"[changed_row] <- chemshift - round(new_tol, round_num)
    dspedt_refmet_data$"Chemical shift tolerance (ppm)"[changed_row] <- round(new_tol, round_num)

  } else if(edtd_colname == "Signal left edge (ppm)"){

    new_ROIs <- c(change,
                  dspedt_refmet_data$"ROI right edge (ppm)"[changed_row])
    chemshift <- dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]

    dist <- ifelse(new_ROIs[1] < dspedt_refmet_data$"ROI left edge (ppm)"[changed_row],
                   min(abs(chemshift - new_ROIs[1]), abs(chemshift - new_ROIs[2])),
                   max(abs(chemshift - new_ROIs[1]), abs(chemshift - new_ROIs[2])))

    dspedt_refmet_data$"ROI left edge (ppm)"[changed_row]  <- chemshift + round(dist, round_num)
    dspedt_refmet_data$"ROI right edge (ppm)"[changed_row] <- chemshift - round(dist, round_num)
    dspedt_refmet_data$"Chemical shift tolerance (ppm)"[changed_row] <- round(min(dist/2, 0.005), round_num)

  } else if(edtd_colname == "Signal right edge (ppm)"){

    new_ROIs <- c(dspedt_refmet_data$"ROI left edge (ppm)"[changed_row],
                  change)
    chemshift <- dspedt_refmet_data$"Chemical shift(ppm)"[changed_row]

    dist <- ifelse(new_ROIs[2] < dspedt_refmet_data$"ROI right edge (ppm)"[changed_row],
                   max(abs(chemshift - new_ROIs[1]), abs(chemshift - new_ROIs[2])),
                   min(abs(chemshift - new_ROIs[1]), abs(chemshift - new_ROIs[2])))

    dspedt_refmet_data$"ROI left edge (ppm)"[changed_row]  <- chemshift + round(dist, round_num)
    dspedt_refmet_data$"ROI right edge (ppm)"[changed_row] <- chemshift - round(dist, round_num)
    dspedt_refmet_data$"Chemical shift tolerance (ppm)"[changed_row] <- round(min(dist/2, 0.005), round_num)

  }

  return(dspedt_refmet_data)
}
