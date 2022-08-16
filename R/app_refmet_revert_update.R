#' Revert saved change(s) to reference metabolite fitting parameters.
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
#' @param updated_refmet A string providing the name of the edited target (reference) metabolite.
#' @param rvlist A list. See details.
#' @param all A logical, default is FALSE. If TRUE, all saved changes are reverted. Otherwise, only the last saved change
#' is reverted.
#'
#' The object supplied to "rvlist" is technically a reactive values object, but these objects behave very similarly to lists.
#' The elements of rvlist are as follows:
#' 1) change_counter: a named list where each element is a counter for the number of changes applied to the particular target metabolite.
#' 2) refchanges: A named list of lists, where each element is a list documenting the changes saved for the particular target metabolite.
#' 3) unsaved_change: A named list where each element contains any changed data which has yet to be saved for a particular target metabolite.
#' 4) user_reference_data: The initially uploaded set of target metabolite data containing all specified target metabolites and associated fitting parameter information.
#' 5) dspedt_user_reference_data: The fitting parameter information specified to a particular target metabolite.
#' 6) dspedt_user_xpmt_data: Experimental data filtered to include only PPM values in the neighborhood of the peak centers of the target metabolite featured in dspedt_user_reference_data
#'
#' @return A list containing the same elements as rvlist, but with values updated accordingly.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
refmet_revert_update <- function(updated_refmet, rvlist, all = FALSE){
  cmet <- updated_refmet

  if(all){
    unedited_version <- rvlist$unedited_bestmatch_ref_data %>% dplyr::filter(.data$Metabolite %in% cmet)
    current_version <- rvlist$user_reference_data[rvlist$user_reference_data$Metabolite == cmet,]
    added_signals <- nrow(current_version) - nrow(unedited_version)

    rvlist$user_reference_data[rvlist$user_reference_data$Metabolite == cmet &
                                 rvlist$user_reference_data$`Quantification Signal` %in% unedited_version$`Quantification Signal`,] <-
      unedited_version

    if(added_signals > 0){
      added_entry_data <- data.frame(`ROI left edge (ppm)`            = rep(0.02, added_signals),
                                     `ROI right edge (ppm)`           = rep(-0.02, added_signals),
                                     `Quantification Mode`            = rep("Baseline Fitting", added_signals),
                                     `Metabolite`                     = input$which_refmet_dspedt,
                                     `Quantification Signal`          = c((nrow(unedited_version) + 1):(nrow(unedited_version) + added_signals)),
                                     `Chemical shift(ppm)`            = rep(0, added_signals),
                                     `Chemical shift tolerance (ppm)` = rep(0.02, added_signals),
                                     `Half bandwidth (Hz)`            = rep(1.4, added_signals),
                                     `Multiplicity`                   = rep("1", added_signals),
                                     `J coupling (Hz)`                = rep(0, added_signals),
                                     `Roof effect`                    = rep(0, added_signals),
                                     `J coupling 2 (Hz)`              = rep(0, added_signals),
                                     `Roof effect 2`                  = rep(0, added_signals),
                                     `Quantify`                       = rep(1, added_signals),
                                     `Frequency (MHz)`                = rep(attr(xpmt_data(), "exp_info")$instrument_strength, added_signals),
                                     `pH`                             = rep(attr(xpmt_data(), "exp_info")$ph, added_signals),
                                     `Concentration (mM)`             = rep(attr(xpmt_data(), "exp_info")$concentration, added_signals),
                                     `Temperature (K)`                = rep(attr(xpmt_data(), "exp_info")$temperature, added_signals),
                                     `Solvent`                        = rep(attr(xpmt_data(), "exp_info")$solvent, added_signals),
                                     `rowid`                          = paste0(input$which_refmet_dspedt, c((nrow(unedited_version) + 1):(nrow(unedited_version) + added_signals))),
                                     check.names = FALSE)

      rvlist$user_reference_data[rvlist$user_reference_data$Metabolite == cmet &
                                   rvlist$user_reference_data$`Quantification Signal` %ni% unedited_version$`Quantification Signal`,] <-
        added_entry_data
    }
    rvlist$dspedt_user_reference_data <-
      rvlist$user_reference_data %>% dplyr::filter(.data$Metabolite %in% cmet)

    rvlist$change_counter[[cmet]] <- 0

  } else {

    changed_rows <- rvlist$refchanges[[cmet]][[rvlist$change_counter[[cmet]]]]$rowid

    rvlist$user_reference_data[rvlist$user_reference_data$rowid %in% changed_rows,] <-
      rvlist$refchanges[[cmet]][[rvlist$change_counter[[cmet]]]] # revert to previous values

    rvlist$dspedt_user_reference_data[rvlist$dspedt_user_reference_data$rowid %in% changed_rows,] <-
      rvlist$user_reference_data[rvlist$user_reference_data$rowid %in% changed_rows,] # similarly revert dspedt copy

    rvlist$change_counter[[cmet]] <- rvlist$change_counter[[cmet]] - 1 # move the change counter down a tick

  }

  rvlist$unsaved_change[[cmet]] <- NULL

  return(rvlist)
}
