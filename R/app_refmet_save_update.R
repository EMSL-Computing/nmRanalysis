#' Save updates to reference metabolite fitting parameters
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
refmet_save_update <- function(updated_refmet, rvlist){
  cmet <- updated_refmet

  rvlist$change_counter[[cmet]] <- sum(c(rvlist$change_counter[[cmet]], 1))
  changed_rows <- rvlist$dspedt_user_reference_data$rowid

  rvlist$refchanges[[cmet]][[rvlist$change_counter[[cmet]]]] <-
    rvlist$user_reference_data[rvlist$user_reference_data$rowid %in% changed_rows,] # store values prior to change

  rvlist$user_reference_data[rvlist$user_reference_data$rowid %in% changed_rows,] <-
    rvlist$dspedt_user_reference_data # Update values to change

  rvlist$unsaved_change[[cmet]] <- NULL

  return(rvlist)
}
