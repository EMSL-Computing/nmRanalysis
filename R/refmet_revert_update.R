#' Revert saved change(s) to reference metabolite fitting parameters.
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
refmet_revert_update <- function(updated_refmet, rvlist, all = FALSE){
  cmet <- updated_refmet
  
  if(all){
    changed_rows <- rvlist$refchanges[[cmet]][[1]]$rowid
    
    rvlist$user_reference_data[rvlist$user_reference_data$rowid %in% changed_rows,] <- 
      rvlist$refchanges[[cmet]][[1]]
    
    rvlist$dspedt_user_reference_data <- 
      rvlist$user_reference_data[rvlist$user_reference_data$rowid %in% changed_rows,]
    
    rvlist$change_counter[[cmet]] <- 0
    
  } else {
    changed_rows <- rvlist$refchanges[[cmet]][[rvlist$change_counter[[cmet]]]]$rowid
    
    rvlist$user_reference_data[rvlist$user_reference_data$rowid %in% changed_rows,] <- 
      rvlist$refchanges[[cmet]][[rvlist$change_counter[[cmet]]]] # revert to previous values
    
    rvlist$dspedt_user_reference_data <- 
      rvlist$user_reference_data[rvlist$user_reference_data$rowid %in% changed_rows,] # similarly revert dspedt copy
    
    rvlist$change_counter[[cmet]] <- rvlist$change_counter[[cmet]] - 1 # move the change counter down a tick
    
  }
  
  rvlist$unsaved_change[[cmet]] <- NULL
  
  return(rvlist)
}