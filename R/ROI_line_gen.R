#' Generate plotly shapes (lines) representative of regions of interest for a specific target metabolite
#'
#' @param dspedt_refmet_data A dataframe containing the fitting parameter information specified to a particular target metabolite. See details.
#'
#' @details The dataframe required by dspedt_refmet_data should contain all of the fitting parameter information specific to the peak centers of a selected
#' reference metabolite. This dataframe should have the following named columns, in the following order: "ROI left edge (ppm)", 
#' "ROI right edge (ppm)", "Quantification Mode", "Metabolite", "Quantification Signal", "Chemical shift(ppm)", 
#' "Chemical shift tolerance (ppm)", "Half bandwidth (Hz)", "Multiplicity", "J coupling (Hz)", "Roof effect", 
#' "Quantify", "HMDB_code", and "rowid".
#' 
#' @return A list containing information necessary to create plotly shapes (lines).
#'
ROI_line_gen <- function(dspedt_refmet_data){
  
  # Create default line object to add as shape to plot
  ROI_line <- list(
    type = "line",
    line = list(color = "red"),
    xref = "x",
    yref = "y"
  )
  
  # Create list containing all line objects. For each line object in this list, populate with the
  # ROI information corresponding to the given reference metabolite peak.
  ROI_lines <- list()
  for(i in 1:nrow(dspedt_refmet_data)){
    
    if (dspedt_refmet_data$Quantify[i] == 1){
      
      ROI_line[["x0"]]        <- dspedt_refmet_data[i,,drop = FALSE]$"ROI left edge (ppm)"
      ROI_line[["x1"]]        <- dspedt_refmet_data[i,,drop = FALSE]$"ROI right edge (ppm)"
      ROI_line[c("y0", "y1")] <- 0
      
    } else {
      
      ROI_line[["x0"]]        <- dspedt_refmet_data[i,,drop = FALSE]$"Chemical shift(ppm)"
      ROI_line[["x1"]]        <- dspedt_refmet_data[i,,drop = FALSE]$"Chemical shift(ppm)"
      ROI_line[c("y0", "y1")] <- 0
      
    }
    ROI_lines               <- c(ROI_lines, list(ROI_line))
  }
  
  return(ROI_lines)
}