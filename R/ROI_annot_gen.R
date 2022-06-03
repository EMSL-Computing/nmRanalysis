 #' Generate plotly annotations representative of regions of interest for a specific target metabolite
#'
#' @param dspedt_refmet_data A dataframe containing the fitting parameter information specified to a particular target metabolite. See details. 
#'
#' @details The dataframe required by dspedt_refmet_data should contain all of the fitting parameter information specific to the peak centers of a selected
#' reference metabolite. This dataframe should have the following named columns, in the following order: "ROI left edge (ppm)", 
#' "ROI right edge (ppm)", "Quantification Mode", "Metabolite", "Quantification Signal", "Chemical shift(ppm)", 
#' "Chemical shift tolerance (ppm)", "Half bandwidth (Hz)", "Multiplicity", "J coupling (Hz)", "Roof effect", 
#' "Quantify", "HMDB_code", and "rowid".
#' 
#' @return A list containing information necessary to create plotly annotations.
#'
ROI_annot_gen <- function(dspedt_refmet_data){
  
  # Create default annot object to add as annotation to plot
  ROI_annot <- list(
    y         = 0,
    xref      = "x",
    yref      = "y",
    arrowhead = 4,
    ay        = 40
  )
  
  ROI_annots <- list()
  for(i in 1:nrow(dspedt_refmet_data)){
    if (dspedt_refmet_data$Quantify[i] == 1){
      ROI_annot[["x"]]         <- dspedt_refmet_data[i,,drop = FALSE]$"Chemical shift(ppm)"
      ROI_annot[["text"]]      <- paste0(sprintf("<b>%s</b>", "ROI: "),
                                         dspedt_refmet_data[i,,drop = FALSE]$"Chemical shift(ppm)", " (",
                                         dspedt_refmet_data[i,,drop = FALSE]$"ROI left edge (ppm)", ", ",
                                         dspedt_refmet_data[i,,drop = FALSE]$"ROI right edge (ppm)", ")")
      ROI_annot[["arrowsize"]] <- dspedt_refmet_data[i,,drop = FALSE]$"Chemical shift tolerance (ppm)"
      ROI_annot[["showarrow"]] <- TRUE
    } else {
      ROI_annot[["x"]]         <- dspedt_refmet_data[i,,drop = FALSE]$"Chemical shift(ppm)"
      ROI_annot[["text"]]      <- ""
      ROI_annot[["arrowsize"]] <- 0
      ROI_annot[["showarrow"]] <- FALSE
    }
    ROI_annots               <- c(ROI_annots, list(ROI_annot))
  }
  
  return(ROI_annots)
}