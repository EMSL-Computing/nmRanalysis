#' Generate plotly annotations representative of regions of interest for a specific target metabolite
#'
#' @param data A dataframe containing the fitting parameter information. See details.
#'
#' @details The dataframe required by data should contain all of the fitting parameter information for a set of metabolite(s).
#' This dataframe should have the following named columns, in the following order: "ROI left edge (ppm)",
#' "ROI right edge (ppm)", "Quantification Mode", "Metabolite", "Quantification Signal", "Chemical shift(ppm)",
#' "Chemical shift tolerance (ppm)", "Half bandwidth (Hz)", "Multiplicity", "J coupling (Hz)", "Roof effect",
#' "Quantify", "HMDB_code", and "rowid".
#'
#' @return A list containing information necessary to create plotly annotations.
#'
ROI_annot_gen <- function(data){

  # Create default annot object to add as annotation to plot
  ROI_annot <- list(
    y         = 0,
    xref      = "x",
    yref      = "y",
    arrowhead = 4,
    ay        = 40
  )

  ROI_annots <- list()
  for(i in 1:nrow(data)){
    if (data$Quantify[i] == 1){
      ROI_annot[["x"]]         <- data[i,,drop = FALSE]$"Chemical shift(ppm)"
      ROI_annot[["text"]]      <- paste0(sprintf("<b>%s</b>", paste0(data[i,,drop = FALSE]$Metabolite,
                                                                     " [", data[i,,drop = FALSE]$`Quantification Signal`,
                                                                     "]: ")),
                                         data[i,,drop = FALSE]$"Chemical shift(ppm)", " (",
                                         data[i,,drop = FALSE]$"ROI left edge (ppm)", ", ",
                                         data[i,,drop = FALSE]$"ROI right edge (ppm)", ")")
      ROI_annot[["arrowsize"]] <- data[i,,drop = FALSE]$"Chemical shift tolerance (ppm)"
      ROI_annot[["showarrow"]] <- TRUE
    } else {
      ROI_annot[["x"]]         <- data[i,,drop = FALSE]$"Chemical shift(ppm)"
      ROI_annot[["text"]]      <- ""
      ROI_annot[["arrowsize"]] <- 0
      ROI_annot[["showarrow"]] <- FALSE
    }
    ROI_annots               <- c(ROI_annots, list(ROI_annot))
  }

  return(ROI_annots)
}
