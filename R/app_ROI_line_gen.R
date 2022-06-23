#' Generate plotly shapes (lines) representative of regions of interest for a specific target metabolite
#'
#' @param data A dataframe containing the fitting parameter information. See details.
#'
#' @details The dataframe required by data should contain all of the fitting parameter information for a set of metabolite(s).
#'  This dataframe should have the following named columns, in the following order: "ROI left edge (ppm)",
#' "ROI right edge (ppm)", "Quantification Mode", "Metabolite", "Quantification Signal", "Chemical shift(ppm)",
#' "Chemical shift tolerance (ppm)", "Half bandwidth (Hz)", "Multiplicity", "J coupling (Hz)", "Roof effect",
#' "Quantify", "HMDB_code", and "rowid".
#'
#' @return A list containing information necessary to create plotly shapes (lines).
#'
ROI_line_gen <- function(data){

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
  for(i in 1:nrow(data)){

    if (data$Quantify[i] == 1){

      ROI_line[["x0"]]        <- data[i,,drop = FALSE]$"ROI left edge (ppm)"
      ROI_line[["x1"]]        <- data[i,,drop = FALSE]$"ROI right edge (ppm)"
      ROI_line[c("y0", "y1")] <- 0

    } else {

      ROI_line[["x0"]]        <- data[i,,drop = FALSE]$"Chemical shift(ppm)"
      ROI_line[["x1"]]        <- data[i,,drop = FALSE]$"Chemical shift(ppm)"
      ROI_line[c("y0", "y1")] <- 0

    }
    ROI_lines               <- c(ROI_lines, list(ROI_line))
  }

  return(ROI_lines)
}
