#' Remove PPM Filter
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
#' Removes a previously applied PPM filter by restoring removed rows and clearing 'filters' attribute
#'
#' @param ppmData a `ppmData` object, consisting of a list containing the data tables `e_data` and `f_data`, upon which a PPM filter has previously been applied using the function \code{\link{filter_ppm}}
#' @param filters integer vector specifying which PPM filter(s) to remove. Can be accessed using \code{\link{attr}} function.
#'
#' @return a `ppmData` object with previously removed rows restored to `e_data` and PPM filter attributes set to `NULL`
#'
#' @examples
#' \dontrun{
#' remove_filter_ppm(mydata, filters = c(1,2))
#' }
#'
#' @author Natalie Winans
#'
#' @export
#'
#' @importFrom plyr .
#'
remove_filter_ppm <- function(ppmData, filters) {

  if(!inherits(ppmData, "ppmData")){
    stop("'ppmData' must be 'ppmData' object created using the function 'as.ppmData'")
  }
  if(length(attr(ppmData, "filters")) == 0){
    stop("No PPM filter has been applied to ppmData object")
  }
  if(!(length(attr(ppmData, "filters")) >= length(filters))){
    stop("Length of 'filters' parameter greater than the number of filters applied to 'ppmData'")
  }

  filtered_data <- list()
  for (i in 1:length(filters)){
    filtered_data[[i]] <- attr(ppmData, "filters")[[filters[i]]]$filtered_data
  }

  new_edata <- do.call(rbind, filtered_data) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(., ppmData$e_data)

  res <- list("e_data" = new_edata,
              "f_data" = ppmData$f_data)

  attributes(res) <- attributes(ppmData)

  for (i in sort(filters, decreasing = TRUE)) {
    attr(res, "filters")[[i]] <- NULL
  }

  return(res)
}
