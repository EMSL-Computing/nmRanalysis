#' PPM Filtering
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
#' Filters out specified PPM ranges from the dataset
#'
#' @param ppmData a list object from containing the data tables `e_data` and `f_data`, created using the function \code{\link{as.ppmData}}
#' @param range a list specifying the minimum and maximum value(s) in the range(s) to be filtered. Multiple ranges may be specified. Ranges are inclusive.
#'
#' @return `ppmData` object with 'filters' attribute including range and `data.frame` with removed rows
#'
#' @examples
#' \dontrun{
#' filter_ppm(mydata, range = list(min = 2, max = 3))
#' filter_ppm(mydata, range = list(min = c(1,3), max = c(2,4)))
#' }
#'
#' @author Allison Thompson, Natalie Winans
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
filter_ppm <- function(ppmData, range){

  # initial checks
  if(!inherits(ppmData, "ppmData")){
    stop("'ppmData' must be object created using the function 'as.ppmData'")
  }
  if(!is.list(range)){
    stop("Range must be a list containing the minimum and maximum value in the range to be filtered")
  }
  if(!("min" %in% names(range))){
    stop("Must specify minimum value in the range(s)")
  }
  if(!("max" %in% names(range))){
    stop("Must specify maximum value in the range(s)")
  }
  if(!(length(range$min) == length(range$max))){
    stop("Number of range minima must equal number of range maxima")
  }

  #if(!is.null(attr(ppmData, "filters")$ppm)) stop("A PPM filter has already been applied to this dataset")

  # get ppm column name
  ppm <- attr(ppmData, "cnames")$edata_cname

  # PPM to remove
  range_length <- length(range$min)

  removed_data <- list()
  for (i in 1:range_length) {
    removed_data[[i]] <- ppmData$e_data %>%
      dplyr::filter(.data[[ppm]] >= range$min[i] & .data[[ppm]] <= range$max[i])
  }

  filtered_data <- do.call(rbind, removed_data) %>%
    dplyr::distinct()

  # PPM to keep
  new_edata <- suppressMessages(dplyr::anti_join(ppmData$e_data, filtered_data))

  res <- list("e_data" = new_edata, "f_data" = ppmData$f_data)

  attributes(res) <- attributes(ppmData)

  if(length(attr(ppmData, "filters")) == 0) {
    attr(res, "filters") <- list(list("range" = range, "filtered_data" = filtered_data))
  } else {
    n_filters <- length(attr(res, "filters"))
    attr(res, "filters")[[n_filters + 1]] <- list("range" = range, "filtered_data" = filtered_data)
  }

  return(res)
}
