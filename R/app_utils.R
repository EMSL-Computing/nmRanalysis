#' Negation of \%in\% operator
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
#' @param a a vector
#' @param b a vector
#'
#' @details This function determines which of the elements of "a" are not contained within "b"
#'
#' @return a logical-valued vector with length identical to vector "a".
#'
#'
'%ni%' <- function(a, b){
  !(a %in% b)
}


#' Load experimental NMR data, experimental NMR metadata, or target metabolite data
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
#' @param path The filepath of the data of interest.
#' @param dataset The type of data to be uploaded. Users may specify either "experiment", "experiment_metadata",
#' or "metabolites" depending on whether experimental data, experimental metadata, or target metabolite data are being uploaded, respectively.
#'
#' @details If users are uploading experimental data, it is assumed that the data are contained within a .csv file and
#' that these data are organized such that the first column contains the PPM values of the experimental spectra and
#' each subsequent column contains intensity values for a given sample at the corresponding PPM value.
#'
#' If users are uploading experimental metadata, it is assumed that the data are contained within a .csv file, that the first column
#' of these data contain the sample name(s), and the second column contains an experimental label.
#'
#' Last, uploaded target metabolite data is assumed to be contained within an .xlsx file that contains a column of CAS numbers
#' corresponding to the metabolites of interest.
#'
#' @return A dataframe containing the uploaded data.
#'
#' @importFrom magrittr %>%
#'
load_file <- function(path, dataset){

  if(dataset == "experiment"){
    data              <- utils::read.csv(path) %>% as.data.frame()
    colnames(data)[1] <- "PPM"

  } else if(dataset == "experiment_metadata"){
    data              <- utils::read.csv(path) %>% as.data.frame()
    colnames(data)[1] <- "Sample"
    data$Sample       <- gsub("#", ".", data$Sample)

  } else if(dataset == "metabolites"){
    data           <- readxl::read_excel(path)
    colnames(data) <- data[2,]
    data           <- stats::na.omit(data)
  }

  return(data)
}

