#' Negation of \%in\% operator
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

