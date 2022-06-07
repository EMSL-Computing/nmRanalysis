#' BMSE Associations
#'
#' A dataset containing metabolite data
#'
#' @format A data frame with 2224 rows and 9 columns:
#' \describe{
#'   \item{Entry_ID}{BMSE identifier}
#'   \item{CASno}{CAS number}
#'   \item{Field_strength}{}
#'   \item{Solute}{}
#'   \item{Solvent}{Experimental conditions}
#'   \item{Reference}{}
#'   \item{pH}{Experimental conditions}
#'   \item{Temperature}{Experimental conditions}
#'   \item{Concentration}{Experimental Conditions}
#' }
#'
"bmse_associations"

#' Example e_data
#'
#' A dataset containing NMR spectral data over 4 samples
#'
#' @format A data frame with 262144 rows and 5 columns:
#' \describe{
#'   \item{PPM}{Observed PPMs in NMR spectra}
#'   \item{AA_1x_a_20201209.17.fid.1}{Sample data at each PPM}
#'   \item{AA_1x_a_20201209.18.fid.2}{Sample data at each PPM}
#'   \item{AA_1x_a_20201209.19.fid.3}{Sample data at each PPM}
#'   \item{AA_1x_a_20201209.20.fid.4}{Sample data at each PPM}
#' }
#'
"e_data"


#' Example f_data
#'
#' A dataset containing sample metadata
#'
#' @format A data frame with 4 rows and 2 columns:
#' \describe{
#'   \item{Sample}{Unique sample identifiers}
#'   \item{Experiment}{Experiment number}
#' }
#'
"f_data"


#' Example metabolites
#'
#' A dataset containing example metabolite data
#'
#' @format A data frame with 10 rows and 12 columns:
#' \describe{
#'   \item{ROI left edge (ppm)}{Left boundary of region of interest}
#'   \item{ROI right edge (ppm)}{Right boundary of region of interest}
#'   \item{Quantification Mode}{}
#'   \item{Metabolite}{}
#'   \item{Quantification Signal}{}
#'   \item{Chemical shift(ppm)}{}
#'   \item{Chemical shift tolerance (ppm)}{}
#'   \item{Half bandwidth (Hz)}{}
#'   \item{Multiplicity}{}
#'   \item{J coupling (Hz)}{}
#'   \item{Roof effect}{}
#'   \item{HMDB_code}{}
#' }
#'
"example_metabs"
