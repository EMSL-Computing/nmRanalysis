#' Example e_data
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
