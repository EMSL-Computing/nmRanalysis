% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_module_xpmt_data_viz.R
\name{xpmt_data_vizUI}
\alias{xpmt_data_vizUI}
\title{Module: UI elements specific to experimental data visualization}
\usage{
xpmt_data_vizUI(id)
}
\arguments{
\item{id}{A string denoting the namespace id.}
}
\description{
Copyright (C) 2022 Battelle Memorial Institute

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}
\details{
This is the UI component for the module created to handle the visualization of experimental data.
The value provided for 'id' should be identical across xpmt_data_vizUI(), xpmt_data_vizoptionsUI(),
and xpmt_data_vizServer().

This module component provides the UI elements that allow users to:
1) Visualize uploaded experimental data through an interactive plotly figure
2) Visualize experimental metadata through a searchable datatable.
}
