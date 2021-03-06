% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rD_fittingloop_bg.R
\name{fittingloop_bg}
\alias{fittingloop_bg}
\title{rDolphin: fittingloop_bg}
\usage{
fittingloop_bg(FeaturesMatrix, Xdata, Ydata, program_parameters)
}
\arguments{
\item{FeaturesMatrix}{}

\item{Xdata}{}

\item{Ydata}{}

\item{program_parameters}{}
}
\description{
rDolphin: fittingloop_bg
}
\details{
This function was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This function is responsible
for fitting background signals to the observed data. Modifications have been made to the original function to allow for
the fitting of double-doublets. Generally speaking, this involves the addition of new fields (a second J-coupling field and second
roof effect field) containing parameters necessary to fit this new pattern.
}
