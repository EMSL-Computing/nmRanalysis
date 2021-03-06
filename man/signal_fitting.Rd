% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rD_signal_fitting.R
\name{signal_fitting}
\alias{signal_fitting}
\title{rDolphin: signal_fitting}
\usage{
signal_fitting(parS, Xdata, multiplicities, roof_effect, roof_effect2, freq)
}
\arguments{
\item{parS}{}

\item{Xdata}{}

\item{multiplicities}{}

\item{roof_effect}{}

\item{freq}{}
}
\description{
rDolphin: signal_fitting
}
\details{
This function was forked directly from rDolphin (https://github.com/danielcanueto/rDolphin/tree/master/R). This is the function
that fits the lineshape patterns according to the specified fitting parameters. Modifications have been made to the original function to allow for
the fitting of double-doublets. Generally speaking, this involves the addition of new fields (a second J-coupling field and second
roof effect field) containing parameters necessary to fit this new pattern and new fitting equations that make use of these additional
fields.
}
