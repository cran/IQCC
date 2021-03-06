% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cchart.u.R
\name{cchart.u}
\alias{cchart.u}
\title{u-chart}
\usage{
cchart.u(x1 = NULL, n1 = NULL, type = "norm", u1 = NULL, x2 = NULL,
  n2 = NULL, lambda = NULL, u2 = NULL)
}
\arguments{
\item{x1}{The phase I data that will be plotted (if it is a phase I chart).}

\item{n1}{A value or a vector of values specifying the sample sizes
associated with each group for the phase I data.}

\item{type}{The type of u-chart to be plotted. The options are "norm"
(traditional Shewhart u-chart), "CF" (improved u-chart) and "std"
(standardized u-chart). If not specified, a Shewhart u-chart will be
plotted.}

\item{u1}{The sample ratios used to estimate the Poisson parameter (lambda).
(x1 / n1).}

\item{x2}{The phase II data that will be plotted in a phase II chart.}

\item{n2}{A value or a vector of values specifying the sample sizes
associated with each group for the phase II data.}

\item{lambda}{The estimate of lambda.}

\item{u2}{The sample ratios of the phase II data (x2 / n2).}
}
\value{
Returns a u-chart.
}
\description{
This function builds a u-chart for the Poisson-based count data statistic.
}
\details{
For a phase I u-chart, n1 must be specified and either x1 or u1.  For a
phase II u-chart, n2 must be specified, plus x2 or u2 and either phat, x1
and n1, or u1 and n1.  It is important to note that the normal approximation
used in the Shewhart u-chart is valid only for n*u large. For small n*p , it
should be used an "improved u chart" (with non-normal correction) given by
using the argument "CF".
}
\examples{

data(moonroof)
attach(moonroof)
cchart.u(x1 = yi[1:17], n1 = ni[1:17])
cchart.u(x1 = yi[1:17], n1 = ni[1:17], type = "CF", x2 = yi[18:34], n2 = ni[18:34])
cchart.u(type = "std", u2 = ui[18:34], n2 = ni[18:34], lambda = 1.4)

}
\author{
Daniela R. Recchia, Emanuel P. Barbosa
}
