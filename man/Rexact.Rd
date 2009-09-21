\name{Rexact}
\alias{Rexact}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exact R control chart. }
\description{
  This function builds a R control chart with exact (probability) limits.
}
\usage{
Rexact(x, y, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{y}{ The data used in phase I to estimate the standard deviation. }
  \item{m}{ The sample size. }
}
\details{
  The exact limits are the alfa/2 and 1-alfa/2 quantiles of the R distribution that are calculated as estimated process sd times the quantiles of the relative range (W=R/sigma) distribution.
}
\value{
  Return a R control chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{Raprox} }
\examples{
data(pistonrings)
attach(pistonrings)
Rexact(pistonrings[26:40,],pistonrings[1:25,],5)
}
