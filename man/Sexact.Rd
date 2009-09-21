\name{Sexact}
\alias{Sexact}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exact S control chart. }
\description{
  This function builds a S control chart with exact (probability) limits.
}
\usage{
Sexact(x, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{m}{ The sample sizes. }
}
\details{
  The exact limits are the alfa/2 and 1-alfa/2 quantiles of the S distribution which is proportional to the square root of a chi-square distribution.
}
\value{
  Return a S control chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{Saprox} }
\examples{
data(softdrink)
Sexact(softdrink,10)
}
