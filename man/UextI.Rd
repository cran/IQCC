\name{UextI}
\alias{UextI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exact (improved) u chart for phase I. }
\description{
  This function builds an improved version of the Shewhart u chart where the traditional normal-based control limits are corrected for the non-normality of the u-statistic for phase I.
}
\usage{
UextI(x, sizes, par, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data (counts) to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{par}{ The sample ratios used to estimate the Poisson parameter(lambda). }
  \item{length}{ The length of the data. }
}
\value{
  Return the improved U chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{UextII} }
\examples{
data(moonroof)
attach(moonroof)
UextI(x=yi[1:17],sizes=ni[1:17],par=ui[1:17],17)
}
