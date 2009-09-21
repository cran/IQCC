\name{UstdI}
\alias{UstdI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Standardized Shewhart u chart for phase I. }
\description{
  This function builds a standardized  version of the u control chart for phase I.
}
\usage{
UstdI(par, sizes, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{ The sample ratios used to estimate the Poisson parameter(lambda). }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{length}{ The length of the data. }
}
\details{
  The standardized version of the u statistic has zero mean and unitary standard deviation, so that the  center line is zero and the 3-sigma control limits are -3 and +3.
}
\value{
  Return a standardized u chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{UstdII} }
\examples{
data(moonroof)
attach(moonroof)
UstdI(par=ui[1:17],sizes=ni[1:17],length=17)
}
