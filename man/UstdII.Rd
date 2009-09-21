\name{UstdII}
\alias{UstdII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Standardized Shewhart u chart for phase II. }
\description{
  This function builds a standardized  version of the u control chart for phase II.
}
\usage{
UstdII(par, sizes, lambda, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{ The data used to calculate the standardization. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{lambda}{ The estimate of lambda. }
  \item{length}{ The length of the data. }
}
\details{
  The standardized version of the u statistic has zero mean and unitary standard deviation, so that the  center line is zero and the 3-sigma control limits are -3 and +3.
}
\value{
  Return a standardized u chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{UstdI} }
\examples{
data(moonroof)
attach(moonroof)
ustI<-UstdI(par=ui[1:17],sizes=ni[1:17],length=17)
UstdII(par=ui[18:34],sizes=ni[18:34],lambda=ustI[1],length=17)
}
