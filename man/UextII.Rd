\name{UextII}
\alias{UextII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exact (improved) u chart for phase II. }
\description{
  This function builds an improved version of the Shewhart u chart where the traditional normal-based control limits are corrected for the non-normality of the u-statistic for phase II.
}
\usage{
UextII(x, sizes, lambda, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{lambda}{ The estimate of lambda. }
  \item{length}{ The length of the data. }
}
\details{
  To use this function it is necessary to have the information about the UextI. 
}
\value{
  Return the improved U chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{UextI} }
\examples{
data(moonroof)
attach(moonroof)
uexI<-UextI(x=yi[1:17],sizes=ni[1:17],par=ui[1:17],17)
UextII(x=yi[18:34],sizes=ni[18:34],lambda=uexI[1],length=17)
}
