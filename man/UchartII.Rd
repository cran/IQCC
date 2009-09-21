\name{UchartII}
\alias{UchartII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Traditional Shewhart u chart for phase II. }
\description{
  This function builds the traditional Shewhart u chart based on normal approx. for the Poisson-based count data statistic . It is designed for phase II.
}
\usage{
UchartII(x, sizes, lambda, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{lambda}{ The estimate of lambda. }
  \item{length}{ The length of the data. }
}
\details{
  To use this function it is necessary to have the information about the UchartI. 
It is important to note that the normal approx. involved is valid only for n.p large. For small n.p , it should be used an "improved u chart" (with non-normal correction) given by the function "UextI" for phase-I or "UextII" for phase II.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{UchartI},\link{UextII} }
\examples{
data(moonroof)
attach(moonroof)
uchI<-UchartI(x=yi[1:17],sizes=ni[1:17],par=ui[1:17],17)
UchartII(yi[18:34],sizes=ni[18:34],lambda=uchI[1],length=17)
}
