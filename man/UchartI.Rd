\name{UchartI}
\alias{UchartI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Traditional Shewhart u chart for phase I. }
\description{
  This function builds the traditional Shewhart u chart based on normal approx. for the Poisson-based count data statistic . It is designed for phase I.
}
\usage{
UchartI(x, sizes, par, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{par}{ The sample ratios used to estimate the Poisson parameter(lambda). }
  \item{length}{ The length of the data. }
}
\details{
  It is important to note that the normal approx. involved is valid only for n.p large. For small n.p , it should be used an "improved u chart" (with non-normal correction) given by the function "UextI" for phase-I or "UextII" for phase II .
}
\value{
  Return a traditional U control chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{UchartII} }
\examples{
data(moonroof)
attach(moonroof)
UchartI(par=ui[1:17],x=yi[1:17],sizes=ni[1:17],17)
}

