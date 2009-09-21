\name{PchartII}
\alias{PchartII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Traditional Shewhart p-chart for phase II. }
\description{
  This function builds the traditional Shewhart 3-sigma normal-approx. p chart for phase II.
}
\usage{
PchartII(x, sizes, phat, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{phat}{ The value of the estimate of p. }
  \item{length}{ The length of the data. }
}
\details{
  To use this function it is necessary to have the information about the PchartI. 
}
\value{
  Return the Shewhrat p chart.
}\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{PchartI},\link{PextI},\link{PextII} }
\examples{
data(binomdata)
attach(binomdata)
pchI<-PchartI(x=Di[1:12],sizes=ni[1:12],par=pi[1:12],12)
PchartII(Di[13:25],sizes=ni[13:25],phat=pchI[1],length=13)
}
