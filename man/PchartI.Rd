\name{PchartI}
\alias{PchartI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Traditional Shewhart p-chart for phase I. }
\description{
  This function builds the traditional Shewhart 3-sigma normal-approx. p chart for phase I (retrospective / validation analysis).
}
\usage{
PchartI(x, sizes, par, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group.  }
  \item{par}{ The data used to estimate the phat (x/sizes). This data is used only in this phase. }
  \item{length}{ The length of the data. }
}
\details{
 This chart is based on normal-approx. and should be used only for large values of np or ni.pi (np>6).
}
\value{
  Return the Shewhrat p chart.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{PchartII},\link{PextI},\link{PextII} }
\examples{
data(binomdata)
attach(binomdata)
PchartI(par=pi[1:12],x=Di[1:12],sizes=ni[1:12],12)
}
