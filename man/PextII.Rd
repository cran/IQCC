\name{PextII}
\alias{PextII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exact (improved) p chart for phase II. }
\description{
  This function builds an improved version (non-normal correction) of the p chart for phase II.
}
\usage{
PextII(x, sizes, phat, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{phat}{ The estimate of p. }
  \item{length}{ The length of the data. }
}
\details{
  To use this function it is necessary to have the information about the PextI.
 The non-normal correction considered is based on the Cornish-Fisher formula using cumulants until 3rd order.
}
\value{
  Return an exact p chart.
}
\references{ Chen,G.(1998). An improved p chart through simple adjustment. Journal of Quality Technology, vol 30, pp 142-151. }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{PextI} }
\examples{
data(binomdata)
attach(binomdata)
pexI<-PextI(x=Di[1:12],sizes=ni[1:12],par=pi[1:12],length=12)
PextII(x=Di[1:12],sizes=ni[1:12],phat=pexI[1],length=12)
}
