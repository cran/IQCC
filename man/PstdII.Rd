\name{PstdII}
\alias{PstdII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Standardized p chart for phase II. }
\description{
  This function builds a standardized version of the p chart for phase II.
}
\usage{
PstdII(par, sizes, phat, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{ The data used to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{phat}{ The estimate of p. }
  \item{length}{ The length of the data. }
}
\details{
  To use this function it is necessary to have the information about the PstdI. 
 The original binomial counts are standardized (mean zero and sd one) and the normal approximated control limits become -3 and +3.
}
\value{
  Return a standardized p chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{PstdI} }
\examples{
data(binomdata)
attach(binomdata)
pstI<-PstdI(par=pi[1:12],sizes=ni[1:12],length=12)
PstdII(par=pi[13:25],sizes=ni[13:25],phat=pstI[1],length=13)
}
