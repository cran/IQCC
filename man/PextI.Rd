\name{PextI}
\alias{PextI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Exact (improved) p chart for phase I. }
\description{
  This function builds an improved version (non-normal correction) of the p chart for phase I.
}
\usage{
PextI(x, sizes,  par, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{par}{ The data used to estimate the phat (x/sizes). This data is used only in this phase. }
  \item{length}{ The length of the data. }
}
\details{
  The non-normal correction considered is based on the Cornish-Fisher formula using cumulants until 3rd order.
}
\value{
  Return an exact p chart.
}
\references{ Chen,G.(1998). An improved p chart through simple adjustment. Journal of Quality Technology, vol 30, pp 142-151.
 }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{PextII} }
\examples{
data(binomdata)
attach(binomdata)
PextI(par=pi[1:12],x=Di[1:12],sizes=ni[1:12],length=12)
}

