\name{PstdI}
\alias{PstdI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Standardized p chart for phase I. }
\description{
  This function builds a standardized version of the p chart for phase I.
}
\usage{
PstdI(par, sizes, length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{ The data used to estimate p. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
  \item{length}{ The length of the data (number of samples or groups). }
}
\details{
  The original binomial counts are standardized (mean zero and sd one) and the normal approximated control limits become -3 and +3.
}
\value{
  Return a standardized p chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{PstdII} }
\examples{
data(binomdata)
attach(binomdata)
PstdI(par=pi[1:12],sizes=ni[1:12],length=12)
}
