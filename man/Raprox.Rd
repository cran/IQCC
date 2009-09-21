\name{Raprox}
\alias{Raprox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Approx. R control chart with warning. }
\description{
  This function builds a traditional 3-sigma R chart with information about the actual false alarm risk.
}
\usage{
Raprox(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{n}{ The sample size. }
}
\details{
  This chart was designed for phase I (at this moment).
}
\value{
  Return a R control chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{Rexact} }
\examples{
data(pistonrings)
attach(pistonrings)
Raprox(pistonrings[1:25,],5)
}
