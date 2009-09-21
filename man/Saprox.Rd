\name{Saprox}
\alias{Saprox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Approx. S control chart. }
\description{
  This function builds a traditional 3-sigma S chart.
}
\usage{
Saprox(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
}
\value{
  Return a S control chart.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{Sexact} }
\examples{
data(softdrink)
Saprox(softdrink)
}
