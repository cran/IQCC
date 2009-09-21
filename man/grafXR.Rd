\name{grafXR}
\alias{grafXR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ X-bar and R control charts }
\description{
  This function builds the X-bar and R control charts in the same window.
}
\usage{
grafXR(x, sizes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The data to be plotted. }
  \item{sizes}{ A value or a vector of values specifying the sample sizes associated with each group. }
}
\value{
  Return the two control charts.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa. }
\examples{
data(pistonrings)
attach(pistonrings)
grafXR(pistonrings[1:25,])
}

