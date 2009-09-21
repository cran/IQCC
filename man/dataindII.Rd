\name{dataindII}
\alias{dataindII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling Control Chart Phase II with individual observations simulated data. }
\description{
  This function simulate a normal data set to be used in the phase II Hotelling control charts with individual observations.
}
\usage{
dataindII(estat, p, delta = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a matrix with the means, mean of the means and mean of the standard deviation. }
  \item{delta}{ A value to be added on the vector of means. }
  \item{p}{ The dimension. }
}
\details{
To use this function it is necessary to have the information about the phase I given by the functions dataindI and estatauxind.
}
\value{
  Return a matrix with the simulated data.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{dataindI},\link{datasubI},\link{datasubII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
#estat is the list with the values of the auxiliary statistics.
estat<-estatauxind(datum,50,2)
datum2<-dataindII(estat,p=2)
}
