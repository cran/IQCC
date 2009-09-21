\name{T2indI}
\alias{T2indI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling T2 Statistic for Individual Phase I. }
\description{
  Calculate the Hotelling T2 statistic for multivariate individual observations at phase I , to be used to build the corresponding control chart.
}
\usage{
T2indI(estat, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a matrix with the means, mean of the means and mean of the standard deviation. }
  \item{m}{ The number of sample generated previously in function dataindI. }
}
\details{
 Before using this function it is necessary to execute the function "estatauxind" (that calculate the auxiliary statistics involved in the T2 formula) and the function "dataindI" (or other way to supply the data).
}
\value{
  Return a vector with the Hotelling T2 statistics.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley. }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{estatauxind},\link{dataindI},\link{graphindI} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
# estat is the list with the values of the auxiliary statistics.
estat<-estatauxind(datum,50,2)
T2indI(estat,50)
}
