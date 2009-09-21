\name{T2indII}
\alias{T2indII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling T2 Statistic for Individual Phase II. }
\description{
  Calculate the Hotelling T2 statistic for multivariate individual observations at phase II , to be used to build the corresponding control chart.
}
\usage{
T2indII(datum2, estat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datum2}{ The data set for the phase II. Shoul be a vector. }
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a matrix with the means, mean of the means and mean of the standard deviation. }
}
\details{
Before using this function it is necessary to execute the function "estatauxind" (that calculate the auxiliary statistics involved in the T2 formula) and the function "dataindII" (or other way to supply the data).
}
\value{
  Return a vector with the Hotelling T2 statistics.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley. }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{T2indI},\link{estatauxind}, \link{dataindII}, \link{graphindII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
estat<-estatauxind(datum,50,2)
datum2<-dataindII(estat,p=2)
T2II<-T2indII(datum2,estat)
}
