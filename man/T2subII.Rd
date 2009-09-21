\name{T2subII}
\alias{T2subII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling T2 Statistic for sub group Phase II. }
\description{
  Calculate the Hotelling T2 statistic for multivariate sub-group observations at phase II , to be used to build the corresponding control chart.
}
\usage{
T2subII(datum2, estat, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datum2}{ The data set for the phase II. Shoul be a vector. }
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a matrix with the means, mean of the means and mean of the standard deviation. }
  \item{n}{ The size of each sub group used previously in datasubII. }
}
\details{
Before using this function it is necessary to execute the function "estatauxsub"(that calculate the auxiliary statistics involved in the T2 formula) and the function "datasubII" (or other way to supply the data).
}
\value{
  Return a vector with the Hotelling T2 statistics.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley. }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{T2subI},\link{estatauxsub}, \link{datasubII}, \link{graphsubII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-datasubI(20,10,mu,Sigma,2)
estat<-estatauxsub(datum,20,10,2)
datum2<-datasubII(estat,10,p=2)
T2II<-T2subII(datum2,estat,10)
}
