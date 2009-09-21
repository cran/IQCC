\name{T2subI}
\alias{T2subI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling T2 Statistic for sub group Phase I. }
\description{
  Calculate the Hotelling T2 statistic for multivariate sub-group observations at phase I , to be used to build the corresponding control chart.
}
\usage{
T2subI(estat, m, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a matrix with the means, mean of the means and mean of the standard deviation. }
  \item{m}{ The number of sub groups generated previously in datasubI. }
  \item{n}{ The size of each sub group used previously in datasubI. }
}
\details{
  Before using this function it is necessary to execute the function "estatauxsub"(that calculate the auxiliary statistics involved in the T2 formula) and the function "datasubI" (or other way to supply the data).
}
\value{
  Return a vector with the Hotelling T2 statistics.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley. }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{estatauxsub}, \link{datasubI}, \link{graphsubI} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-datasubI(20,10,mu,Sigma,2)
estat<-estatauxsub(datum,20,10,2) 
T2subI(estat,20,10)
}
