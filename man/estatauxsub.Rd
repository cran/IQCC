\name{estatauxsub}
\alias{estatauxsub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary statistics for the multivariate control chart with sub-group observations. }
\description{
  This function calculate the auxiliary statistics necessary to build the control chart reference lines.
}
\usage{
estatauxsub(datum, m, n, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datum}{ The data set. Should be an array. }
  \item{m}{ The number of sub groups generated previously in datasubI. }
  \item{n}{ The size of each sub group used previously in datasubI. }
  \item{p}{ The dimension used previously in function datasubI. }
}
\details{
To use this function it is necessary to have the information about the datasubI. 
}
\value{
  Return the values of the three statistics: a matrix with the means, mean of the means and mean of the standard deviation.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-datasubI(20,10,mu,Sigma,2)
estat<-estatauxsub(datum,20,10,2)
}
