\name{estatauxind}
\alias{estatauxind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Auxiliary statistics for the multivariate control charts with individual observations. }
\description{
  This function calculate the auxiliary statistics necessary to build the control chart reference lines.
}
\usage{
estatauxind(datum, m, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datum}{ The data set. Should be a matrix. }
  \item{m}{ The number of sample generated previously in function dataindI. }
  \item{p}{ The dimension used previously in function dataindI. }
}
\details{
To use this function it is necessary to have the information about the dataindI. 
}
\value{
 Return the values of the three statistics: a matrix with the means, mean of the means and mean of the standard deviation. 
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
estat<-estatauxind(datum,50,2)
}




