\name{datasubI}
\alias{datasubI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling COntrol Chart Phase I with sub-group observations simulated data. }
\description{
  This function simulate a normal data set to be used in the phase I Hoteliing control charts with sub-group observations.
}
\usage{
datasubI(m, n, mu, Sigma, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{ The number of sub groups to be generated. }
  \item{n}{ The size of each sub group. }
  \item{mu}{ The vector with the means of the data to be generated. }
  \item{Sigma}{ The vector with the variance-covariance matrix of the data to be generated. }
  \item{p}{ The dimension. }
}
\details{
  It is necessary to give the name of "datum" for the output of this function, ex. datum<-datasubI(), because this name will be called in the next functions.
}
\value{
  Return an array with the simulated data.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{datasubII},\link{dataindI},\link{dataindII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-datasubI(20,10,mu,Sigma,2)
}
