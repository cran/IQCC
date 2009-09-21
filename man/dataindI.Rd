\name{dataindI}
\alias{dataindI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Hotelling Control Chart Phase I with indiviual observations simulated data. }
\description{
  This function simulate a normal data set to be used in the phase I Hotelling control charts with individual observations.
}
\usage{
dataindI(m, mu, Sigma, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{ The number of samples to be generated. }
  \item{mu}{ The vector with the means of the data to be generated. }
  \item{Sigma}{ The vector with the variance-covariance matrix of the data to be generated. }
  \item{p}{ The dimension. }
}
\details{
It is necessary to give the name "datum" for the output of this function, ex. datum<-dataindI(), because this name will be called in the next functions.
}
\value{
  Return a matrix with the simulated data.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }

\seealso{ \link{dataindII},\link{datasubI},\link{datasubII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
}

