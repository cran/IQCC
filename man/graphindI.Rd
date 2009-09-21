\name{graphindI}
\alias{graphindI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Individual Phase I Hotelling Control Chart. }
\description{
  Builds the individual phase I Hotelling control chart.
}
\usage{
graphindI(T2, m, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T2}{ The values of the T2 statistic. Shoul be a matrix. }
  \item{m}{ The number of samples generated previously in function dataindI. }
  \item{p}{ The dimension used previously in function dataindI. }
}
\value{
  Return a control chart.
}
\details{
  It builds the Hotelling T2 control chart for multivariate normal data (individual observations : m samples of size n=1) used in retrospective / validation analysis (phase I) ; the control limits are based on a beta distribution, and the covariance matrix estimator is based on the sucessive difference method.
}
\references{
 Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{graphindII},\link{graphsubI},\link{graphsubII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
estat<-estatauxind(datum,50,2)
# T2 is the matrix with the values of T2 statistic.
T2<-T2indI(estat,50)
graphindI(T2,50,2)
}
