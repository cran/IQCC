\name{graphindII}
\alias{graphindII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Individual Phase II Hotelling Control Chart. }
\description{
  Builds the individual phase II Hoteling control chart.
}
\usage{
graphindII(T2II, m, j, t, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T2II}{ A vector with the value of T2 statistic for one sample. }
  \item{m}{ The number of samples generated previously in function dataindI. }
  \item{j}{ The index of the current sample. }
  \item{t}{ The maximum value of the x axis. }
  \item{p}{ The dimension used previously in function dataindI. }
}
\details{
  It builds the Hotelling T2 control chart for multivariate normal data (individual observations taken sequentialy); the control limits are based on the F distribution, and the covariance matrix estimator is based on the sucessive difference method.
}
\value{
  Return a control chart.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{graphindI},\link{graphsubI},\link{graphsubII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
estat<-estatauxind(datum,50,2)
datum2<-dataindII(estat,p=2)
T2II<-T2indII(datum2,estat)
# for the first sample j=1; T2II is a vector with the value of the firts T2 statistic.
graphindII(T2II,50,1,25,2)
}

