\name{graphsubII}
\alias{graphsubII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sub group Phase II Hotelling Control Chart. }
\description{
  Builds the sub group phase II Hotelling control chart.
}
\usage{
graphsubII(T2II, m, n, j, t, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T2II}{ A vector with the value of T2 statistic for one sample. }
  \item{m}{ The number of sub groups generated previously in datasubI. }
  \item{n}{ The size of each sub group used previously in datasubI. }
  \item{j}{ The index of the current sample. }
  \item{t}{ The maximum value of the x axis. }
  \item{p}{ The dimension used previously in function datasubI. }
}
\details{
  It builds the Hotelling T2 control chart for multivariate normal data (sequential sub-groups/samples of size n>1) to be used in the operational phase ( known as phase II) ; the control limits are based on the F distribution.
}
\value{
 Return a control chart.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{graphindI},\link{graphindII},\link{graphsubI} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-datasubI(20,10,mu,Sigma,2)
estat<-estatauxsub(datum,20,10,2)
datum2<-datasubII(estat,10,p=2)
T2II<-T2subII(datum2,estat,10)
# for the first sample j=1. T2II is a vector with the value of the firts T2 statistic.
graphsubII(T2II,20,10,1,25,2)
}
