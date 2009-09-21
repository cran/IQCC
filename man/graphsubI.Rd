\name{graphsubI}
\alias{graphsubI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sub group Phase I Hotelling Control Chart. }
\description{
  Builds the sub group phase I Hotelling control chart.
}
\usage{
graphsubI(estat, T2, m, n, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a matrix with the means, mean of the means and mean of the standard deviation. }
  \item{T2}{ The values of the T2 statistic. Shoul be a matrix. }
  \item{m}{ The number of sub groups generated previously in datasubI. }
  \item{n}{ The size of each sub group used previously in datasubI. }
  \item{p}{ The dimension used previously in function datasubI. }
}
\details{
  It builds the Hotelling T2 control chart for multivariate normal data (m sub-groups /samples of size n>1), used retrospective /validation analysis (phase I); the control limits are based on the F distribution.
}
\value{
  Return a control chart.
}
\references{ Montgomery, D.C.,(2008)."Introduction to Statistical Quality Control". Chapter 11. Wiley }
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{graphindI},\link{graphindII},\link{graphsubII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-datasubI(20,10,mu,Sigma,2)
estat<-estatauxsub(datum,20,10,2)
T2<-T2subI(estat,20,10)
dados<-datasubI(20,10,mu,Sigma,2)
# estat is a list with the auxiliary statistics. T2 is a matrix with the values of the T2 statistic.
graphsubI(estat,T2,20,10,2)
}
