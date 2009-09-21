\name{updateind}
\alias{updateind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Update the Hotelling Control Chart for individual observations. }
\description{
  This function is used to update the individual phase II control chart with new observations.
}
\usage{
updateind(datum2,estat, T2II, t, j)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datum2}{ The data set for the phase II. Should be a vector. }
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a vector with the means, the variance-covariance estimated matrix  and a matrix with x-xbar vectors. }
  \item{T2II}{ A vector with the value of T2 statistic for one sample. }
  \item{t}{ The maximum value of the x axis. }
  \item{j}{ The index of the current sample. }
}
\details{
 To use this function it is necessary to have the output given by the function T2indII. At every step you should entry with the new data set.}
\value{
  Add the new observation to the current Hoteliing control chart for phase II with individual observations.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{T2indII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
estat<-estatauxind(datum,50,2)
datum2<-dataindII(estat,p=2)
T2II<-T2indII(datum2,estat)
updateind(datum2,estat,T2II,25,2)
}
