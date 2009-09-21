\name{updatesub}
\alias{updatesub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Update the Hotelling Control Chart for sub-group observations. }
\description{
  This function is used to update the sub-group phase II control chart with new observations.
}
\usage{
updatesub(datum2, estat, T2II, n, t, j)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datum2}{ The data set for the phase II. Shoul be a vector. }
  \item{estat}{ The values of the auxiliary statistics. Should be a list with a vector with the mean of the mean vectors, a matrix with the average of the variance-covariance matrices and a matrix with the means. }
  \item{T2II}{ A vector with the value of T2 statistic for one sample. }
  \item{n}{ The sub group size. }
  \item{t}{ The maximum value of the x axis. }
  \item{j}{ The index of the current sample. }
}
\details{
 To use this function it is necessary to have the output given by the function T2subII. At every step you should entry with the new data set.
}
\value{
  Add the new observation to the current Hoteliing control chart for phase II with sub-group observations.
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{T2subII} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-datasubI(20,10,mu,Sigma,2)
estat<-estatauxsub(datum,20,10,2)
datum2<-datasubII(estat,10,p=2)
T2II<-T2subII(datum2,estat,10)
updatesub(datum2,estat,T2II,10,25,2)
}
