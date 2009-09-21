\name{removeind}
\alias{removeind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Remove an undesirable observation. }
\description{
  This function removes an undesirable data that might be out of control in your data set. It is used at Hotelling T2 control charts with individual observations for phase I.
}
\usage{
removeind(datum, i)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datum}{ The data set. Should be a matrix.}
  \item{i}{ The index in the matrix of the data to be removed. }
}

\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\value{
  Return the new data set without the observation that was removed.
}
\seealso{ \link{removesub} }
\examples{
mu<-c(5.682,88.22)
Sigma<-symMatrix(c(3.770,-5.495,13.53),2)
datum<-dataindI(50,mu,Sigma,2)
# Removing the observation 43 from the data set "datum" and creating a new one:
datum<-removeind(datum,43)
}

