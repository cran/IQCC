\name{tukeyq}
\alias{tukeyq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Tukey Quantile Table }
\description{
  Builds a table with quantiles of the sample relative range distribution.
}
\usage{
tukeyq(alfa, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alfa}{ The probability of type-I error of false alarm , that is equal to 1 minus the confidence level. }
  \item{n}{ The maximum sample size. }
}
\value{
  It is used the fact that the sample relative range distribution is the same as the sample studentized range distribution (tukey distribution) with infinity d.f. in the denominator . It is considered 4 quantiles: alfa/2 , alfa , 1-alfa and 1-alfa/2, for different sample size values .
}
\author{ Daniela R. Recchia, Emanuel P. Barbosa }
\seealso{ \link{const.table},\link{alfa},\link{qtukey} }
\examples{
tukeyq(0.0027,15)
}
