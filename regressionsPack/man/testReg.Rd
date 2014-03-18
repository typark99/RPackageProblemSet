\name{testReg}
\alias{Regressions,ANY-method}
\alias{testReg}
\title{Statistical significance test}
\usage{
testReg(Y, X, ...)
}
\arguments{
  \item{Y}{A matrix object; The number of columns is one;
  The number of rows depends on the data}

  \item{X}{A matrix object; The number of rows is the same
  as that of \code{Y}; The number of columns depends on the
  data}
}
\value{
An object of class Regressions containing
\item{output}{Output includes t-statistics, p-value, and
the sign of significance for each covariate}
}
\description{
Tests the significance of the coefficients
}
\examples{
set.seed(0520)
myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1)
myX <- matrix(c(runif(50), runif(50), rnorm(50)), 50, 3)
testReg(Y=myY, X=myX)
}
\author{
Taeyong Park
}
\seealso{
\code{\link{runReg}}
}

