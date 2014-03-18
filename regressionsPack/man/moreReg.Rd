\name{moreReg}
\alias{MoreRegressions,ANY-method}
\alias{moreReg}
\title{Adding the mean of R.squared to superclass Regressions}
\usage{
moreReg(Y, X, ...)
}
\arguments{
  \item{Y}{A matrix object; The number of columns is one;
  The number of rows depends on the data}

  \item{X}{A matrix object; The number of rows is the same
  as that of \code{Y}; The number of columns depends on the
  data}
}
\value{
An object of class MoreRegressions containing
\item{meanR2}{The mean of R.squared}
}
\description{
Computes the mean of R.squared and adds it to the ourput of
coefficients and R.squared
}
\examples{
set.seed(0520)
myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1)
myX <- matrix(c(runif(50), runif(50), rnorm(50)), 50, 3)
moreReg(Y=myY, X=myX)
}
\author{
Taeyong Park
}
\seealso{
\code{\link{runReg}} \code{\link{testReg}}
}

