\docType{class}
\name{Regressions-class}
\alias{MoreRegressions-class}
\alias{Regressions-class}
\alias{getMoreRegressions,MoreRegressions-method}
\alias{getRegressions}
\alias{getRegressions,Regressions-method}
\alias{initialize,MoreRegressions-method}
\alias{initialize,Regressions-method}
\title{A regression object}
\usage{
getRegressions(object = "Regressions")
}
\description{
Object of class \code{Regressions} are created by the
\code{runReg} and \code{testReg} functions

Object of class \code{MoreRegressions} as created by the
\code{moreReg} functions
}
\details{
An object of the class `Regressions' has the following
slots: \itemize{ \item \code{Y} A depedent variable \item
\code{X} Covariates \item \code{output} Output includes
coefficients, R^2 values, tStat values, and pValue }

An object of the class `MoreRegressions' has the following
slots: \itemize{ \item \code{Y} A depedent variable \item
\code{X} Covariates \item \code{output} Output includes
either coefficients and R^2 values or tStat and pValue
\item \code{meanR2} The mean of R^2 values from a
regression }
}
\author{
Taeyong Park: \email{typark99@gmail.com}

Taeyong Park: \email{typark99@gmail.com}
}

