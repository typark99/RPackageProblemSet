#' Adding the mean of R.squared to superclass Regressions
#'
#' Computes the mean of R.squared and adds it to the ourput of coefficients and R.squared
#'
#' @param Y A matrix object; The number of columns is one; The number of rows depends on the data 
#' @param X A matrix object; The number of rows is the same as that of \code{Y}; The number of columns depends on the data
#'  
#' @return An object of class zMoreRegressions containing
#'  \item{meanR2}{The mean of R.squared}
#' @author Taeyong Park
#' @note
#' @examples
#' set.seed(0520)
#' myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1) 
#' myX <- matrix(c(runif(50), runif(50), rnorm(50)), 50, 3) 
#' moreReg(myX, myY)
#' @seealso \code{\link{runReg}} \code{\link{testReg}}
#' @rdname moreReg
#' @aliases zAllRegressions,ANY-method
#' @export
setGeneric(name="moreReg",
           def=function(Y, X, ...)
           {standardGeneric("moreReg")}
)

#' @export
setMethod(f="moreReg",
          definition=function(Y, X, ...){
            Z <- list()  # Z will contain every combination of X
            coefficientsList <- list() # This will be transformed to coefficients which is a matrix
            R2 <- numeric() 
            coefficients<-matrix(NA, ncol(X), ncol(X)+1) # We want the output of coefficients as a matrix
            for (i in 2:ncol(X)){ # The first elements are not looped to make it easy to create every combination of the covariates
              Z[[1]] <- X[,1]  
              Z[[i]] <- cbind(X[,i],Z[[i-1]]) # This ensures that Z will contain every combination of the covariates
              coefficientsList[[1]] <- summary(lm(Y ~ Z[[1]]))$coef[,1] # The first element for coefficient is not looped 
              coefficientsList[[i]] <- summary(lm(Y ~ Z[[i]]))$coef[,1]
              coefficients[1,] <- c(coefficientsList[[1]], rep(NA, (ncol(X)+1)-length(coefficientsList[[1]]))) # Now, we want to transform coef to the form of matrix
              coefficients[i,] <- c(coefficientsList[[i]], rep(NA, (ncol(X)+1)-length(coefficientsList[[i]]))) # An empty cell will be expressed as "NA"
              R2[1] <- summary(lm(Y ~ Z[[1]]))$r.squared
              R2[i] <- summary(lm(Y ~ Z[[i]]))$r.squared
            }
            meanR2 <- mean(R2)
            output <- list(coefficients, R2)
            names(output) <- c("coefficients", "R.squared")
            return((new("zMoreRegressions", output=output, meanR2=meanR2)))
          }
)

getMoreRegressions(moreReg(myY, myX))
