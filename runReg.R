#' Running regressions and reporting results
#'
#' Runs regressions for every combination of the input covariates
#'
#' @param Y A matrix object; The number of columns is one; The number of rows depends on the data 
#' @param X A matrix object; The number of rows is the same as that of \code{Y}; The number of columns depends on the data
#'  
#' @return An object of class Regressions containing
#'  \item{coefficients}{The coefficients for the covariates from a regression}
#'  \item{R2}{Model fit}
#' @author Taeyong Park
#' @note
#' @examples
#' set.seed(0520)
#' myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1) 
#' myX <- matrix(c(runif(50), runif(50), rnorm(50)), 50, 3) 
#' runReg(myX, myY)
#' @seealso \code{\link{testReg}}
#' @rdname runReg
#' @aliases Regressions,ANY-method
#' @export
setGeneric(name="runReg",
           def=function(Y, X, ...)
           {standardGeneric("runReg")}
)

#' @export
setMethod(f="runReg",
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
            output <- list(coefficients, R2)
            names(output) <- c("coefficients", "R.squared")
            return((new("Regressions", Y=Y, X=X, output=output)))
          }
)

getRegressions(runReg(myY, myX))
