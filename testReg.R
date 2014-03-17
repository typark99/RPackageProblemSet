#' Statistical significance test
#'
#' Tests the significance of the coefficients
#'
#' @param Y A matrix object; The number of columns is one; The number of rows depends on the data 
#' @param X A matrix object; The number of rows is the same as that of \code{Y}; The number of columns depends on the data
#'  
#' @return An object of class Regressions containing
#'  \item{output}{Output includes t-statistics, p-value, and the sign of significance for each covariate}
#' @author Taeyong Park
#' @note
#' @examples
#' set.seed(0520)
#' myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1) 
#' myX <- matrix(c(runif(50), runif(50), rnorm(50)), 50, 3) 
#' testReg(myX, myY)
#' @seealso \code{\link{runReg}}
#' @rdname testReg
#' @aliases Regressions,ANY-method
#' @export
setGeneric(name="testReg",
           def=function(Y, X, ...)
           {standardGeneric("testReg")}
)

#' @export
setMethod(f="testReg",
          definition=function(Y, X, ...){
            Z <- list()  # Z will contain every combination of X
            tStatList <- list() # This will be transformed to the form of matrix
            tStat <- matrix(NA, ncol(X), ncol(X)+1)
            pValueList <- list() # This will be transformed to the form of matrix
            pValue <- matrix(NA, ncol(X), ncol(X)+1)
            sigList <- list() # This will be transformed to the form of matrix
            sig <- matrix("Not Signigicant", ncol(X), ncol(X)+1) # This will report "significance" of each coefficient
            alpha = 0.05
            for (i in 2:ncol(X)){ # The first elements are not looped to make it easy to create every combination of the covariates
              Z[[1]] <- X[,1]  
              Z[[i]] <- cbind(X[,i],Z[[i-1]]) # This ensures that Z will contain every combination of the covariates
              tStatList[[1]] <- summary(lm(Y ~ Z[[1]]))$coef[,3]
              tStatList[[i]] <- summary(lm(Y ~ Z[[i]]))$coef[,3]
              tStat[1,] <- c(tStatList[[1]], rep(NA, (ncol(X)+1)-length(tStatList[[1]]))) # Now, we want to transform tStatList to the form of matrix
              tStat[i,] <- c(tStatList[[i]], rep(NA, (ncol(X)+1)-length(tStatList[[i]]))) 
              pValueList[[1]] <- summary(lm(Y ~ Z[[1]]))$coef[,4]
              pValueList[[i]] <- summary(lm(Y ~ Z[[i]]))$coef[,4]
              pValue[1,] <- c(pValueList[[1]], rep(NA, (ncol(X)+1)-length(pValueList[[1]]))) # Now, we want to transform pValueList to the form of matrix
              pValue[i,] <- c(pValueList[[i]], rep(NA, (ncol(X)+1)-length(pValueList[[i]]))) 
              sigList[[1]] <- ifelse(pValueList[[1]]>=alpha, "Not Significant", "Significant") # If pValue for the coefficient is larger than the significance level (alpha), then it returns "Not Significant."
              sigList[[i]] <- ifelse(pValueList[[i]]>=alpha, "Not Significant", "Significant")
              sig[1,] <- c(sigList[[1]], rep(NA, (ncol(X)+1)-length(sigList[[1]]))) # Now, we want to transform sigList to the form of matrix
              sig[i,] <- c(sigList[[i]], rep(NA, (ncol(X)+1)-length(sigList[[i]])))
            }
            output <- list(tStat, pValue, sig)
            names(output) <- c("tStat", "pValue", "significance")
            return((new("Regressions", Y=Y, X=X, output=output)))
          }
)

getRegressions(testReg(myY, myX))
