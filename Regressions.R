#' A regression object 
#' 
#' Object of class \code{Regressions} are created by the \code{runReg} and \code{testReg} functions
#'
#' 
#' An object of the class `Regressions' has the following slots:
#' \itemize{
#' \item \code{Y} A depedent variable
#' \item \code{X} Covariates
#' \item \code{coefficients} Coefficients from the output
#' \item \code{R2} R^2 values from the output
#' }
#'
#' @author Taeyong Park: \email{typark99@gmail.com}
#' @aliases Regressions-class initialize,Regressions-method getRegressions,Regressions-method 
#' @rdname Regressions
#' @export
setClass(Class="Regressions", 
         slot = list(
           Y = "matrix", # Input
           X = "matrix", # Input
           output = "list" # Output; This will include coefficients and R.squared for runReg function and tStat, pValue, and sig for testReg function
         ),
         prototype = prototype(
           Y = matrix(nrow=0, ncol=0),
           X = matrix(nrow=0, ncol=0),
           output = list()
         )
)

#' @export
setMethod("initialize", "Regressions", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @rdname Regressions
#' @export 
setGeneric("getRegressions", 
           function(object="Regressions")  {
             standardGeneric("getRegressions")
           }
)

#' @export
setMethod(f="getRegressions", 
          signature="Regressions", 
          function(object){ 
            return(output=object@output)
          }
) 
