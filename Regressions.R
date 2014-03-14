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
           Y = "matrix",
           X = "matrix",
           coefficients = "matrix",
           R2 = "numeric"
         ),
         prototype = prototype(
           Y = matrix(),
           X = matrix(),
           coefficients = matrix(),
           R2 = numeric()
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
setMethod("getRegressions", "Regressions",
          function(object){ 
            return(list(coefficients=object@coefficients,R2=object@R2))
          }
            ) 