#' A regression object 
#' 
#' Object of class \code{zMoreRegressions} as created by the \code{moreReg} functions
#'
#' 
#' An object of the class `zMoreRegressions' has the following slots:
#' \itemize{
#' \item \code{Y} A depedent variable
#' \item \code{X} Covariates
#' \item \code{output} Output includes either coefficients and R^2 values or tStat and pValue
#' \item \code{meanR2} The mean of R^2 values from a regression
#' }
#'
#' @author Taeyong Park: \email{typark99@gmail.com}
#' @aliases zMoreRegressions-class initialize,zMoreRegressions-method getMoreRegressions,zMoreRegressions-method 
#' @rdname Regressions
#' @export
setClass(Class="zMoreRegressions", 
         contains="Regressions",
         slot = list(
           Y = "matrix",
           X = "matrix",
           output = "list",
           meanR2 = "numeric"
         ),
         prototype = prototype(
           Y = matrix(nrow=0, ncol=0),
           X = matrix(nrow=0, ncol=0),
           output = list(),
           meanR2 = numeric()
         )
)

#' @export
setMethod("initialize", "zMoreRegressions", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export 
setGeneric("getMoreRegressions", 
           function(object="zMoreRegressions")  {
             standardGeneric("getMoreRegressions")
           }
)

#' @export
setMethod(f="getMoreRegressions", 
          signature="zMoreRegressions", 
          function(object){ 
            return(list(output=object@output,
                        meanR2=object@meanR2))
          }
)


##
setAs(from="Regressions", to="zMoreRegressions",
      def=function(from){
        new("zMoreRegressions",
            Y=from@Y,
            X=from@X,
            output=from@output,
        )
      }
)

