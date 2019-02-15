#' Class "drawdowns"
#'
#' @description Class for representing drawdowns (and corresponding derivative of the drwadowns) at a well or a piezometer, for positive times during or after pumping (recovery).
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{dd}:}{Object of class \code{"data.frame"}, containing the times and the corresponding numerical values of the drawdowns.}
#'    \item{\code{ldiff}:}{Object of class \code{"data.frame"}, containing the times and the correspondinf davlues of the calculated derivative of the drawdowns.}
#'  }
#' @name drawdowns-class
#' @exportClass drawdowns
#' @author François Bertone
#' @importFrom methods setClass
setClass("drawdowns",
         representation(dd="data.frame",ldiff="data.frame"),
         prototype = list(dd=data.frame(t=NULL,s=NULL),ldiff=data.frame(dt=NULL,ds=NULL)),
         validity = function(object){
           if((length(object@dd$t) != length(object@dd$s)) || ((length(object@dd$t) != length(object@dd$s)))) {
             return("Inconsistent length of vectors")
           }
           return(TRUE)
         })
