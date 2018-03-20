#' @include NCRNbirds_Class_def.R
#'  
#' @title getDesign
#' 
#' @description Returns informaiton on the study design stored in an \code{NCRNbirds} object.
#' 
#' @param object An \code{NCRNbirds} object or a list of such objects.
#' @param info  A length one chararcter vector. Indicates which aspect of the study design should be returned.
#' \describe{
#' \item{"visits"}{The number of visits typcially made to a point during a monitoring season. Retrieved from the \code{VisitNumber} slot.}
#' \item{"bands"}{The distance bands used for the study. Retrieved from the \code{Bands} slot.}
#' \item{"intervals"}{The time intervals used for the study. Retrieved from the \code{VisitNumber} slot.}
#' }
#' @details This function returns information about the study design used to collect the data. The type of information is 
#' determined by the \code{info} argument. When "visits" is selected the output will be a length one numeric vector, the other 
#' two options will return a\code{data.frame}. If the input is a \code{list} of \code{NCRNbird} objects, the the output will be eithe a 
#' list of vectors or a list of \code{date.frame}s depending on the \code{info} argument.
#'   
#' @export


setGeneric(name="getDesign",function(object,info){standardGeneric("getDesign")}, signature="object")


setMethod(f="getDesign", signature=c(object="list"),
          function(object,info) {
            XOut<-lapply(X=object, FUN=getDesign,info=info)
            return(XOut)

          })


setMethod(f="getDesign", signature=c(object="NCRNbirds"),
          function(object,info){
            
            XOut<-switch(info,
                   visits= object@VisitNumber,
                   bands= object@Bands,
                   intervals=object@Intervals)
            return(XOut)
            
          }
)
