#' @title getChecklist
#' 
#' @description Returns a checklist of birds found based on the  \code{Birds} slot of an NCRNbirds object. The returned data can be filtered to meet various criteria.
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param years A numeric vector. Returns data only from points where the years the point was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Year} column in the \code{visits} slot which is dervied from the imformation in the \code{Date} column.
#'  @param points A character vector. The names of one or more points where the data was collected.
#'  @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#'  @details This function returns AOU codes frome either a single \code{NCRNbirds} object or a list of such objects. The default output is a\code{data.frame}. However, if \code{object} is a list and \code{output} is "list" then a list of \code{data.frame}s will be returned. The name of each element in this list will correspond to the \code{ParkCode} in each NCRNbirds object. 
#'  
#'  
#'  @include NCRNbirds_Class_def.R
#'  @include getBirds.R
#'  @export


setGeneric(name="getChecklist",function(object, years=NA, points=NA, output="dataframe"){standardGeneric("getChecklist")}, signature="object")


setMethod(f="getChecklist", signature=c(object="list"),
          function(object,years,points,output) {
            OutList<-lapply(X=object, FUN=getChecklist, years=years, points=points)
            switch(output,
                   list={return(OutList)},
                   dataframe=return( sort( unique( do.call("c",OutList))))
            )
          })


setMethod(f="getChecklist", signature=c(object="NCRNbirds"),
          function(object,years,points, output){
            
            return( sort( unique( getBirds(object=object, years=years, points=points)$AOU)))
            
          }
)
