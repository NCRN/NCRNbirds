#' @include NCRNbirds_Class_def.R getBirds.R getBirdNames.R
#' 
#' @title getChecklist
#' 
#' @description Returns a checklist of birds found based on the  \code{Birds} slot of an NCRNbirds object. The returned data can be filtered to meet various criteria.
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param years A numeric vector. Returns data only from points where the years the point was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Year} column in the \code{visits} slot which is derived from the information in the \code{EventDate} column.
#'  @param points A character vector. The names of one or more points where the data was collected.
#'   @param out.style A length 1 character vector. Either "AOU", "Latin", "common". Indicates the type of names to be returned. Defaults to "AOU"
#'  @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#'  @param ... Other options passed onto \code{\link{getBirds}}. Any valid option for \code{getBirds}, such as \code{bands}, \code{min.count} etc. can be added to further specify which data should be considered in making the checklist.
#' 
#'  @details This function returns AOU codes frome either a single \code{NCRNbirds} object or a list of such objects. The default output is a\code{data.frame}. However, if \code{object} is a list and \code{output} is "list" then a list of \code{data.frame}s will be returned. The name of each element in this list will correspond to the \code{ParkCode} in each NCRNbirds object. 
#'  
#'  @export


setGeneric(name="getChecklist",function(object, years=NA, points=NA, out.style="AOU",output="dataframe",...){standardGeneric("getChecklist")}, signature="object")


setMethod(f="getChecklist", signature=c(object="list"),
          function(object,years,points,output,...) {
            OutList<-lapply(X=object, FUN=getChecklist, years=years, points=points, out.style=out.style,...)
            switch(output,
                   list={return(OutList)},
                   dataframe=return( sort( unique( do.call("c",OutList))))
            )
          })


setMethod(f="getChecklist", signature=c(object="NCRNbirds"),
          function(object,years,points, out.style, output,...){
            
            return( sort( getBirdNames(object=object, names=unique( getBirds(object=object, years=years, points=points,...)$AOU),
                                       in.style="AOU", out.style=out.style) ))
            
          }
)
