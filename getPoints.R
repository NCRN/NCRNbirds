#' @title getPoints
#' 
#' @description Returns the contents of the \code{Points} slot of an NCRNbirds object. The returned data can be filtered to meet various criteria.
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param type One of three options that indicate what type of points are to be considered. Must be in quotes. Options are:
#' \describe{
#' \item{"active"}{The default. Only returns data for points which are listed as active in the /code{Points$Location_Status} field.} 
#' \item{"all"}{The default. retruns data from all types of points.}
#' \item{"retired"}{Only returns data from points which are listed as retired in the \code{Points$Location_Status} field. }
#' }
#' @param visits A numeric vector. Returns only data from points where the number of point visits matches one of the values in \code{visits} The number of visits to a plot is determined by the \code{Event_Count} column in the \code{Events} slot. 
#' @param years A numeric vector. Returns data only from points where the years the point was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Event_Year} column in the \code{Events} slot. 
#'  @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#'  @details This function returns point data either from a single NCRNbirds object or a list of such objects. The default output is a\code{data.frame}. However, if \code{object} is a list and \code{output} is "list" then a list of \code{data.frame}s will be returned. The name of each element in this list will correspond to the \code{ParkCode} in each NCRNbirds object. 
#'  
#'  @export


setGeneric(name="getPoints",function(object,type = "active",visits=NA, years=NA, points=NA, output="dataframe"){standardGeneric("getPoints")}, signature="object")


setMethod(f="getPoints", signature=c(object="list"),
          function(object,type,visits,years,points,output) {
            OutPoints<-lapply(X=object, FUN=getPoints, type=type, visits=visits, years=years, points=points)
            switch(output,
                   list={names(OutPoints)<-getNames(object,name.class="code")
                         return(OutPoints)},
                   dataframe=return(do.call("rbind",OutPoints))
            )
          })


setMethod(f="getPoints", signature=c(object="NCRNbirds"),
          function(object,type,visits,years,points,output){
            switch(type,
                   all = XPlots<-object@Points,
                   ############## Fix these two - change $Location Status to a correct thing.
                   active= XPlots<-object@Points[object@Points$Location_Status=="Active",], 
                  retired= XPlots<-object@Points[object@Points$Location_Status=="Retired",],
                  
                  ##########################
                   stop("getPlots type not recognized")
            )
            
            ############## FIX THese!!!
            if(all(!is.na(visits))) XPoints<-XPoints[XPoints$Event_Count %in% visits,
            
            if(all(!is.na(years))) XPoints<-XPoints[XPoints$Point_Name %in% object@Events$Point_Name[object@Events$Event_Year %in% years],]
            
            #########################
            if(all(!is.na(plots))) XPoints<-XPoints[XPoints$Plot_Name %in% points,]
            
            
            return(XPoints)
            
          }
)

