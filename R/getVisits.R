#' @include NCRNbirds_Class_def.R
#' 
#' @title getVisits
#' 
#' @description Returns the contents of the \code{Visits} slot of an NCRNbirds object. The returned data can be filtered to meet various criteria.
#' 
#' @param object An \code{NCRNbirds} object or a list of such objects.
#' @param points A character vector or point names. Only visits to these points will be returned.
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been vistied is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param years A numeric vector. Returns data only from points where the years the point was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Year} column in the \code{visits} slot which is dervied from the imformation in the \code{Date} column.
#'  @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#'  @details This function returns point data either from a single NCRNbirds object or a list of such objects. The default output is a\code{data.frame}. However, if \code{object} is a list and \code{output} is "list" then a list of \code{data.frame}s will be returned. The name of each element in this list will correspond to the \code{ParkCode} in each NCRNbirds object. 
#'  
#'  @export


setGeneric(name="getVisits",function(object,times=NA, years=NA, points=NA, visits=NA, output="dataframe"){standardGeneric("getVisits")}, signature="object")


setMethod(f="getVisits", signature=c(object="list"),
          function(object,times,years,points,visits,output) {
            OutVisits<-lapply(X=object, FUN=getVisits, times=times, years=years, points=points, visits=visits)
            switch(output,
                   list={#names(OutPoints)<-getNames(object,name.class="code")
                     return(OutVisits)},
                   dataframe=return(do.call("rbind",OutVisits))
            )
          })


setMethod(f="getVisits", signature=c(object="NCRNbirds"),
          function(object,times,years,points,visits, output){
            
            XVisits<-object@Visits
            
            if(!anyNA(times))      {
              X<-tbl_df(object@Visits) %>%
                group_by(Plot_Name) %>%
                mutate(Times=length(unique(Year))) %>%
                filter(Times>=times) %>%
                dplyr::select(Plot_Name) %>%
                unique
              XVisits<-XVisits[XVisits$Plot_Name %in% X$Plot_Name,]
              
            }
            
            if(!anyNA(years))XVisits<-XVisits[XVisits$Year %in% years,]
            
            if(!anyNA(points)) XVisits<-XVisits[XVisits$Plot_Name %in% points,]
            
            if(!anyNA(visits)) XVisits<-XVisits[XVisits$Visit %in% visits,]
            
            return(XVisits)
            
          }
)
