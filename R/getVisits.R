#' @include NCRNbirds_Class_def.R
#' 
#' @title getVisits
#' 
#' @description Returns the contents of the \code{Visits} slot of an NCRNbirds object. The returned data can be filtered to meet various criteria.
#' 
#' @importFrom dplyr filter group_by mutate select semi_join tbl_df
#' 
#' @param object An \code{NCRNbirds} object or a list of such objects.
#' @param points A character vector or point names. Only visits to these points will be returned.
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been vistied is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param years A numeric vector. Returns data only from points where the years the point was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Year} column in the \code{visits} slot which is dervied from the imformation in the \code{EventDate} column.
#' @param visits A length 1 numeric vector, defaults to NA. Returns data only from the indicated visits.
#' @param reps A length 1 numeric vector,d deflatus to NA, Returns only data form points and years where the point has been visited at least \code{reps} times in the year.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @details This function returns point data either from a single NCRNbirds object or a list of such objects. The default output is a\code{data.frame}. However, if \code{object} is a list and \code{output} is "list" then a list of \code{data.frame}s will be returned. The name of each element in this list will correspond to the \code{ParkCode} in each NCRNbirds object. 
#'  
#' @export


setGeneric(name="getVisits",function(object,times=NA, years=NA, points=NA, visits=NA, reps=NA, output="dataframe"){standardGeneric("getVisits")}, signature="object")


setMethod(f="getVisits", signature=c(object="list"),
          function(object,times,years,points,visits,reps,output) {
            OutVisits<-lapply(X=object, FUN=getVisits, times=times, years=years, points=points, visits=visits,reps=reps)
            switch(output,
                   list={#names(OutPoints)<-getNames(object,name.class="code")
                     return(OutVisits)},
                   dataframe=return(do.call("rbind",OutVisits))
            )
          })


setMethod(f="getVisits", signature=c(object="NCRNbirds"),
          function(object,times,years,points,visits,reps, output){
            
            XVisits<-object@Visits
            
            if(!anyNA(years))XVisits<-XVisits[XVisits$Year %in% years,]
            
            if(!anyNA(points)) XVisits<-XVisits[XVisits$Point_Name %in% points,]
            
            if(!anyNA(visits)) XVisits<-XVisits[XVisits$Visit %in% visits,]
            
            if(!anyNA(times))      {
              X<-tbl_df(object@Visits) %>%
                group_by(Point_Name) %>%
                summarize(Times=n_distinct(Year)) %>%
                filter(Times>=times) %>%
                dplyr::select(Point_Name) %>%
                unique
              XVisits<-XVisits %>% semi_join(X, by="Point_Name","Year")
            }
            
                        
            if(!anyNA(reps))      {
              X<-tbl_df(object@Visits) %>%
                group_by(Point_Name,Year) %>%
                summarize(Reps=n()) %>%
                filter(Reps>=reps) %>%
                dplyr::select(Point_Name, Year) %>%
                unique
              XVisits<-XVisits %>% semi_join(X, by=c("Point_Name","Year"))
            }

            
            return(XVisits)
            
          }
)
