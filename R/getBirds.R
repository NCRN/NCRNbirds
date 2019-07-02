#' @include NCRNbirds_Class_def.R getVisits.R
#' 
#' @title getBirds
#' 
#' @importFrom dplyr filter semi_join select
#' @importFrom magrittr %>%
#' 
#' @description Returns bird monitoring data from the \code{Birds} slot of an \code{NCRNbirds} object. 
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' 
#' @param points A character vecotr. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param min.count  A numeric vector of length one. Will only return data with a bird count equal to or geater than \code{min.count}
#' @param max.count  A numeric vector of length one. Will only return data with a bird count equal to or less than \code{max.count}
#' @param band. A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned. Options are \code{1} for birds closer than 50m to the observer, \code{2} for birds between 50 and 100 meters from the observer, \code{c(1,2)} for birds between 0 and 100 meters of the observer, or \code{NA} for all birds regardless of distance. 
#' @param interval A numeirc vector. Only observations whose \code{Interval} field matches a value in \code{interval} will be returned.
#' @param reps A numeric vector of length 1. Defaults to NA. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#'  @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param visits A length 1 numeric vector, defaults to NA. Returns data only from the incidated visits.

#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#' @details Returns the data from the \code{Birds} slot of a single \code{NCNRbirds} object or a \code{list} of such objects. The data can be filtered using the various arguements. he default output is a\code{data.frame}. However, if \code{object} is a \code{list} and \code{output} is "list" then a \code{list} of \code{data.frame}s will be returned.
#' 
#' @export

setGeneric(name="getBirds",function(object,points=NA,AOU=NA,years=NA,min.count=NA, max.count=NA,band=NA,interval=NA,visits=NA,times=NA,reps=NA,output="dataframe"){standardGeneric("getBirds")}, signature="object")

setMethod(f="getBirds", signature=c(object="list"),
          function(object,points,AOU,years,min.count,max.count,band,interval,visits,times,reps, output) {
            OutBirds<-lapply(X=object, FUN=getBirds, points=points,AOU=AOU,years=years,min.count=min.count,max.count=max.count,band=band,interval=interval,visits=visits, times=times, reps=reps, output=output)
            switch(output,
                   list= return(OutBirds),
                   dataframe=return(do.call("rbind",OutBirds))
            )
          })


setMethod(f="getBirds", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,min.count,max.count,band,interval,visits,times,reps,output){
            XBirds<-object@Birds
  
            if(!anyNA(points)) XBirds<-XBirds %>% filter(Point_Name %in% points)
            if(!anyNA(AOU)) XBirds<-XBirds %>% filter (AOU_Code %in% AOU)
            if(!anyNA(years)) XBirds<-XBirds %>% filter(Year %in% years)
            if(!anyNA(min.count)) XBirds<-XBirds %>% filter(Bird_Count >= min.count)  
            if(!anyNA(max.count)) XBirds<-XBirds %>% filter(Bird_Count <= max.count)
            if(!anyNA(band)) XBirds<-XBirds %>% filter(Distance_id %in% band)  
            if(!anyNA(interval)) XBirds<-XBirds %>%  filter(Interval %in% interval)
            if(!anyNA(times) || !anyNA(visits) || !anyNA(reps)) XBirds<-XBirds %>% 
                semi_join(getVisits(object=object, points=points, years=years,visits=visits,times=times, reps=reps) %>% 
                            dplyr::select(Point_Name,Year,EventDate, Visit), by=c("Point_Name","Year","EventDate", "Visit")
                )
            return(XBirds)
            
          }
)