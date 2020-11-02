#' @include NCRNbirds_Class_def.R getVisits.R
#' 
#' @title getBirds
#' 
#' @importFrom dplyr filter select semi_join
#' @importFrom magrittr %>%
#' 
#' @description Returns bird monitoring data from the \code{Birds} slot of an \code{NCRNbirds} object. 
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' 
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of numbers. Will return only data from the indicated years.
#' @param min.count  A numeric vector of length one. Will only return data with a bird count equal to or geater than \code{min.count}
#' @param max.count  A numeric vector of length one. Will only return data with a bird count equal to or less than \code{max.count}
#' @param band A numeric vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned. \code{NA} returns all bands.
#' @param interval A numeric vector. Only observations whose \code{Interval} field matches a value in \code{interval} will be returned.
#' @param reps A numeric vector of length 1. Defaults to NA. Returns only data from points where the number of years that a point has been 
#' visited is greater or equal to the valuemax,site,dist,wind,sky, of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been visited is greater or equal
#' to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param visits A length 1 numeric vector, defaults to \code{NA}. Returns data only from the indicated visits.
#' @param flyover Logical. \code{TRUE} to include flyovers in count data. Defaults to \code{FALSE}.
#' @param incidental Logical. \code{TRUE} to include incidental observations in count data. Defaults to \code{FALSE}.
#' @param juvenile Logical. \code{TRUE} to include observations of juvenile birds in count data. Defaults to \code{FALSE}.
#' @param gender Character. Defaults to selecting all detected individuals. Options to filter by gender are "Male", "Female", "Undetermined".
#' @param first3min Logical. If \code{TRUE}, only returns detections within first 3 minutes of the timed count. Defaults to selecting counts from all timed intervals. 
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#' @details Returns the data from the \code{Birds} slot of a single \code{NCNRbirds} object or a \code{list} of such objects. The data can be 
#' filtered using the various arguments. The default output is a\code{data.frame}. However, if \code{object} is a \code{list} and \code{output} is "list" 
#' then a \code{list} of \code{data.frame}s will be returned.
#' 
#' @export

setGeneric(name="getBirds",function(object,points=NA,AOU=NA,years=NA,min.count=NA, max.count=NA,band=NA,interval=NA,
                                    visits=NA,times=NA,reps=NA, flyover= FALSE,gender=NA, first3min = FALSE,incidental= FALSE, juvenile= FALSE, output="dataframe"){standardGeneric("getBirds")}, signature="object")

setMethod(f="getBirds", signature=c(object="list"),
          function(object,points,AOU,years,min.count,max.count,band,interval,visits,times,reps,flyover, gender, first3min, incidental,juvenile, output) {
            OutBirds<-lapply(X=object, FUN=getBirds, points=points,AOU=AOU,years=years,min.count=min.count,max.count=max.count,
                             band=band,interval=interval,visits=visits, times=times, reps=reps, flyover=flyover, gender=gender,
                             first3min=first3min, incidental=incidental, juvenile=juvenile,output=output)
            switch(output,
                   list= return(OutBirds),
                   dataframe=return(do.call("rbind",OutBirds))
            )
          })


setMethod(f="getBirds", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,min.count,max.count,band,interval,visits,times,reps,flyover,gender,first3min,incidental,juvenile, output){
            XBirds<-object@Birds
            if(!flyover & exists("Flyover_Observed", XBirds)) XBirds<-XBirds %>% filter(Flyover_Observed %in% 0)# keep flyovers in the data if TRUE
            if(first3min & exists("Initial_Tree_Min_Cnt", XBirds)) XBirds<-XBirds %>% filter(Initial_Three_Min_Cnt %in% 1) # keep only detecions during first 3 min if TRUE
            if(!incidental & exists("Incidental", XBirds)) XBirds<-XBirds %>% filter(Incidental %in% 0)# keep incidental obs in the data if TRUE
            if(!juvenile& exists("Juvenile", XBirds)) XBirds<-XBirds %>% filter(Juvenile %in% 0)# keep juveniles obs ni the data if TRUE
            if(!anyNA(gender)) XBirds<-XBirds %>% filter(Sex %in% gender)
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
