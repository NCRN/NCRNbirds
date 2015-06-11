#' @include NCRNbirds_Class_def.R
#' 
#' @title getBirds
#' 
#' @importFrom dplyr filter
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
#' @param band. A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param interval A numeirc vector. Only observations whose \code{Interval} field matches a value in \code{interval} will be returned.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#' @details Returns the data from the \code{Birds} slot of a single \code{NCNRbirds} object or a \code{list} of such objects. The data can be filtered using the various arguements. he default output is a\code{data.frame}. However, if \code{object} is a \code{list} and \code{output} is "list" then a \code{list} of \code{data.frame}s will be returned.
#' 
#' @export
#'

setGeneric(name="getBirds",function(object,points=NA,AOU=NA,years=NA,min.count=NA, max.count=NA,band=NA,interval=NA,double=TRUE,output="dataframe"){standardGeneric("getBirds")}, signature="object")

setMethod(f="getBirds", signature=c(object="list"),
          function(object,points,AOU,years,min.count,max.count,band,interval,double, output) {
            OutBirds<-lapply(X=object, FUN=getBirds, points=points,AOU=AOU,years=years,min.count=min.count,max.count=max.count,band=band,interval=interval, double=double, output=output)
            switch(output,
                   list= return(OutBirds),
                   dataframe=return(do.call("rbind",OutBirds))
            )
          })


setMethod(f="getBirds", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,min.count,max.count,band,interval,double,output){
            XBirds<-object@Birds
  
            if(!is.na(points)) XBirds<-XBirds %>% filter(Plot_Name %in% points)
            if(!is.na(AOU)) XBirds<-XBirds %>% filter (AOU_Code %in% AOU)
            if(!is.na(years)) XBirds<-XBirds %>% filter(Year %in% years)
            if(!is.na(min.count)) XBirds<-XBirds %>% filter(Bird_Count >= min.count)  
            if(!is.na(max.count)) XBirds<-XBirds %>% filter(Bird_Count <= max.count)
            if(!is.na(band)) XBirds<-XBirds %>% filter(Distance_id %in% band)  
            if(!is.na(interval)) XBirds<-XBirds %>%  filter(Interval %in% interval)
            #if(double)Xbirds<-XBirds[XBirds$Plot_Name %in% unique(getVisits(object=object,
            #                                                    years=years,points=points,visit=c(1,2))$Plot_Name), ]

            return(XBirds)
            
          }
)