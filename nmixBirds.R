#' @title nmixBirds
#' 
#' @description Peforms trend analysis on bird data using N-mixture models (Royal 2004) form the unmarked package.
#' 
#'  @import unmarked
#'  @import tidyr 
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' 
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param min.count  A numeric vector of length one. Will only return data with a bird count equal to or geater than \code{min.count}
#' @param max.count  A numeric vector of length one. Will only return data with a bird count equal to or less than \code{max.count}
#' @param band. A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param interval A numeirc vector. Only observations whose \code{Interval} field matches a value in \code{interval} will be returned.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#' @details ADD ME
#' 
#' @include NCRNbirds_Class_def.R
#' @export


########################


setGeneric(name="nmixBirds",function(object,points=NA,AOU=NA,years=NA,min.count=NA, max.count=NA,band=1,interval=NA,double=TRUE,output="dataframe"){standardGeneric("nmixBirds")}, signature="object")

setMethod(f="nmixBirds", signature=c(object="list"),
          function(object,points,AOU,years,min.count,max.count,band,interval,double, output) {
            OutBirds<-lapply(X=object, FUN=getBirds, points=points,AOU=AOU,years=years,min.count=min.count,max.count=max.count,band=band,interval=interval, double=double, output=output)
            switch(output,
                   list= return(OutBirds),
                   dataframe=return(do.call("rbind",OutBirds))
            )
          })


setMethod(f="nmixBirds", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,min.count,max.count,band,interval,double,output){
            
            
            MixBirds<-getBirds(object, points, AOU,years,min.count,max.count,band,interval,double)
            MixVisits<-getVisits(object=object,years=years,points=points) %>%
              mutate(Visit=paste0("Visit",Visit))
            MixBirds<-MixBirds %>%
              left_join(MixVisits %>% dplyr::select(Plot_Name,Date,Visit))
            MixData<-spread(MixBirds,key=Visit,value=Bird_Count,fill=0) 
            unmarkedData<-unmarkedFramePCount(y=dplyr::select(MixData,Visit1,Visit2), 
                                              siteCovs=dplyr::select(MixData,Year,Admin_Unit_Code,Plot_Name))
           ifelse(length(years)==1,
              return(pcount(~1~1, data=unmarkedData, mixture="P",K=100)),
              return(pcount(~1~Year, data=unmarkedData,mixture="P",K=100))
           )
            
            
          }
)