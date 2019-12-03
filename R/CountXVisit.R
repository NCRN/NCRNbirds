#' @include NCRNbirds_Class_def.R getVisits.R getBirds.R getDesign.R
#' 
#' @title CountXVisit
#' 
#' @description Produces a Count X Visit matrix for use in analyses
#' 
#' @importFrom dplyr case_when group_by left_join matches mutate select summarize ungroup vars
#' @importFrom magrittr %>%
#' @importFrom rlang !!! syms
#' @importFrom tidyr spread 
#' @importFrom data.table rbindlist
#' 
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param parks A character vector of park codes. Only visits within these parks will be returned.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years A vector of numbers. will return only data from the indicated years.
#' @param times A numeric vector of length 1 passed on to \code{\link{getVisits}}. Returns only data from points where the number of years that a point has 
#' been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param band A numeric vector. Defaults to 1. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits The visits that will be used for the matrix. Defautls to \code{NA}. See Details below.
#' @param max Logical, defaults to \code{FALSE}. If \code{TRUE} then the matrix will also include a single column with the maximum detections seen at 
#' each point across all the visits specified in the \code{visits} argument.
#' @param type Either "count", the default of "occupancy". Determines the type of data in the matrix. If "count" then each number will represent the
#' number of birds observed. If "occupancy" then the data will be 1 if one or more birds is observed, and 0 otherwise. 
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{getBirds}
#' 
#' @details This produces a Count X Visit matrix for a \code{NCRNbirds} object or a \code{list} of such objects. Each row of the matrix
#'  will correspond to a different point in a different year. The columns of the matrix will be the park code, the point name, the year 
#'  visited, and a column of abundances of the indcated species at that visit. If multiple species are indicated in \code{AOU}, their 
#'  abundances will be totaled. If \code{max=T} then the maximum accross all visits will be returned as well as the indvidual visit totals.
#'  
#'  If \code{visits} is left as \code{NA} then the visits used will be 1 through the number of visits indicated in the \code{visits} slot. 
#'  Otherwise a numeric vectore e.g. c(1,2) can be used to select which visits are used. 
#'  
#'  If \code{type} is set as "occupancy" the the matix will have 1 for visits where a site is occuped and 0 forvisits where it is unoccupied rather than 
#'  abundances. 
#'     
#' @export


########################


setGeneric(name="CountXVisit",function(object,parks= NA, points=NA,AOU=NA,years=NA,times=NA,band=1,visits=NA,max=F,type="count",
                                       output="dataframe",...){standardGeneric("CountXVisit")}, signature="object")



setMethod(f="CountXVisit", signature=c(object="list"),
          function(object, parks, points, AOU, years, times, band, visits, max, type, output,...) {
            OutMat<-lapply(X=object, FUN=CountXVisit,parks=parks, points=points, AOU=AOU, years=years, times=times,band=band,visits=visits,max=max,
                           type=type,...)
            switch(output,
                   list= return(OutMat),
                   dataframe= return(rbindlist(OutMat, use.names=TRUE, fill=TRUE)) 
            )
          })


setMethod(f="CountXVisit", signature=c(object="NCRNbirds"),
          function(object,parks, points,AOU,years,times, band,visits,max, type,...){
            
            ## This makes a matrix with 1 for visits that occured and NA for visits that did not occur (such as only
            ##  visiting a point once instead of twice)
            visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
            
            VisitMat<-getVisits(object=object,parks=parks, points=points,years=years,times=times,visits=visits) %>%
              mutate(Visit=paste0("Visit",Visit),Visited=1) %>%
              dplyr::select(Admin_Unit_Code,Point_Name,Year,Visit,Visited) %>%
              spread(key=Visit, value=Visited)
            
            ## This makes a matrix that has the value of the count for each visit, summed across all intervals. If there is no 
            ## data there will be a "0", but this will occur for both missed visits and zero counts.

            
            CountMat<-getVisits(object=object,parks=parks, points=points,years=years,times=times,visits=visits)%>%
              dplyr::select(Admin_Unit_Code,Point_Name,EventDate,Visit,Year) %>%
              left_join(getBirds(object=object, points=points, AOU=AOU, years=years, band=band, ...))%>% 
              mutate(Visit=paste0("Visit",Visit)) %>%
              group_by(Admin_Unit_Code,Point_Name, Year, Visit) %>%
              summarize(Counts=case_when(
                        type =="count"~ sum(Bird_Count),
                        type=="occupancy"~ min(Bird_Count,1) )) %>% #if Bird_Count is 0, Count will be 0, otherwise it will be 1 
              spread(key=Visit,value=Counts,fill=0) 
              
            
            ## Now we need to multiply the Visit1, Visit2 etc. columns from each matrix so that missing visits will get
            ## NA instead of 0 in the output
            
          
            if( ncol( CountMat)> 3) {
              CountMat[4:ncol(CountMat)]<-CountMat[4:ncol(CountMat)]*VisitMat[4:ncol(CountMat)]
            }
           
            
            CountMat<-CountMat %>% ungroup  # to fix errors with dplyr when maniplating grouped tables
            
            if(max & nrow(CountMat)>0){
              VisitCols<-CountMat %>% dplyr::select(-c(Admin_Unit_Code, Point_Name, Year)) %>% names
              CountMat<-CountMat %>%mutate(Max=pmax(!!!syms(VisitCols), na.rm=T))
            }
                                                                 
            return(CountMat)
            
            
          }
)