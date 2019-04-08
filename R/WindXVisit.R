#' @include NCRNbirds_Class_def.R getVisits.R getDesign.R
#' 
#' @title WindXVisit
#' 
#' @description Produces a Wind X Visit matrix for use in analyses
#' 
#' @importFrom dplyr group_by left_join mutate select summarize ungroup bind_rows
#' @importFrom magrittr %>%
#' @importFrom tidyr spread 
#' @importFrom data.table rbindlist
#' 
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of numbers. will return only data from the indicated years.
#' @param times  A numeric vector of length 1 passed on to \code{link{getVisits}}. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param band. A numeric vector. Defaults to 1. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits  The visits that will be used for the matrix. Defautls to \code{NA}. See Details below,
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{getBirds}
#' 
#' @details This produces a Count X Visit matrix for a \code{NCRNbirds} object or a \code{list} of such objects. Each row of the matrix
#'  will correspond to a different pointt in a different year. The columns of the matrix will be the park code, the point name, the year 
#'  visited, and a column of abundances of the indcated species at that visit. If multiple species are indicated in \code{AOU}, their 
#'  abundances will be totaled. 
#'  
#'  If \code{visits} is left as \code{NA} then the visits used will be 1 through the number of visits indicated in the \code{visits} slot. 
#'  Otherwise a numeric vectore e.g. c(1,2) can be used to select which visits are used. 
#'     
#' @export


########################


setGeneric(name="WindXVisit",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=NA,
                                       output="dataframe",...){standardGeneric("WindXVisit")}, signature="object")



setMethod(f="WindXVisit", signature=c(object="list"),
          function(object, points, AOU, years, band, visits, output,...) {
            OutMat<-lapply(X=object, FUN=WindXVisit, points=points, AOU=AOU, years=years, times=times,band=band,visits=visits,...)
            switch(output,
                   list= return(OutMat),
                   dataframe= return(rbindlist(OutMat, use.names=TRUE, fill=TRUE)) #return(bind_rows(OutMat))
            )
          })


setMethod(f="WindXVisit", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,band,visits,output,...){
            
            ## This makes a matrix with 1 for visits that occured and NA for visits that did not occur (such as only
            ##  visiting a point once instead of twice)
            visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
            
            VisitMat<-getVisits(object=object,points=points,years=years,times=times,visits=visits) %>%
              mutate(Visit=paste0("Visit",Visit),Visited=1) %>%
              dplyr::select(Admin_Unit_Code,Point_Name,Year,Visit,Visited) %>%
              spread(key=Visit, value=Visited)
            
            ## This makes a matrix that has the value of the count for each visit, summed across all intervals. If there is no 
            ## data there will be a "0", but this will occur for both missed visits and zero counts.

            
            WindMat<-getVisits(object=object,points=points,years=years,times=times,visits=visits)%>%
              dplyr::select(Admin_Unit_Code,Point_Name,EventDate,Visit,Year, Wind_Code) %>%
              mutate(Visit=paste0("Visit",Visit)) %>%
              group_by(Admin_Unit_Code,Point_Name, Year, Visit) %>%
              summarize(Wind=max(Wind_Code)) %>%
              spread(key=Visit,value=Wind,fill=0) 
              names(WindMat)[names(WindMat)=="Visit1"]<-"Wind1"
              names(WindMat)[names(WindMat)=="Visit2"]<-"Wind2"
              
              
            
            ## Now we need to multiply the Visit1, Visit2 etc. columns from each matrix so that missing visits will get
            ## NA instead of 0 in the output
            
          
            # if( ncol( WindMat)> 3) {
            #   WindMat[4:ncol(WindMat)]<-WindMat[4:ncol(WindMat)]*VisitMat[4:ncol(WindMat)]
            # }
           
            
            WindMat<-WindMat %>% ungroup  # to fix errors with dplyr when maniplating grouped tables
            return(WindMat)
            
            
          }
)