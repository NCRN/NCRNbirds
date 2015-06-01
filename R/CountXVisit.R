#' @title CountXVisit
#' 
#' @description Produces a Count X Visit matrix for use in analyses
#' 
#' @import tidyr 
#' @import dplyr
#' 
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param times  A numeric vector. Returns data only from points where the years the point was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Year} column in the \code{visits} slot which is dervied from the imformation in the \code{Date} column.
#' @param band. A numeric vector. Defaults to 1. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits, The visits that will be used for the matrix. Defautls to \code{c(1,2)}.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{getBirds}
#' 
#' @details This produces a Count X Visit matrix for a \code{NPSbirds} object or a \code{list} of such objects. Each row of the matrix will correspond to a differnet plot in a differnt year. The columns of the matrix will be the park code, the point name, the year visited, and a column of abundances of the indcated species at that visit. If multiple species are indicated in \code{AOU}, their abundaces will be totaled. 
#' 
#' 
#' @include NCRNbirds_Class_def.R
#' @include getVisits.R
#' @include getBirds.R
#' @export


########################


setGeneric(name="CountXVisit",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=c(1,2),output="dataframe",...){standardGeneric("CountXVisit")}, signature="object")



setMethod(f="CountXVisit", signature=c(object="list"),
          function(object, points, AOU, years, band, visits, output,...) {
            OutMat<-lapply(X=object, FUN=CountXVisit, points=points, AOU=AOU, years=years, times=times,band=band,visits=visits,...)
            switch(output,
                   list= return(OutMat),
                   dataframe=return(do.call("rbind",OutMat))
            )
          })


setMethod(f="CountXVisit", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,band,visits,output,...){
            
            ## This makes a matrix with 1 for visits that occured and NA for visits that did not occur (such as only
            ##  visiting a point once instead of twice)
            
            VisitMat<-getVisits(object=object,points=points,years=years,times=times,visits=visits) %>%
              mutate(Visit=paste0("Visit",Visit),Visited=1) %>%
              dplyr::select(Admin_Unit_Code,Plot_Name,Year,Visit,Visited) %>%
              spread(key=Visit, value=Visited)
            
            ## This makes a matrix that has the value of the count for each visit, summed across all intervals. If there is no 
            ## data there will be a "0", but this will occur for both missed visits and zero counts.

            
            CountMat<-getVisits(object=object,points=points,years=years,times=times,visits=visits)%>%
              mutate(Visit=paste0("Visit",Visit)) %>%
              dplyr::select(Admin_Unit_Code,Plot_Name,Date,Visit,Year) %>%
              left_join(getBirds(object=object, points=points, AOU=AOU, years=years, band=band, ...))%>% 
              group_by(Admin_Unit_Code,Plot_Name, Year, Visit) %>%
              summarize(Counts=sum(Bird_Count)) %>%
              spread(key=Visit,value=Counts,fill=0) 
              
            
            ## Now we need to multiply the Visit1, Visit2 etc. columns from each matrix so that missing visits will get
            ## NA instead of 0 in the output
            
            CountMat[4:ncol(CountMat)]<-CountMat[4:ncol(CountMat)]*VisitMat[4:ncol(CountMat)]
            return(CountMat)
            
            
          }
)