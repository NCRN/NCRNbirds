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
#' 
#' @paramt times


#' @param band. A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.

#' @param visits, A length 1 numeric vector. Will only use data from points with that number of visits.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#' @details ADD ME
#' 
#' @include NCRNbirds_Class_def.R
#' @export


########################


setGeneric(name="nmixBirds",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=c(1,2),output="dataframe",...){standardGeneric("nmixBirds")}, signature="object")


############ need to add list method I

setMethod(f="nmixBirds", signature=c(object="list"),
          function(object,points,AOU,years,band,interval,visits, output) {
            OutBirds<-lapply(X=object, FUN=getBirds, points=points,AOU=AOU,years=years,band=band,visits=visits, output=output)
            switch(output
                   ,
                   list= return(OutBirds),
                   dataframe=return(do.call("rbind",OutBirds))
            )
          })


setMethod(f="nmixBirds", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,times,band,visits,...){
            
            VisitMat<-CountXVisit(object=object, points=points, AOU=AOU, years=years, times=times,
                                      band=band, visits=visits,...)
            
            UnmarkedData<-unmarkedFramePCount(y=VisitMat[4:ncol(VisitMat)], 
                                              siteCovs=as.data.frame((dplyr::select(VisitMat,Year,Admin_Unit_Code,Plot_Name))))
              
            
            #if only 1 year, then no predictors, otherwise, use  year for trends            
            PcountFormula<-if(length(unique(VisitMat$Year))==1) formula(~1~1) else formula(~1~Year)
            
            #run pcount
            return(pcount(formula=PcountFormula, data=UnmarkedData, mixture="P", K=100))
          }
)