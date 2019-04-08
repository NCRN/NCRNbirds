#' @include NCRNbirds_Class_def.R CountXVisit.R VisitXVisit.R ObsXVisit.R TempXVisit.R HumidXVisit.R SkyXVisit.R WindXVisit.R TimeXVisit.R DayXVisit.R getBirds.R getDesign.R
#' 
#' @title CovsXVisit
#' 
#' @description Produces a compoasite of Covariates X Visits matrix for use in analyses.
#' 
#' @importFrom data.table rbindlist
#'  
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param band 
#' @param visits  The visits that will be used for the matrix. Defautls to \code{NA}. See Details below
#' @param years  A vector of numbers. will return only data from the indicated years.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{getBirds}
#' 
#' @details This generates and combines matrices of the following covariates: temperature, humidity, sky condition, wind, time of day, ordinal day, and observer by Visits for use in building occupancy and abundance models with  \code{estimateOccupancy} and \code{estimateAbundance} functions, respectively. If \code{visits} is left as \code{NA} then the visits used will be 1 through the number of visits indicated in the \code{visits} slot. Otherwise a numeric vectore e.g. c(1,2) can be used to select which visits are used. 
#'     
#' @export


########################


setGeneric(name="CovsXVisit",function(object, points=NA, AOU=NA,band=1,visits=NA, years=NA, output="dataframe",...){standardGeneric("CovsXVisit")}, signature="object")



setMethod(f="CovsXVisit", signature=c(object="list"),
          function(object, points, AOU, band, visits, years,...){
            switch(output,
                   list= return(OutMat),
                   dataframe= return(rbindlist(OutMat, use.names=TRUE, fill=TRUE)) #return(bind_rows(OutMat))
            )
          })


setMethod(f="CovsXVisit", signature=c(object="NCRNbirds"),
          function(object, points, AOU, band, visits, years, output,...){
            
            ## This makes a matrix with 1 for visits that occured and NA for visits that did not occur (such as only
            ##  visiting a point once instead of twice)
            visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
            
            count.data<-CountXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            visit.covs<-VisitXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            day.covs<-DayXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            obs.covs<-ObsXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            time.covs<-TimeXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            temp.covs<-TempXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            humid.covs<-HumidXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            sky.covs<-SkyXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            wind.covs<-WindXVisit(object=object,points=points, AOU=AOU, years=years, band=band, visits=visits)
            
          
            compile.data<-Reduce(function(x, y) merge(x, y, all=TRUE), list(count.data,visit.covs, day.covs, obs.covs,time.covs, temp.covs, humid.covs, sky.covs, wind.covs))
            
            return(compile.data)
            
          }
)