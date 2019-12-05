#' @include NCRNbirds_Class_def.R 
#' 
#' @title makeUMF
#' 
#' @description Makes an unmarked framre for analysis using the unmarekd package
#' 
#' @importFrom dplyr select
#' @importFrom purrr pmap
#' @importFrom  unmarked unmarkedFrameOccu
#'
#' @param object An NCRNbirds object or a list of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param times A numeric vector of length 1 passed on to \code{\link{CountXVisit}} and from there to \code{link{getVisits}}. 
#' Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. 
#' This is determined based on the data found in the \code{Visits} slot.
#' @param band A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits A length 1 numeric vector. Will only use data from points with that number of visits.
#' 
#' @param mixture  Defaults to "P". One of either "P", "NB", or "ZIP". Indicates which latent abundace distibution to use, either Poisson, Negative Binomial or Zero Inflated Poinsson.  Passed on to the \code{mixture} argumnet of \code{pcount} in the \code{unmarked} package. 
#' 
#' @param ovbscovlist A named \code{list} of \code{data.frame}s with observation covariates. Typically this will include covarites that are not returned 
#' by the \code{\link{CovsXVisit}} function.
#' @param output   Either "umf" (the defautlt) or "list". Controls the output when \code{object} is a list. If "umf" the data from all \code{NCRNbirds}
#' objects in the list will be combined into a single unmarked frame. If "list" then the output will be a list of unmarked frames - one for 
#' each \code{NCRNbirds} object in the input list. 
#' @param ... Additonal arguments passed to \code{\link{CountXVisit}} and from there to \code{\link{getBirds}}.
#' 
#' @details This function simplifed the process for fitting a N-mixture model (Royal 2004, Kery et al. 2005) to data in an NCRNbirds object or a list of such objects. The data is first extracted from the object(s) using the \code{\link{CountXVisit}} funciton. The data is then fed to the \code{\link[unmarked]{pcount}} function and the resutls of the analysis are returned. 
#' 
#' @references Kery, M. Royle, J.A. and Schmid. 2005. Modeling avian abundance from replicated counts using binomail mixutre models. Ecological Applications. 15: 1450-1461. 
#' @references Royle, J. A.  2004. N-Mixture models for estimating population size from spatially replicated counts. Biometrics 60: 108-115.
#' 
#' @export


########################

setGeneric(name="makeUMF",function(object,obscovslist=NULL, output="umf",#points=NA,AOU=NA,years=NA,times=NA,band=1,visits=c(1,2),mixture="P",
                                   ... ){standardGeneric("makeUMF")},  signature="object")

setMethod(f="makeUMF", signature=c(object="list"),
           function(object,obscovslist, output,...) {
             switch(output,
               umf= return(CountXVisit(object=object, type="occupancy",output="dataframe",...) %>% 
                             dplyr::select(-c(Admin_Unit_Code, Point_Name, Year)) %>% 
                             makeUMF),
               list={
                  umflist<-list(object=object, obscovslist=obscovslist)
                  umflist<-umflist[!sapply(umflist, is.null)] # removes any null elements so pmap will wrok
                  return(pmap(.l=umflist, makeUMF, ...) )
               }
              )
             
            }
 )
 
 
setMethod(f="makeUMF", signature=c(object="NCRNbirds"),
    function(object,obscovslist, ...){
           
      VisitMat<-CountXVisit(object=object, type="occupancy",...) %>% 
        dplyr::select(-c(Admin_Unit_Code, Point_Name, Year))
           
#             UnmarkedData<-unmarkedFramePCount(y=VisitMat[4:ncol(VisitMat)], 
#                                               siteCovs=as.data.frame((dplyr::select(VisitMat,Year,Admin_Unit_Code,Point_Name))))
#             
#             
#             #if only 1 year, then no predictors, otherwise, use  year for trends            
#             PcountFormula<-if(length(unique(VisitMat$Year))==1) formula(~1~1) else formula(~1~Year)
#             
#             #run pcount
    return(makeUMF(object=VisitMat, obscovslist = obscovslist))
    }
 )

setMethod(f="makeUMF", signature=c(object="data.frame"),
          function(object, obscovslist){
            
            #1 Need data frame with occupancy only - this is the object for this function 
            #2 need site covarite data
            #3 need  observation covariate data
            #4 call unmarkedFrameOccu from unmareked package
            

            return(unmarkedFrameOccu(y=object, obsCovs=obscovslist))
          }
)