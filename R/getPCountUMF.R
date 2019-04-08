#' @include NCRNbirds_Class_def.R CovsXVisit.R
#' 
#' @title getPCountUMF
#' 
#' @description Creates an umarkedFrame object for fitting N-mixture models to count data with the \code{\link[unmarked]{pcount}} function (Fiske et al. 2011).
#' 
#' @importFrom  unmarked unmarkedFramePCount
#'
#' @param object An NCRNbirds object or a list of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param times A numeric vector of length 1 passed on to \code{\link{CountXVisit}} and from there to \code{link{getVisits}}. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param band A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits A length 1 numeric vector. Will only use data from points with that number of visits.
#' @param ... Additonal arguments passed to \code{\link{getPCountUMF}}.
#' 
#' @details This function is used to create an unmarkedFrame object for inputing data for abundance estimation using the \code{\link[unmarked]{pcount}}function (Fiske et al. 2011).
#' 
#' @references Kery, M. Royle, J.A. and Schmid. 2005. Modeling avian abundance from replicated counts using binomail mixutre models. Ecological Applications. 15: 1450-1461. 
#' @references Royle, J. A.  2004. N-Mixture models for estimating population size from spatially replicated counts. Biometrics 60: 108-115.
#' @references Fiske, I., R. Chandler, and others. 2011. Unmarked: An R package for fitting hierarchical models of wildlife occurrence and abundance. Journal of Statistical Software 43:1-23.
#' 
#' @export



########################


setGeneric(name="getPCountUMF",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=NA,day=NA,observer=NA,temp=NA,humid=NA,sky=NA,mixture="P",...){standardGeneric("getPCountUMF")}, signature="object")


############ need to add list method I

setMethod(f="getPCountUMF", signature=c(object="list"),
          function(object,points,AOU,years,times,band,visits,day,observer,temp,humid,sky,wind,mixture,...) {
            
            return(lapply(X=object, FUN=getPCountUMF, points=points,AOU=AOU,years=years,times=times,band=band,visits=visits,day=day,observer=observer,temp=temp,humid=humid,sky=sky, wind=wind,mixture=mixture,...)
                   )
          }
  )


setMethod(f="getPCountUMF", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,times,band,visits,day,observer,temp,humid,sky,wind,mixture,...){
            
            CovsMat<-CovsXVisit(object=object, points=points,AOU=AOU,years=years,times=times,band=band,visits=visits,day=day,observer=observer,temp=temp,humid=humid,sky=sky,wind=wind,...)
            
            #remove NAs resulting from user subsetting decisions (e.g., years=c(2007,2008,2009))
            CovsMat<-subset(CovsMat, ! is.na(Count1))
            
            count.data<-data.frame(y.1=CovsMat$Count1,y.2=CovsMat$Count2)
            
            #define site covariates - FOR MULTIPLE YEARS
            site.covs<- data.frame(Year=as.factor(as.character(CovsMat$Year)), Admin_Unit_Code=as.factor(as.character(CovsMat$Admin_Unit_Code)), Point_Name=as.factor(as.character(CovsMat$Point_Name)))
            
            #create obsCovs matrices (Detection process)
            Day=data.frame(CovsMat$Day1, CovsMat$Day2)
            Visit=data.frame(CovsMat$Visit1, CovsMat$Visit2)
            Time=data.frame(CovsMat$Time1, CovsMat$Time2)
            Temp=data.frame(CovsMat$Temperature1, CovsMat$Temperature2)
            Humid=data.frame(CovsMat$Humidity1, CovsMat$Humidity2)
            Sky=data.frame(CovsMat$Sky_Condition1, CovsMat$Sky_Condition2)
            Wind=data.frame(CovsMat$Wind1, CovsMat$Wind2) 
            Observer=data.frame(CovsMat$Observer1, CovsMat$Observer2)
            
            #define abundance covariates (yearly site covs)
            obs.covs<-list(Visit=Visit, Day=Day, Time=Time, Temp=Temp, Humid=Humid,Sky=Sky, Wind=Wind,Observer=Observer)
            
            #set up unmarked FRAME GMM
            umf<-unmarkedFramePCount(y=count.data,siteCovs = site.covs,obsCovs= obs.covs)
            
            #run pcount
            return(umf)
          }
)