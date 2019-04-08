#' @include NCRNbirds_Class_def.R getPCountUMF.R CountXVisit.R getVisits.R
#' 
#' @title nmixBirds_zl
#' 
#' @description Fits N-mixture models (Royal 2004) to count data using the pcount function (Fiske et al. 2011).
#' 
#' @importFrom unmarked pcount unmarkedFramePCount
#'
#' @param object An NCRNbirds object or a list of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param times A numeric vector of length 1 passed on to \code{\link{CountXVisit}} and from there to \code{\link{getVisits}}. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param band A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits A length 1 numeric vector. Will only use data from points with that number of visits.
#' @param site.covs A list of desired site-level covariates that can be included within the abundance estimation process of the N-mixture model.
#' @param obs.covs A list of desired observation-level covariates that can be included within the detection probability estimation process of the N-mixture model.
#' @param mixture  Defaults to "P". One of either "P", "NB", or "ZIP". Indicates which latent abundace distibution to use, either Poisson, Negative Binomial or Zero Inflated Poisson.  Passed on to the \code{mixture} argumnet of \code{pcount} in the \code{unmarked} package. 
#' @param ... Additonal arguments passed to \code{\link{CountXVisit}}.
#' 
#' @details This function simplifed the process for fitting a N-mixture model (Royal 2004, Kery et al. 2005) to data in an NCRNbirds object or a list of such objects. The data is first extracted from the object(s) using the \code{\link{CountXVisit}} funciton. The data is then fed to the \code{\link[unmarked]{pcount}} function and the resutls of the analysis are returned. 
#' 
#' @references Kery, M. Royle, J.A. and Schmid. 2005. Modeling avian abundance from replicated counts using binomail mixutre models. Ecological Applications. 15: 1450-1461. 
#' @references Royle, J. A.  2004. N-Mixture models for estimating population size from spatially replicated counts. Biometrics 60: 108-115.
#' @references Fiske, I., R. Chandler, and others. 2011. Unmarked: An R package for fitting hierarchical models of wildlife occurrence and abundance. Journal of Statistical Software 43:1-23.
#'
#' @export


########################


setGeneric(name="nmixBirds_zl",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=c(1,2),site.covs=NA,obs.covs=NA,mixture="P",...){standardGeneric("nmixBirds_zl")}, signature="object")


############ need to add list method I

setMethod(f="nmixBirds_zl", signature=c(object="list"),
          function(object,points,AOU,years,times,band,visits,site.covs,obs.covs,mixture,...) {
            
            return(lapply(X=object, FUN=nmixBirds_zl, points=points,AOU=AOU,years=years,times=times,band=band,
                           visits=visits,site.covs=site.covs, obs.covs=obs.covs,mixture=mixture,...)
                   )
          }
  )


setMethod(f="nmixBirds_zl", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,times,band,visits,site.covs,obs.covs,mixture,...){
            
            umf<-getPCountUMF(object=object,points=points,AOU=AOU,years=years,times=times,band=band,visits=visits, mixture,...)
              
            site.covs.add<-ifelse(length(site.covs)>1, paste("~", paste(site.covs, collapse='+' ),sep=""),
                                  ifelse(!is.na(site.covs),paste("~", site.covs, sep=""),
                                         paste("~",1,sep="")))
                                
                 
            obs.covs.add<-ifelse(length(obs.covs)>1,paste("~", paste(obs.covs, collapse='+' ),sep=""),
                                 ifelse(!is.na(obs.covs), paste("~", obs.covs, sep=""),
                                        paste("~",1,sep="")))
            
            mod.add<-paste(obs.covs.add, site.covs.add)
            
            mod.formula<-formula(mod.add)
            
      
            #if only 1 year, then no predictors, otherwise, use  year for trends      
            PcountFormula<-if(length(unique(umf@siteCovs$Year))==1) formula(~1~1) else mod.formula
            
            #run pcount
            return(pcount(formula=PcountFormula, data=umf, mixture=mixture))
          }
)