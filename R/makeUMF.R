#' @include NCRNbirds_Class_def.R CountXVisit.R CovsXVisit.R
#' @inheritParams getBirds
#' 
#' @title makeUMF
#' 
#' @description Makes an unmarked frame for analysis using the unmarked package
#' 
#' @importFrom dplyr bind_cols select transmute
#' @importFrom magrittr %>%
#' @importFrom purrr map pmap
#' @importFrom unmarked unmarkedFrameOccu unmarkedFramePCount
#'
#' @param frametype Indicates the type of \code{unmarkedFrame} to make. Options are "pcount" the default incudes number of decections for n-mixture modeling. 
#' "occu" is used for occupancy modeling. 
#' @param sitecovs A \code{data.frame} of site covariates created by the user.Each covariate shoulb be in a separate olumn and point in its own row.
#' This should not include a column of point names. Make sure that the order of the points in this \code{data.frame} math the order in the observation data. 
#' If the input \code{object} is a \code{list} and the desired output is a \code{list} than \code{sitecovs} should also be a \code{list} of \code{data.frames}
#' with one \code{data.frame} for each park.
#' @param visitcovs A characeter vector with names of visit covariates to be used which ARE obtained from the \code{\link{CovsXVisit}} function.\code{makeUMF}
#' will automtically format these for use in unmarkedFrames. These will be formatted at a list of named \code{data.frames}
#' @param obscovslist A named \code{list} of \code{data.frame}s with observation covariates. Typically this will include covariates that ARE NOT returned 
#' by the \code{\link{CovsXVisit}} function.This will be used in conjuciton with the visit covariates specified by \code{visitcovs}.If the input \code{object}
#' is a \code{list} and the desired output is a \code{list} than \code{obscovslist} should also be a \code{list} of \code{lists}
#' with one \code{list} of observation covariates for each park.
#' @param trend Used for multi-year data sets to indicate if the year of obsevation shoud be included in the model. Options are "none" the default, "numeric"
#' to indicate that year should be treated as a numeric value and "factor" to indicate that year should be treated as a factor. Note that is the trend is set
#' to "numeric" it is centered to imporve the odds of the model successfully fitting.
#' @param output   Either "umf" (the defautlt) or "list". Controls the output when \code{object} is a list. If "umf" the data from all \code{NCRNbirds}
#' objects in the list will be combined into a single unmarked frame. If "list" then the output will be a list of unmarked frames - one for 
#' each \code{NCRNbirds} object in the input list. 
#' @param ... Additonal arguments passed to \code{\link{CountXVisit}} and from there to \code{\link{getBirds}}.
#' 
#' @return Either an \code{unmarkedFrameOccu} or an \code{unmarkedFramePcount} object. 
#' 
#' @details This function simplifies the process for fitting of occupancy or N-mixture models (Royal 2004, Kery et al. 2005) to data in an NCRNbirds object 
#' or a list of such objects. The data is first extracted from the object(s) using the \code{\link{CountXVisit}} funciton. The data is then fed to 
#' the \code{\link[unmarked]{unmarkedFramePCount}} or  \code{\link[unmarked]{unmarkedFrameOccu}}function. The resulting object can then be passed to \
#' \code{\link{unmarkedBirds}} for analysis. 
#' 
#' @references Kery, M. Royle, J.A. and Schmid. 2005. Modeling avian abundance from replicated counts using binomail mixutre models. Ecological Applications. 15: 1450-1461. 
#' @references Royle, J. A.  2004. N-Mixture models for estimating population size from spatially replicated counts. Biometrics 60: 108-115.
#' 
#' @export


########################

setGeneric(name="makeUMF",function(object, parks=NA, points=NA, AOU=NA, years=NA, times=NA, visits=NA, reps=NA, frametype="pcount",sitecovs=NULL, visitcovs=NA, 
                                   obscovslist=NULL, trend="none", output="umf",... ){standardGeneric("makeUMF")},  signature="object")

setMethod(f="makeUMF", signature=c(object="list"),
  function(object, parks, points, AOU, years, times, visits, reps, frametype, sitecovs, visitcovs, obscovslist, trend, output, ...) {
    switch(output,
      umf= {
            BirdMat<-CountXVisit(object=object, parks=parks, points=points, AOU=AOU, years=years, times=times, visits=visits, reps=reps,
                                 type=switch(frametype, pcount="count", occu="occupancy"),output="dataframe", ...) %>% 
            dplyr::select(-c(Admin_Unit_Code, Point_Name))
                 
            VisitData<- if(!anyNA(visitcovs)){
              visitcovs %>% map(~CovsXVisit(object, parks=parks, points=points, years=years, times=times, visits=visits, reps=reps, 
                                            covs=.x, output="dataframe") %>% 
                            select(-Admin_Unit_Code,-Point_Name,-Year)) %>% 
                           setNames(visitcovs)
            }else NULL
            
            YearMat<-switch(trend,
                            none=NULL,
                            numeric=BirdMat %>% transmute(Year=scale(Year,scale=FALSE)),
                            factor=BirdMat %>% transmute(Year=factor(Year))
                           )
            
          sitecovs<-if(trend!="none") bind_cols(YearMat,sitecovs) else sitecovs #bind_cols used as cbind has issues with NULL
          
          return(makeUMF(object=BirdMat %>% select(-Year),frametype=frametype, sitecovs=sitecovs, obscovslist = c(VisitData, obscovslist)))
      },
               
      list={
            umflist<-list(object, sitecovs, obscovslist)
            umflist<-umflist[!sapply(umflist, is.null)] # removes any null elements so pmap will work
            return(pmap(.l=umflist, makeUMF, parks=parks, points=points,AOU=AOU, years=years,times=times, visits=visits, reps=reps, frametype=frametype, 
                        visitcovs=visitcovs, trend=trend) )
      }
    )
   }
 )
 
 
setMethod(f="makeUMF", signature=c(object="NCRNbirds"),
    function(object, parks, points, AOU, years, times, visits, reps, frametype, sitecovs, visitcovs, obscovslist, trend,...){
           
      BirdMat<-CountXVisit(object=object, parks=parks, points=points, AOU=AOU, years=years, times=times, visits=visits,
                           reps=reps,  type=switch(frametype, pcount="count", occu="occupancy"), ...) %>% 
        dplyr::select(-c(Admin_Unit_Code, Point_Name))
      
      VisitData<- if(!anyNA(visitcovs)){
        visitcovs %>% map(~CovsXVisit(object, parks=parks, points=points, years=years, times=times, visits=visits, reps=reps, covs=.x) %>% 
                      select(-Admin_Unit_Code,-Point_Name)) %>% 
          setNames(visitcovs)
      }else NULL
      
      YearMat<-switch(trend,
                      none=NULL,
                      numeric=BirdMat %>% transmute(Year=scale(Year,scale=FALSE)),
                      factor=BirdMat %>% transmute(Year=factor(Year))
      )
      sitecovs<-if(trend!="none") bind_cols(YearMat,sitecovs) else sitecovs #bind_cols used as cbind has issues with NULL
      
   return(makeUMF(object=BirdMat %>% select(-Year), frametype=frametype,sitecovs=sitecovs, obscovslist = c(VisitData, obscovslist)))
  }
 )

setMethod(f="makeUMF", signature=c(object="data.frame"),
  function(object, frametype, sitecovs, obscovslist){
        
    
          
    UMFrame<-switch(frametype,
                  pcount=unmarkedFramePCount(y=object, siteCovs=sitecovs, obsCovs=obscovslist),
                  occu=unmarkedFrameOccu(y=object, siteCovs=sitecovs, obsCovs=obscovslist)
                  )
    
    return(UMFrame)
    
  }
)