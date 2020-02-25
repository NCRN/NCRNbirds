#' @include NCRNbirds_Class_def.R 
#' @inheritParams getBirds
#' 
#' @title unmarkedBirds
#' 
#' @description Analyzes an unmarkedFrame object using methods from the unmarked package.
#' 
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @importFrom  unmarked obsCovs occu siteCovs

#' @importFrom purrr map pmap


#' @object An unamked frame object such as those created by \code{makeUMF}. 
#' @param formula A formula for the the model. Formula indicates covariates for observation and detection in that order. i.e. ~Day ~Forest_Type  Note that 
#' this is NOT in quotes.
#' 
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
#' 
#' @param output   Either "umf" (the defautlt) or "list". Controls the output when \code{object} is a list. If "umf" the data from all \code{NCRNbirds}
#' objects in the list will be combined into a single unmarked frame. If "list" then the output will be a list of unmarked frames - one for 
#' each \code{NCRNbirds} object in the input list. 
#' @param ... Additonal arguments passed to \code{\link{CountXVisit}} and from there to \code{\link{getBirds}}.
#' 
#' @return Either an \code{unmarkedFrameOccu} or an \code{unmarkedFramePcount} object. 
#' 
#' @details This function simplifies the process for fitting a N-mixture model (Royal 2004, Kery et al. 2005) to data in an NCRNbirds object or a list of such objects. The data is first extracted from the object(s) using the \code{\link{CountXVisit}} funciton. The data is then fed to the \code{\link[unmarked]{pcount}} function and the resutls of the analysis are returned. 
#' 
#' @references Kery, M. Royle, J.A. and Schmid. 2005. Modeling avian abundance from replicated counts using binomail mixutre models. Ecological Applications. 15: 1450-1461. 
#' @references Royle, J. A.  2004. N-Mixture models for estimating population size from spatially replicated counts. Biometrics 60: 108-115.
#' 
#' @export


########################

setGeneric(name="unmarkedBirds",function(object,formula=NULL, parks=NA, points=NA, AOU=NA, years=NA, times=NA, visits=NA, reps=NA, frametype="pcount",sitecovs=NULL, visitcovs=NA, 
                                   obscovslist=NULL, output="umf",... ){standardGeneric("unmarkedBirds")},  signature="object")

# setMethod(f="unmarkedBirds", signature=c(object="list"),
#   function(object, parks, points, AOU, years, times, visits, reps, frametype, sitecovs, visitcovs, obscovslist, output, ...) {
#     switch(output,
#       umf= {
#             BirdMat<-CountXVisit(object=object, parks=parks, points=points, AOU=AOU, years=years, times=times, visits=visits, reps=reps,
#                                  type=switch(frametype, pcount="count", occu="occupancy"),output="dataframe", ...) %>% 
#             dplyr::select(-c(Admin_Unit_Code, Point_Name, Year))
#                  
#             VisitData<- if(!anyNA(visitcovs)){
#               visitcovs %>% map(~CovsXVisit(object, parks=parks, points=points, years=years, times=times, visits=visits, reps=reps, 
#                                             covs=.x, output="dataframe") %>% 
#                             select(-Admin_Unit_Code,-Point_Name,-Year)) %>% 
#                            setNames(visitcovs)
#             }else NULL
#             return(makeUMF(object=BirdMat,frametype=frametype, sitecovs=sitecovs, obscovslist = c(VisitData, obscovslist)))
#       },
#                
#       list={
#             umflist<-list(object, sitecovs, obscovslist)
#             umflist<-umflist[!sapply(umflist, is.null)] # removes any null elements so pmap will work
#             return(pmap(.l=umflist, makeUMF, parks=parks, points=points, years=years,times=times, visits=visits, reps=reps, frametype=frametype, 
#                         visitcovs=visitcovs) )
#       }
#     )
#    }
#  )
#  
#  
# setMethod(f="unmarkedBirds", signature=c(object="NCRNbirds"),
#     function(object, parks, points, AOU, years, times, visits, reps, frametype, sitecovs, visitcovs, obscovslist, ...){
#            
#       BirdMat<-CountXVisit(object=object, parks=parks, points=points, AOU=AOU, years=years, times=times, visits=visits,
#                            reps=reps,  type=switch(frametype, pcount="count", occu="occupancy"), ...) %>% 
#         dplyr::select(-c(Admin_Unit_Code, Point_Name, Year))
#       
#       VisitData<- if(!anyNA(visitcovs)){
#         visitcovs %>% map(~CovsXVisit(object, parks=parks, points=points, years=years, times=times, visits=visits, reps=reps, covs=.x) %>% 
#                       select(-Admin_Unit_Code,-Point_Name,-Year)) %>% 
#           setNames(visitcovs)
#       }else NULL
#  
#    return(makeUMF(object=BirdMat, frametype=frametype,sitecovs=sitecovs, obscovslist = c(VisitData, obscovslist)))
#   }
#  )
# 
# setMethod(f="unmarkedBirds", signature=c(object="data.frame"),
#   function(object, frametype, sitecovs, obscovslist){
#         
#     
#           
#     UMFrame<-switch(frametype,
#                   pcount=unmarkedFramePCount(y=object, siteCovs=sitecovs, obsCovs=obscovslist),
#                   occu=unmarkedFrameOccu(y=object, siteCovs=sitecovs, obsCovs=obscovslist)
#                   )
#     
#     return(UMFrame)
#     
#   }
# )


setMethod(f="unmarkedBirds", signature=c(object="unmarkedFrameOccu"),
          function(object,formula,...){
        
  #create the formula from the object if no formula is provided. Uses all covariates by default          

                  
  obs_formula<-case_when(
    !is.null(formula) ~ NA_character_,
    is.null(formula) & is.null(obsCovs(object)) ~ "~1",
    is.null(formula) & !is.null(obsCovs(object)) ~ paste("~",paste(object %>% obsCovs %>% names,collapse="+"),sep="")
  )   
   

  site_formula<-case_when(
   !is.null(formula) ~ NA_character_,
   is.null(formula) & is.null(siteCovs(object)) ~ "~1",
   is.null(formula) & !is.null(siteCovs(object)) ~ paste("~",paste(object %>% siteCovs %>% names,collapse="+"),sep="")
  )
         
      
  formula<-if(is.null(formula)) formula(paste(obs_formula, site_formula)) else formula

            
  return(occu(formula=formula, data=object, ...))
            
  }
)

setMethod(f="unmarkedBirds", signature=c(object="unmarkedFramePCount"),
          function(object,formula,...){
            
            #create the formula from the object if no formula is provided. Uses all covariates by default          
            
            
            obs_formula<-case_when(
              !is.null(formula) ~ NA_character_,
              is.null(formula) & is.null(obsCovs(object)) ~ "~1",
              is.null(formula) & !is.null(obsCovs(object)) ~ paste("~",paste(object %>% obsCovs %>% names,collapse="+"),sep="")
            )   
            
            
            site_formula<-case_when(
              !is.null(formula) ~ NA_character_,
              is.null(formula) & is.null(siteCovs(object)) ~ "~1",
              is.null(formula) & !is.null(siteCovs(object)) ~ paste("~",paste(object %>% siteCovs %>% names,collapse="+"),sep="")
            )
            
            
            formula<-if(is.null(formula)) formula(paste(obs_formula, site_formula)) else formula
            
            
            return(pcount(formula=formula, data=object, ...))
            
          }
)