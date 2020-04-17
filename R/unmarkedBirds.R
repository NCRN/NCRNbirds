#' @include NCRNbirds_Class_def.R 
#' 
#' @title unmarkedBirds
#' 
#' @description Analyzes an unmarkedFrame object using methods from the unmarked package.
#' 
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @importFrom unmarked obsCovs occu pcount siteCovs

#' @importFrom purrr map pmap


#' @param object An unamked frame object such as those created by \code{makeUMF} or a \code{list} of such frames. 
#' @param formula A formula for the the model. Formula indicates covariates for observation and detection in that order. i.e. ~Day ~Forest_Type  Note that 
#' this is NOT in quotes.
#' @param ... Additonal arguments passed to the relevant unmarked model fitting method. 
#' @return A fitted unmaked model. Currently fits occupancy and n-mixture models. 
#' 
#' @details This function simplifies the process unmarked models (Royal 2004, Kery et al. 2005) to data in an NCRNbirds object or a list of such objects.
#' The data is preppared by first using the \code{\link{makeUMF}} function to make the proper type of \code{unmarkedFrame}. The fraame can then be fed into
#' \code(unmarkedBirds) for analysis. This function will automatically detect the frame type and do the corresponding analysis. If a formula is not supplied, 
#' then all visit covariates in the unmarkedFrame will be used as detection covariates, and all site covaritates will be used as state (abundance or occupancy) 
#' by defaults. 
#' 
#' @references Kery, M. Royle, J.A. and Schmid. 2005. Modeling avian abundance from replicated counts using binomail mixutre models. Ecological Applications. 15: 1450-1461. 
#' @references Royle, J. A.  2004. N-Mixture models for estimating population size from spatially replicated counts. Biometrics 60: 108-115.
#' 
#' @export


########################

setGeneric(name="unmarkedBirds",function(object,formula=NULL, ...){standardGeneric("unmarkedBirds")},  signature="object")


setMethod(f="unmarkedBirds", signature=c(object="list"),
          function(object, formula, ...) {
            lapply(X=object, FUN=unmarkedBirds, formula=formula, ...)
          }
)



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