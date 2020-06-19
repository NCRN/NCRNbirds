#' @include NCRNbirds_Class_def.R 
#' 
#' @title summarizeTrend
#' 
#' @description Analyzes an unmarkedFrame object using methods from the unmarked package.
#' 
#' @importFrom boot inv.logit
#' @importFrom dplyr arrange group_by mutate summarize
#' @importFrom magrittr %>%
#' @importFrom unmarked bup coef confint getData getY predict ranef SE show siteCovs
#' @importFrom purrr map pmap
#' 
#' @param object An unmarked fit object such as those created by \code{unmarkedBirds} or a \code{list} of such frames. 
#' @return A table which summarizes the trend analysis results. Currently fits occupancy models with only year (factor or numeric) as a site covariate. 
#' @details This function simplifies the process of extracting trends from unmarked models (Royal 2004, Kery et al. 2005) generated
#' by the \code{\link{unmarkedBirds}} function. 
#' 
#' The function will return a table, where each row is a differnet year. Columns include, the year, the number of points visited. naive 
#' occupancy for each year, the model coefficient and standard error. The function also returns two sets of estimates of the true occupancy for each year.
#' The first, \emph{psi}, is the inverse logit of the model coefficient and represents the estimated occupancy of a site given its site and visit covariates. 
#' The second estimate, \emph{z}, takes into account not only covariates but also the actual detection history for each site.  Each estimate comes with 
#' its own upper and lower limits for a 95\% CI. Note that as \emph{z} takes into account the actual detection history, its lower limit is 
#' always at least the naive occupancy, whereas \emph{psi} has no such lower boundary.
#'  
#' 
#' @export


########################

setGeneric(name="summarizeTrend",function(object,tabletype="state", ...){standardGeneric("summarizeTrend")},  signature="object")


# setMethod(f="unmarkedBirds", signature=c(object="list"),
#           function(object, formula, ...) {
#             lapply(X=object, FUN=unmarkedBirds, formula=formula, ...)
#           }
# )



setMethod(f="summarizeTrend", signature=c(object="unmarkedFitOccu"),
          function(object,tabletype,...){

  ModelType<-if(all(class(siteCovs(getData(object))$Year)=="factor")) "factor" else "numeric"
 
  ModelData<-getData(object)
 
  
  InFrame<-as.data.frame(getY(ModelData))

  InFrame<-cbind(InFrame, siteCovs(ModelData))%>% mutate(Occupied = Visit1 | Visit2) %>% 
   group_by(Year) %>% summarize(Points=n(), Occupancy=sum(Occupied/Points, na.rm=T))

  
  ModelRanef<-ranef(object)

  RanefMode<-bup(ModelRanef, stat="mean")
  ModeCI<-confint(ModelRanef, level=0.95)
  
  Estimates<-cbind(data.frame(Mode=RanefMode),ModeCI, as.data.frame(siteCovs(ModelData))) %>% 
    group_by(Year) %>% summarize(Estimate=round(mean(Mode),3), Lower=round(mean(`2.5%`),3),Upper=round(mean(`97.5%`),3))
  
  
 # StatePreds<-cbind(predict(object,'state'), siteCovs(ModelData)) %>% 
#   group_by(Year) %>% summarize(Preidct=round(mean(Predicted),3), Lower=round(mean(lower),3),Upper=round(mean(upper),3))

  # State_ranef<-cbind( show(ranef(object)), siteCovs(ModelData) ) # %>% 
  #  group_by(Year) %>% summarize(PredIct=round(mean(Mean),3), Lower=round(mean(`2.5%`),3),Upper=round(mean(`97.5%`),3))
 


 
OutTable<-data.frame(Year=if(ModelType=="factor") levels(siteCovs(ModelData)$Year) else Estimates$Year,
                    Points=InFrame$Points,
                    Naive_Occu=round(InFrame$Occupancy,2),
                    Coef=switch (ModelType,
                      factor = c(coef(object, type="state")[[1]],coef(object, type="state")[[1]]+coef(object, type="state")[-1]) %>% round(2),
                      numeric = (coef(object,"state")[[1]]+coef(object, "state")[[2]]*Estimates$Year) %>% round(2)
                    ),
                    SE=switch(ModelType,
                       factor= c( SE(object, type="state")[1], sqrt( (SE(object, type="state")[1]^2)+ (SE(object, type="state")[-1]^2))) %>% round(2),
                       numeric=sqrt((SE(object, type="state")[[1]]^2)+(SE(object, type="state")[[2]]^2)) %>% round(2)
                    ),
                   z=Estimates$Estimate %>% round(2),
                   z_lower95=Estimates$Lower %>% round(2),
                   z_upper95=Estimates$Upper %>% round(2)

)

 OutTable<-OutTable %>% mutate(psi=inv.logit(Coef) %>% round(2), psi_lower95=inv.logit(Coef-2*SE) %>% round(2), 
                               psi_upper95=inv.logit(Coef+2*SE) %>% round(2)) %>% 
   arrange(Year, Points, Naive_Occu, Coef, SE, psi, psi_lower95, psi_upper95, z, z_lower95, z_upper95)

 OutTable$Year<-if(ModelType=="numeric") OutTable$Year + attr(siteCovs(ModelData)$Year,"scaled:center") else OutTable$Year
    
rownames(OutTable)<-c()

return(OutTable)




            
# Detection table 
# if list a model name            
#for each predictor
# estiamted detection 
# se


            
        

            
  }
)

# setMethod(f="summarizeTrend", signature=c(object="unmarkedFramePCount"),
#           function(object,...){
#             
#             #create the formula from the object if no formula is provided. Uses all covariates by default          
#             
#             
#             obs_formula<-case_when(
#               !is.null(formula) ~ NA_character_,
#               is.null(formula) & is.null(obsCovs(object)) ~ "~1",
#               is.null(formula) & !is.null(obsCovs(object)) ~ paste("~",paste(object %>% obsCovs %>% names,collapse="+"),sep="")
#             )   
#             
#             
#             site_formula<-case_when(
#               !is.null(formula) ~ NA_character_,
#               is.null(formula) & is.null(siteCovs(object)) ~ "~1",
#               is.null(formula) & !is.null(siteCovs(object)) ~ paste("~",paste(object %>% siteCovs %>% names,collapse="+"),sep="")
#             )
#             
#             
#             formula<-if(is.null(formula)) formula(paste(obs_formula, site_formula)) else formula
#             
#             
#             return(pcount(formula=formula, data=object, ...))
#             
#           }
# )