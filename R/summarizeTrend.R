#' @include NCRNbirds_Class_def.R 
#' 
#' @title summarizeTrend
#' 
#' @description Analyzes an unmarkedFrame object using methods from the unmarked package.
#' 
#' @importFrom boot inv.logit
#' @importFrom dplyr group_by mutate summarize
#' @importFrom magrittr %>%
#' @importFrom unmarked bup coef confint getData getY predict ranef SE show siteCovs

#' @importFrom purrr map pmap


#' @param object An unamked frame object such as those created by \code{makeUMF} or a \code{list} of such frames. 
#' @return A table which summarizes the trend analysis results. Currently fits occupancy and n-mixture models. 
#' @details This function simplifies the process of extracting trends over tiem from unmarked models (Royal 2004, Kery et al. 2005) generated
#' by the \code{\link{unmarkedBirds}} command. The code currenlty assumes you have an occupancy model fit with the trend Year as a factor. 
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

  ModelType<-if(class(siteCovs(getData(object))$Year)=="factor") "factor" else "numeric"
 
  ModelData<-getData(object)
 
  
  InFrame<-as.data.frame(getY(ModelData))
  InFrame<-cbind(InFrame, siteCovs(ModelData))%>% mutate(Occupied = Visit1 | Visit2) %>% 
   group_by(Year) %>% summarize(Sites=n(), Occupancy=sum(Occupied/Sites, na.rm=T))

  ModelRanef<-ranef(object)
  RanefMode<-bup(ModelRanef, stat="mean")
  ModeCI<-confint(ModelRanef, level=0.95)
  
  Estimates<-cbind(data.frame(Mode=RanefMode),ModeCI, siteCovs(ModelData)) %>% 
    group_by(Year) %>% summarize(Estimate=round(mean(Mode),3), Lower=round(mean(`2.5%`),3),Upper=round(mean(`97.5%`),3))
 # StatePreds<-cbind(predict(object,'state'), siteCovs(ModelData)) %>% 
#   group_by(Year) %>% summarize(Preidct=round(mean(Predicted),3), Lower=round(mean(lower),3),Upper=round(mean(upper),3))

  # State_ranef<-cbind( show(ranef(object)), siteCovs(ModelData) ) # %>% 
  #  group_by(Year) %>% summarize(PredIct=round(mean(Mean),3), Lower=round(mean(`2.5%`),3),Upper=round(mean(`97.5%`),3))
 


 
OutTable<-data.frame(Year=if(ModelType=="factor") levels(siteCovs(ModelData)$Year) else "All" ,
                  Sites=InFrame$Sites,
                  Naive_Occu=round(InFrame$Occupancy,2),
                  Coef=c(coef(object, type="state")[1],coef(object, type="state")[1]+coef(object, type="state")[-1]),
                  Estimate=Estimates$Estimate,
                  Lower95=Estimates$Lower,
                  Upper95=Estimates$Upper

)

rownames(OutTable)<-c()
return(OutTable)


#States
#If list a model nam  e          
            # naive occupancy
# sites
# estimated occupanyc
# SE

            
# Detection table 
# if list a model name            
#for each predictor
# estiamted detection 
# se

            
#Site Table?
#if list a model name
#Visits
#naive estiamte
# probablity of occupnayc
# visit specific detectability
            
        

            
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