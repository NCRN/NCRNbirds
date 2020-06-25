#' @include NCRNbirds_Class_def.R 
#' 
#' @title summarizeTrend
#' 
#' @description Summarizes trends for an unmarkedFit object using methods from the unmarked package.
#' 
#' @importFrom boot inv.logit
#' @importFrom dplyr arrange group_by mutate summarize
#' @importFrom magrittr %>%
#' @importFrom unmarked bup coef confint getData getY predict ranef SE show siteCovs
#' @importFrom purrr map pmap
#' 
#' @param object An unmarked fit object such as those created by \code{\link{unmarkedBirds}} or a \code{list} of such fits. 
#' @return A table which summarizes the trend analysis results. Currently fits occupancy models with only year (factor or numeric) as a site covariate. 
#' If the input is a \code{list} of unmarked fits, then the function will return a \code{list} of table.s
#' @details This function simplifies the process of extracting trends from unmarked models (Royal 2004, Kery et al. 2005) generated
#' by the \code{\link{unmarkedBirds}} function. 
#' 
#' The function will return a table, where each row is a different year. Columns include the year, the number of points visited., naive 
#' occupancy or abundance for each year, the model coefficient and standard error. The function also returns two sets of estimates of the true 
#' occupancy or abundance, by each year. The first, \emph{psi}, is the inverse model coefficient back transformed to occupancy / abundance and 
#' represents the estimated value for  a site given its site and visit covariates. The second estimate, \emph{z}, takes into account not only covariates
#' but also the actual detection history for each site.  Each estimate comes with  its own upper and lower limits for a 95\% CI. Note that as \emph{z} 
#' takes into account the actual detection history, its lower occupancy limit is always at least the naive occupancy, whereas \emph{psi} has no such 
#' lower boundary.
#' 
#' @export


########################

setGeneric(name="summarizeTrend",function(object, ...){standardGeneric("summarizeTrend")},  signature="object")


 setMethod(f="summarizeTrend", signature=c(object="list"),
           function(object, ...) {
             lapply(X=object, FUN=summarizeTrend, ...)
           }
 )



setMethod(f="summarizeTrend", signature=c(object="unmarkedFitOccu"),
          function(object,...){

  ModelType<-if(all(class(siteCovs(getData(object))$Year)=="factor")) "factor" else "numeric"
 
  ModelData<-getData(object)
 
  
  InFrame<-as.data.frame(getY(ModelData))

  InFrame<-cbind(InFrame, siteCovs(ModelData))%>% mutate(Occupied = Visit1 | Visit2) %>% 
   group_by(Year) %>% summarize(Points=n(), Occupancy=sum(Occupied/Points, na.rm=T))

  
  ModelRanef<-ranef(object)

  RanefMean<-bup(ModelRanef, stat="mean")
  RMeanCI<-confint(ModelRanef, level=0.95)
  
  Estimates<-cbind(data.frame(Mode=RanefMean),RMeanCI, as.data.frame(siteCovs(ModelData))) %>% 
    group_by(Year) %>% summarize(Estimate=round(mean(Mode),3), Lower=round(mean(`2.5%`),3),Upper=round(mean(`97.5%`),3))
  
 
  OutTable<-data.frame(Year=if(ModelType=="factor") as.numeric(levels(siteCovs(ModelData)$Year)) else Estimates$Year,
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

 OutTable<-OutTable %>% mutate(psi=inv.logit(Coef) %>% round(2), psi_lower95=inv.logit(Coef-1.96*SE) %>% round(2), 
                               psi_upper95=inv.logit(Coef+1.96*SE) %>% round(2)) %>% 
   arrange(Year, Points, Naive_Occu, Coef, SE, psi, psi_lower95, psi_upper95, z, z_lower95, z_upper95)

 OutTable$Year<-if(ModelType=="numeric") OutTable$Year + attr(siteCovs(ModelData)$Year,"scaled:center") else OutTable$Year
    
rownames(OutTable)<-c()

return(OutTable)

  }
)

setMethod(f="summarizeTrend", signature=c(object="unmarkedFitPCount"),
  function(object,...){
    
    ModelType<-if(all(class(siteCovs(getData(object))$Year)=="factor")) "factor" else "numeric"
    
    ModelData<-getData(object)
       
    InFrame<-as.data.frame(getY(ModelData))
    
    InFrame<-cbind(InFrame, siteCovs(ModelData))%>% mutate(Max = pmax(Visit1,Visit2, na.rm=T)) %>% 
      group_by(Year) %>% summarize(Points=n(), Mean_Count=mean(Max, na.rm=T))         
             
    ModelRanef<-ranef(object)
    RanefMean<-bup(ModelRanef, stat="mean")
    RMeanCI<-confint(ModelRanef, level=0.95)
    
    Estimates<-cbind(data.frame(Mode=RanefMean), RMeanCI, as.data.frame(siteCovs(ModelData))) %>% 
      group_by(Year) %>% summarize(Estimate=round(mean(Mode),3), Lower=round(mean(`2.5%`),3),Upper=round(mean(`97.5%`),3))
    
    
    OutTable<-data.frame(Year=if(ModelType=="factor") as.numeric(levels(siteCovs(ModelData)$Year)) else Estimates$Year,
      Points=InFrame$Points,
      Naive_Mean_Count=round(InFrame$Mean_Count,2),
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
    OutTable<-OutTable %>% mutate(psi=exp(Coef) %>% round(2), psi_lower95=exp(Coef-1.96*SE) %>% round(2), 
        psi_upper95=exp(Coef+1.96*SE) %>% round(2)) %>% 
      arrange(Year, Points, Naive_Mean_Count, Coef, SE, psi, psi_lower95, psi_upper95, z, z_lower95, z_upper95)
    
    OutTable$Year<-if(ModelType=="numeric") OutTable$Year + attr(siteCovs(ModelData)$Year,"scaled:center") else OutTable$Year
    
    rownames(OutTable)<-c()
    
    return(OutTable)         

    }
 )