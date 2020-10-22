#' @include NCRNbirds_Class_def.R birdRichness.R  mapBirds.R
#' 
#' @title mapUnmarked
#' 
#' @description Produces an html map of fits for an unmarked fit. 
#' 
#' @importFrom boot inv.logit
#' @importFrom unmarked bup getData predict ranef siteCovs
#' 
#' @param object Either an object of class \code{NCRNbirds} or a list of such objects.
#' @param model An unmarked fit object such as those created by \code{\link{unmarkedBirds}}.
#' @param estimate Either "psi", the default, or "z". Inidicates the type of esimate you wish to display.
#' @param years A vector of a single numeric value indicating the year of the estimates you wish to map. Do not include multiple years
#'  as the map shows preictios for a particular year. 
#' @param points A character vector of point names. \code{NA}, the default, will map all points.
#' @param palette Color pallete for the colors of the points. Defaults to "BuGn" (blue green) but will accept any RColorBrewer, viridis or custom palette.
#' @param maptype The type of base map to be used. See \code{\link{mapBirds}} for options.
#' @param title  A character vector to be used as the legend title. 
#' 
#' @details  This function prodcues a map showing model fits for unmarked fit objects for a given year, as produced by \code{\link{unmarkedBirds}}. In order to produce
#' the map the function requries you to specify which year's fits you are interestred in. The fits are then passed on to the \code{\link{mapBirds}} function. 
#' 
#' @seealso \code{\link{unmarkedBirds}} \code{\link{mapBirds}}
#' 
#' 
#' @export

setGeneric(name="mapUnmarked",function(object, model,  estimate="psi", years=NA, points=NA, palette="BuGn",maptype="basic",
        title="Model Estimates"){standardGeneric("mapUnmarked")}, signature="model")



setMethod(f="mapUnmarked", signature=c(model="unmarkedFit"),
  function(object, model, estimate, years, points, palette, maptype, title){
    
      
  SiteCovData<-  siteCovs(getData(model))
  
  SiteCovData<- if(any("Year" %in% names(SiteCovData) & class(SiteCovData$Year)=="matrix")){
    SiteCovData %>% mutate(Year=Year+attr(Year,"scaled:center"))} else SiteCovData
  

  if(length(model@sitesRemoved )>0) SiteCovData<-SiteCovData[-model@sitesRemoved,]  #removed sites dropped from model.
                        
                                                
  SiteData<- cbind(SiteCovData, predict(model, "state")[1])

 
  SiteData$z<-bup(ranef(model), stat="mean")
 

  SiteData<- if("Year" %in% names(SiteData) & !anyNA(years)){
   SiteData %>% filter(Year %in% years)} else SiteData

 
  SiteData$psi <-switch(class(model)[1],
                             unmarkedFitOccu=inv.logit(SiteData$Predicted) %>% round(2),
                             unmarkedFitPCount=exp(SiteData$Predicted) %>% round(2)
)
 
 
mapBirds(object=object, points=SiteData$Point_Name, 
         values=switch(estimate,
                       psi=SiteData$psi,
                       z= SiteData$z), 
         colortype = "numeric", colors=palette, maptype = maptype, title=title)
                           
})



