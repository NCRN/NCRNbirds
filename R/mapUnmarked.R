#' @include NCRNbirds_Class_def.R birdRichness.R  mapBirds.R
#' 
#' @title mapUnmarked
#' 
#' @description Produces an html map of bird species richness.
#' 
#' @importFrom boot inv.logit
#' @importFrom unmarked getData predict siteCovs
#' 
#' @param object Either an object of class \code{NCRNbirds} or a list of such objects.
#' @param years Optional. A vector of numeric value(s) indicating the year of the data you wish to map. \code{NA}, the default, combines data across all
#' years.
#' @param points A character vector of point names. \code{NA}, the default, will map all points.
#' @param palette Color pallete for the colors of the points. Defaults to "BuGn" (blue green) but will accept any RColorBrewer, viridis or custom palette.
#' @param maptype The type of base map to be used. See \code{\link{mapBirds}} for options.
#' @param title  A character vector to be used as the legend title. 
#' @param ... Additional arguments passed to \code{\link{birdRichness}}. 
#' 
#' @details  This function prodcues a map showing the bird species richness of each point for one or more years. Bird species richness is calculated using the 
#' \code{\link{birdRichness}} function, and this data is then passed on to the \code{\link{mapBirds}} function. 
#' 
#' @seealso \code{\link{birdRichness}} \code{\link{mapBirds}}
#' 
#' 
#' @export

setGeneric(name="mapUnmarked",function(object, model, years=NA, points=NA, palette="BuGn",maptype="basic",
        title="Species Richness", ...){standardGeneric("mapUnmarked")}, signature="model")


 setMethod(f="mapUnmarked", signature=c(model="list"),
           function(object, model, years, points, palette, maptype,title,...){
             
             
             BRich<-birdRichness(object, years=years, points=points, byPoint=T, ...)
             
             mapBirds(object=object, points=BRich$Point_Name, values=BRich$Richness,  colortype = "numeric", colors=palette, 
                      maptype = maptype, title=title)     
   })



setMethod(f="mapUnmarked", signature=c(model="unmarkedFit"),
  function(object, model, years, points, palette, maptype, title,...){
      
 SiteData<- cbind(siteCovs(getData(model)), predict(model, "state")[1])
 
 SiteData<- if("Year" %in% names(SiteData) & class(SiteData$Year)=="matrix"){
    SiteData %>% mutate(Year=Year+attr(Year,"scaled:center"))} else SiteData

 
 SiteData<- if("Year" %in% names(SiteData) & !anyNA(years)){
   SiteData %>% filter(Year %in% years)} else SiteData

 
SiteData$Estimate <-switch(class(model)[1],
                             unmarkedFitOccu=inv.logit(SiteData$Predicted) %>% round(2),
                             unmarkedFitPCount=exp(SiteData$Predicted) %>% round(2)
)
 
 
mapBirds(object=object, points=SiteData$Point_Name, values=SiteData$Estimate,  colortype = "numeric", colors=palette, 
         maptype = maptype, title=title)
                           
})



