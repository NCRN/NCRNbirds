#' @include NCRNbirds_Class_def.R BCI.R getPoints.R mapBirds.R
#' 
#' @title mapBCI
#' 
#' @importFrom dplyr left_join mutate select
#' @importFrom magrittr %>%
#' 
#' @description Produces an html map of BCI Cateogries for a given year.
#' 
#' @param object Either an object of class \code{NCRNbirds} or a list of such objects
#' @param years A single numeric value indicating the year of the data you wish to map. 
#' @param points A character vector of point names. /code{NA}, the default, will map all points.
#' @param type A mandatory length 1 character \code{vector} that indicates the type of BCI to calculate. Can be "Cent_Appal", the default, or "NETN_Forest_BCI"
#' @param palette Color pallete for the colros of the ponits Defaults to "BuGn" (blue green) but will accept any RColorBrewer, viridis or custom palette.
#' @param maptype The type of base map to be used. See \code{\link{mapBirds}} for options
#' @param title  A character vector to be used as the lengend title. 
#' @param ... Additional arguments passed to \code{\link{BCI}}. 
#' 
#' @details  This function prodcues a map showing the BCI category of each point for a given year. BCI values and categories are calculated using the 
#' \code{\link{BCI}} function, and this data is then passed on to the \code{\link{mapBirds}} function. AS the BCI value makes little sense when calculated 
#' using multiple years of data, only a single year should be indicated in the \code{years} argument. 
#' 
#' @seealso \code{\link{BCI}} \code{\link{mapBirds}}
#' 
#' 
#' @export

setGeneric(name="mapBCI",function(object, years, points=NA, type="Cent_Appal",palette="BuGn",maptype="basic",
                      title="BCI Category", ...){standardGeneric("mapBCI")}, signature="object")


setMethod(f="mapBCI", signature=c(object="list"),
          function(object, years, points, type, palette, maptype,title,...){
            
            InBCI<-BCI(object,years=years, points=points, type=type, ...)
            InPoints<-getPoints(object=object, years=years, points=points)
            
            BCIdata<-left_join(InPoints,InBCI %>% select(Point_Name,BCI, BCI_Category))
            
            BCIdata <- BCIdata %>% 
              mutate(BCI_Cuts=cut(BCI, breaks =c(0,40.1,52.1,60.1,77.1), 
                                  labels=c("Low Integrity", "Medium Integrity", "High Integrity", "Highest Integrity")))
            
            mapBirds(object=object, points=BCIdata$Point_Name, values=BCIdata$BCI_Cuts,  colortype = "factor", colors=palette, 
                     maptype = maptype, title=title)    
  })



setMethod(f="mapBCI", signature=c(object="NCRNbirds"),
  function(object, years, points, type, palette, maptype,title,...){
      
    InBCI<-BCI(object,years=years, points=points, type=type, ...)
    InPoints<-getPoints(object=object, years=years, points=points)
            
    BCIdata<-left_join(InPoints,InBCI %>% select(Point_Name,BCI, BCI_Category))
        
    BCIdata <- BCIdata %>% 
      mutate(BCI_Cuts=cut(BCI, breaks =c(0,40.1,52.1,60.1,77.1), 
                          labels=c("Low Integrity", "Medium Integrity", "High Integrity", "Highest Integrity")))
    
    mapBirds(object=object, points=BCIdata$Point_Name, values=BCIdata$BCI_Cuts,  colortype = "factor", colors=palette, 
             maptype = maptype, title=title)    
            
                           
})



