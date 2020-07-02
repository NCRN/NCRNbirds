#' @include NCRNbirds_Class_def.R summarizeEffort.R  mapBirds.R
#' 
#' @title mapEffort
#' 
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' 
#' @description Produces an html map of the number of visits made to each point.
#' 
#' @param object Either an object of class \code{NCRNbirds} or a list of such objects.
#' @param palette Color pallete for the colors of the points. Defaults to "BuGn" (blue green) but will accept any RColorBrewer, viridis or custom palette.
#' @param maptype The type of base map to be used. See \code{\link{mapBirds}} for options.
#' @param title  A character vector to be used as the legend title. 
#' @param ... Additional arguments passed to \code{\link{summarizeEffort}}. 
#' 
#' @details  This function prodcues a map showing number of visits to each point for one or more years. The number of visits 
#' is calculated using the \code{\link{summarizeEffort}} function, and this data is then passed on to the \code{\link{mapBirds}} 
#' function. 
#' 
#' @seealso \code{\link{summarizeEffort}} \code{\link{mapBirds}}
#' 
#' @export

setGeneric(name="mapEffort",function(object, years=NA, points=NA, palette="BuGn",maptype="basic",
        title="Visits per Point", ...){standardGeneric("mapEffort")}, signature="object")


 setMethod(f="mapEffort", signature=c(object="list"),
           function(object, years, points, palette, maptype,title,...){
             
             Effort<-summarizeEffort(object,byPoint=T, byYear=F, effort="visits", ...)
             
             Effort<-Effort %>%  mutate(Point_Counts=factor(Point_Counts, ordered = T, levels=sort(unique(Effort$Point_Counts))))
            
             
               mapBirds(object=object, points=Effort$Point_Name, values=Effort$Point_Counts,  colortype = "factor", colors=palette, 
                      maptype = maptype, title=title) 
           
   })



setMethod(f="mapEffort", signature=c(object="NCRNbirds"),
  function(object,palette, maptype, title,...){
      
    Effort<-summarizeEffort(object,byPoint=T, byYear=F, effort="visits", ...)  
    Effort<-Effort %>%  mutate(Point_Counts=factor(Point_Counts, ordered = T, levels=sort(unique(Effort$Point_Counts))))
    
    
    mapBirds(object=object, points=Effort$Point_Name, values=Effort$Point_Counts,  colortype = "factor", colors=palette, 
       maptype = maptype, title=title)    
            
                           
})



