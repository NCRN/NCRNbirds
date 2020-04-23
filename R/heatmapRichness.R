#' @include NCRNbirds_Class_def.R birdRichness.R getDesign.R getVisits.R
#'   
#' @title heatmapRichness
#'
#' @importFrom ggplot2 aes element_blank element_rect element_text geom_raster geom_text ggplot ggtitle labs scale_fill_gradient theme unit
#' @importFrom purrr map
#' 
#' @description Makes a heamap of species richness
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{birdRichness()}.
#' 
#' @param color A color that will correspond to the maximum species richness
#' @param output Either "total" (the default) or "list". Only used when \code{object} is a \code{list}
#' 
#' 
#' @param years  A numeric vector. Indicates which years should be graphed.
#' @param points A character vector of point names. Only these points will be used.
#' @param visits A length 1 numeric vector, defaults to NA. Returns data only from the indicated visits.
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been visited is greater 
#' or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param plot_title  Optional,  A title for the plot. 
#' @param point_num An optional numeric vector indicating the number of points sampled each year. If \code{object} is a \code{NCRNbirds} object
#' or a \code{list} of such objects, then this will be calculated automatically. If \code{object} is a \code{data.frame} than this can be
#' provided by the user. 
#' @param ... Additional arguments passed to \code{\link{birdRichness}}
#' @param add_line Logical. Connects points in plot when \code{TRUE}. Defaults to \code{TRUE}.
#' @param plot Logical. Return plot \code{TRUE} (default) or data.frame \code{FALSE}. 
#' @details This function produces a graph of species richness over time. It does this by using the output of the \code{\link{birdRichness}}
#' function. The data is then passed on to ggplot2 for graphing.
#'   
#' @export


setGeneric(name="heatmapRichness",function(object, color="dark green" ,years=NA, points=NA, visits = NA, times=NA, plot_title=NA, point_num=NA, add_line = TRUE, 
                                           output="total", plot=TRUE, ...){standardGeneric("heatmapRichness")}, signature="object")


setMethod(f="heatmapRichness", signature=c(object="list"),
   function(object, color, output) {
     switch(output,
       total={
          heatmapdata=birdRichness(object,byPark=T, byYear=T, output="total")

        return(heatmapRichness(object=heatmapdata, color=color))
      },
       list={
         return(map(object,heatmapRichness, color=color))
       }
     )
 })
 
 setMethod(f="heatmapRichness", signature=c(object="NCRNbirds"),
   function(object, color){
  heatmapdata<-birdRichness(object, byPark=T, byYear = T)
  
  heatmapRichness(heatmapdata, color)

 
 })

setMethod(f="heatmapRichness", signature=c(object="data.frame"),
  function(object, color){#, plot_title, point_num,add_line){
   
    maxRichness<-max(object$Richness)
    
    heatmapOut<-ggplot(data=object, aes(x=as.factor(Year), y=Admin_Unit_Code, fill=Richness)) + 
      geom_raster() +
       theme(panel.background = element_rect(fill="transparent"),
             panel.border= element_rect(color="white", fill="transparent"),
             axis.text.x = element_text(size=10),
             axis.text.y = element_text(size=10),
             title = element_text(size=12),
             legend.key.width=unit(2,"lines"),
             legend.key.height=unit(3,"lines"))+
       scale_fill_gradient(low = "white", high = color, na.value="gray",
                           limits=c(0, ceiling(maxRichness)),
                           breaks=c(round(seq(0,maxRichness, length.out=5),0)))+
        labs(x=element_blank(), y="Park")+
        geom_text(aes(label = round(Richness, 0)), size = 3,color="black")+
       ggtitle("Species Richness by Park and Year")
    
print(heatmapOut)
    
})
