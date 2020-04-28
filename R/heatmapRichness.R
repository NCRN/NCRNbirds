#' @include NCRNbirds_Class_def.R birdRichness.R getDesign.R getVisits.R
#'   
#' @title heatmapRichness
#'
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes element_blank element_rect element_text geom_text geom_raster ggplot ggtitle labs scale_fill_gradient
#' @importFrom ggplot2 scale_y_discrete theme unit
#' @importFrom magrittr %>% 
#' @importFrom purrr map
#' 
#' @description Makes a heamap of species richness
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{\link{birdRichness}}.
#' @param byPoint Defaults to \code{FALSE}. If \code{TRUE} heatamap will have one line for each point, otherwise will have one line for each park
#' @param color A color that will correspond to the maximum species richness
#' @param plot_title  Optional,  A title for the plot. 
#' @param scale If \code{TRUE} will include a scale to the right of the heatmap
#' @param labels If\code{TRUE} will label species richness values on the heatmap.
#' @param output Either "total" (the default) or "list". Only used when \code{object} is a \code{list}
#' @param plot Logical. Return plot \code{TRUE} (default) or data.frame \code{FALSE}. 
#' @param ... Additional arguments passed to \code{\link{birdRichness}}


#' @details This function produces a heatmap of species richness at the park or point level across years. It does this by using the 
#' output of the \code{\link{birdRichness}} function. The data is then passed on to ggplot2 for graphing.
#'   
#' @export


setGeneric(name="heatmapRichness",function(object, byPoint=F, color="dark green", plot_title=NA, scale=T, labels=T, output="total",
                                           plot=TRUE, ...){standardGeneric("heatmapRichness")}, signature="object")


setMethod(f="heatmapRichness", signature=c(object="list"),
   function(object, byPoint, color, plot_title, scale, labels, output, plot,  ...) {
     switch(output,
       total={
          heatmapdata=birdRichness(object,byPark=T, byYear=T, byPoint=byPoint, output="total",...)

        return(heatmapRichness(object=heatmapdata, byPoint=byPoint, color=color, plot_title=plot_title, scale=scale, labels=labels, plot = plot))
      },
       list={
         return(map(object,heatmapRichness, byPoint=byPoint, color=color, plot_title=plot_title, scale=scale, labels=labels, plot=plot, ...))
       }
     )
 })
 
 setMethod(f="heatmapRichness", signature=c(object="NCRNbirds"),
   function(object, byPoint, color, plot_title, scale, labels, plot, ...){
  heatmapdata<-birdRichness(object, byPark=T, byYear=T, byPoint=byPoint, ...)
  
  heatmapRichness(object=heatmapdata, byPoint=byPoint, color=color, plot_title = plot_title, scale=scale, labels=labels, plot=plot)

 
 })

setMethod(f="heatmapRichness", signature=c(object="data.frame"),
  function(object,byPoint, color, plot_title, scale, labels, plot){
    
    if(!plot) return(object)
    object<-object %>% mutate(ParkName=factor(ParkName)) %>% 
      {if(byPoint) mutate(.,Point_Name=factor(Point_Name)) else .}

    maxRichness<-max(object$Richness)
    
    heatmapOut<-ggplot(data=object, aes(x=as.factor(Year), y=if(byPoint) Point_Name else ParkName, fill=Richness)) + 
      geom_raster() +
        theme(panel.background = element_rect(fill="transparent"),
              panel.border= element_rect(color="white", fill="transparent"),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              title = element_text(size=12),
              legend.key.width=unit(2,"lines"),
              legend.key.height=unit(3,"lines"))+
        scale_fill_gradient(low = "white", high = color, na.value="gray", guide=if (scale) 'colorbar' else FALSE,
                            limits=c(0, ceiling(maxRichness)),
                            breaks=c(round(seq(0,maxRichness, length.out=5),0)))+
       scale_y_discrete(limits=rev(if(byPoint) levels(object$Point_Name) else levels(object$ParkName)))+
       labs(x=element_blank(), y=element_blank())+
       {if(labels) geom_text(aes(label = round(Richness, 0)), size = 3,color="black")}+
       {if(!is.na(plot_title)) ggtitle(plot_title)}
    
print(heatmapOut)
    
})
