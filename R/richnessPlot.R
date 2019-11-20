#' @include NCRNbirds_Class_def.R
#' @include birdRichness.R
#'   
#' @title richnessPlot
#'
#' @importFrom dplyr pull
#' @importFrom ggplot2 aes element_line geom_point ggplot ggtitle labs scale_x_continuous theme theme_minimal scale_colour_viridis_d expand_limits geom_line
#' @importFrom magrittr %>% 
#' @importFrom purrr map pmap
#' @importFrom tidyr full_seq
#' 
#' @description Plots species richness over time.
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{birdRichness()}.
#' @param years  A numeric vector. Indicates which years should be graphed.
#' @param points A character vector of point names. Only these points will be used.
#' @param visits A length 1 numeric vector, defaults to NA. Returns data only from the indicated visits.
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param plot_title  Optional,  A title for the plot. 
#' @param point_num An optional numeric vector indicating the number of points sampled each year. If \code{object} is a \code{NCRNbirds} object
#' or a \code{list} of such objects, then this will be calculated automatically. If \code{object} is a \code{data.frame} than this can be
#' provided by the user. 
#' @param output Either "total" (the default) or "list". Only used when \code{object} is a \code{list}. 
#' @param ... Additional arguments passed to \code{\link{birdRichness}}
#' @param add_line Logical. Connects points in plot when \code{TRUE}. Defaults to \code{TRUE}.
#' @details This function produces a graph of species richness over time. It does this by using the output of the \code{\link{birdRichness}}
#' function. The data is then passed on to ggplot2 for graphing.
#'   
#' @export


setGeneric(name="richnessPlot",function(object,years=NA, points=NA, visits = NA, times=NA, plot_title=NA, point_num=NA, add_line = TRUE, output="total", ...){standardGeneric("richnessPlot")}, signature="object")


setMethod(f="richnessPlot", signature=c(object="list"),
  function(object,years,points,visits, times, plot_title, point_num, output, ...) {
    switch(output,
      total={
        visits<-if(anyNA(visits)) getDesign(object,info="visits") %>% unlist %>% max %>% seq else visits
        
        years<-if(anyNA(years)) getVisits(object, points=points, visits=visits, times=times) %>% 
          pull(Year) %>% unique %>% sort %>% full_seq(1) else years
        
        graphdata=birdRichness(object, years=years, points=points, visits = visits, times=times, byYear=T, output="total", ...)
        
        if (all(is.na(point_num))) point_num<-map(visits, function(visits){ 
          map(years, function(years) getVisits(object=object, years=years, visits=visits, times=times) %>% nrow) %>% 
            unlist(F)})
        
        return(richnessPlot(object=graphdata, plot_title=plot_title, point_num = point_num,visits = visits))
      },
      list={
        return(lapply(X=object, FUN=richnessPlot, years=years, points=points, plot_title=plot_title, point_num=point_num))
      }
    )
})


setMethod(f="richnessPlot", signature=c(object="NCRNbirds"),
  function(object, years, points, visits, times, plot_title, point_num, add_line, ...){
    
    visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
    years<-if(anyNA(years)) getVisits(object, points=points, visits=visits, times=times) %>% 
      pull(Year) %>% unique %>% sort %>% full_seq(1) else years
      
    graphdata<-birdRichness(object=object, years=years, points=points, visits=visits, times=times, byYear=T, ...)
    if(is.na(plot_title)) plot_title<-paste0("Number of Bird Species Observed in ", getParkNames(object,name.class = "long"))
    
    
    if (all(is.na(point_num))) point_num<-map(visits, function(visits){ 
      map(years, function(years) getVisits(object=object, years=years, visits=visits, times=times) %>% nrow) %>% unlist(F)})
    
    return(richnessPlot(object=graphdata, plot_title=plot_title, point_num=point_num, add_line=add_line))

})

setMethod(f="richnessPlot", signature=c(object="data.frame"),
  function(object, plot_title, point_num,add_line){
   
    SampEffort<-if(!all(is.na(point_num))) pmap(point_num, paste, sep=",") %>% unlist else NA
    
    integer_breaks<-min(object$Year):max(object$Year)
    YearTicks<- if(!all(is.na(point_num))) paste0(integer_breaks, "\n(", SampEffort,")") else integer_breaks
    
    GraphOut<-ggplot(data=object, aes(x=Year, y=Richness))+expand_limits(y=0)+
      geom_point(size=4, color="blue")+
      {if(add_line == TRUE) geom_line()}+
      scale_x_continuous(breaks=integer_breaks, minor_breaks=integer_breaks, labels=YearTicks)+
      labs(y=" Number of Species Observed" )+
      {if(!is.na(plot_title)) ggtitle(plot_title)}+
      theme_classic()+  
      theme(axis.title.y =element_text(size = 14, face ="bold", vjust= 1))+
      theme(axis.title.x =element_text(size = 14, face ="bold", vjust= 1))+
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12))+
      theme(axis.text.x = element_text(color="black", size = 10))
    
    return(GraphOut)
})
