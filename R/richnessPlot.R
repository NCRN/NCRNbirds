#' @include NCRNbirds_Class_def.R
#' @include birdRichness.R
#'   
#' @title richnessPlot
#'
#' @importFrom dplyr pull
#' @importFrom ggplot2 aes element_line geom_point ggplot ggtitle labs scale_x_continuous theme theme_minimal
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
#' 
#' @details This function produces a graph of species richness over time. It does this by using the output of the \code{\link{birdRichness}}
#' function. The data is then passed on to ggplot2 for graphing.
#'   
#' @export


setGeneric(name="richnessPlot",function(object,years=NA, points=NA, visits = NA, times=NA, plot_title=NA, point_num=NA, output="total", ...){standardGeneric("richnessPlot")}, signature="object")


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
  function(object, years, points, visits, times, plot_title, point_num, ...){
    
    visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
    years<-if(anyNA(years)) getVisits(object, points=points, visits=visits, times=times) %>% 
      pull(Year) %>% unique %>% sort %>% full_seq(1) else years
      
    graphdata<-birdRichness(object=object, years=years, points=points, visits=visits, times=times, byYear=T, ...)
    if(is.na(plot_title)) plot_title<-paste0("Number of Bird Species Observed in ", getParkNames(object,name.class = "long"))
    
    
    if (all(is.na(point_num))) point_num<-map(visits, function(visits){ 
      map(years, function(years) getVisits(object=object, years=years, visits=visits, times=times) %>% nrow) %>% unlist(F)})
    
    return(richnessPlot(object=graphdata, plot_title=plot_title, point_num=point_num))

})

setMethod(f="richnessPlot", signature=c(object="data.frame"),
  function(object, plot_title, point_num){
   
    SampEffort<-if(!all(is.na(point_num))) pmap(point_num, paste, sep=",") %>% unlist else NA
    
    integer_breaks<-min(object$Year):max(object$Year)
    YearTicks<- if(!all(is.na(point_num))) paste0(integer_breaks, "\n(", SampEffort,")") else integer_breaks
    
    GraphOut<-ggplot(data=object, aes(x=Year, y=Richness))+
      geom_point(size=4, color="blue")+
      scale_x_continuous(breaks=integer_breaks, minor_breaks=integer_breaks, labels=YearTicks)+
      labs(y=" Number of Species Observed", caption="Values in parentheses indicate the number of points monitored per visit in each year.")+
      {if(!is.na(plot_title)) ggtitle(plot_title)}+
      theme_minimal()+
      theme(axis.line=element_line(color="black"))
    
    return(GraphOut)
})
