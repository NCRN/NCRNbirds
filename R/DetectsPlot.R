#' @include NCRNbirds_Class_def.R
#' @include CountXVisit.R
#'   
#' @title detectsPlot
#'
#' @importFrom dplyr group_by n_distinct pull right_join summarize
#' @importFrom ggplot2 aes element_line geom_point ggplot ggtitle labs scale_x_continuous theme theme_minimal
#' @importFrom magrittr %>% 
#' @importFrom tidyr replace_na
#' 
#' @description Plots bird detections (relative abundance) over time.
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{CountXVisit()}.
#' @param years  A numeric vector. Indicates which years should be graphed.
#' @param points A character vector of point names. Only these points will be used.
#' @param plot_title  Optional,  A title for the plot. 
#' @param point_num An optional numeric vector indicating the number of points sampled each year. If \code{object} is a \code{NCRNbirds} object
#' or a \code{list} of such objects, then this will be calculated automatically. If \code{object} is a \code{data.frame} than this can be
#' provided by the user. 
#' @param output Either "total" (the default) or "list". Only used when \code{object} is a \code{list}. 
#' @param ... Additional arguments passed to \code{\link{CountXVisit}}
#' 
#' @details This function produces a graph of species detections over time. It does this by using the output of the \code{\link{CountXVisit}}
#' function. The data is then passed on to ggplot2 for graphing.
#'   
#' @export


setGeneric(name="detectsPlot",function(object,years=NA, points=NA, plot_title=NA, point_num=NA,output="total", ...){standardGeneric("detectsPlot")}, signature="object")


setMethod(f="detectsPlot", signature=c(object="list"),
  function(object,years,points,plot_title, point_num, output, ...) {
    switch(output,
      total={ graphdata<-CountXVisit(object=object, years=years, points=points, ...) %>% 
        # if(all(is.na(point_num))) point_num<-getVisits(object, years=years, points=points) %>% 
            group_by(Year) %>%  
            # right_join(.,data.frame(Year=min(.$Year):max(.$Year)), by="Year") %>% pull(Mean) %>% replace_na(0) %>% 
            summarize("Visit 1"=round(mean(Visit1, na.rm=T),digits=2), "Visit 2"= round( mean(Visit2, na.rm=T),digits=2)) %>% 
            gather(key=Visit, value=Mean, `Visit 1`, `Visit 2`)  %>% 
            mutate(Visit= factor(Visit))
        return(detectsPlot(object=graphdata, plot_title=plot_title, point_num = point_num))
      },
      list={
        return(lapply(X=object, FUN=detectsPlot, years=years, points=points, plot_title=plot_title, point_num=point_num))
      }
    )
})


setMethod(f="detectsPlot", signature=c(object="NCRNbirds"),
  function(object,years,points,plot_title,point_num, ...){
      
    graphdata<-CountXVisit(object=object, years=years, points=points, ...) %>% 
      # if(all(is.na(point_num))) point_num<-getVisits(object, years=years, points=points) %>% 
        group_by(Year) %>%  
        # right_join(.,data.frame(Year=min(.$Year):max(.$Year)), by="Year") %>% pull(Mean) %>% replace_na(0) %>% 
        summarize("Visit 1"=round(mean(Visit1, na.rm=T),digits=2), "Visit 2"= round( mean(Visit2, na.rm=T),digits=2)) %>% 
        gather(key=Visit, value=Mean, `Visit 1`, `Visit 2`)  %>% 
        mutate(Visit= factor(Visit))
    
    return(detectsPlot(object=graphdata, plot_title=plot_title, point_num=point_num))

})

setMethod(f="detectsPlot", signature=c(object="data.frame"),
  function(object, plot_title, point_num){
   
    
    integer_breaks<-min(object$Year):max(object$Year)
    YearTicks<- if(!all(is.na(point_num))) paste0(integer_breaks, "\n(", point_num,")") else integer_breaks
    
    GraphOut<-ggplot(data=object, aes(x=Year, y=Mean, colour= Visit))+
      geom_point(size=4)+
      scale_x_continuous(breaks=integer_breaks, minor_breaks=integer_breaks, labels=YearTicks)+
      labs(y=" Mean Number of Birds Detected per Point", caption="Values in parentheses indicate the number of points monitored each year.")+
      {if(!is.na(plot_title)) ggtitle(plot_title)}+
      theme_minimal()+
      theme(axis.line=element_line(color="black"))
    
    return(GraphOut)
})
