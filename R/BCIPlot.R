#' @include NCRNbirds_Class_def.R
#' @include BCI.R
#'   
#' @title BCIPlot
#'
#' @importFrom dplyr case_when group_by n_distinct pull right_join summarize
#' @importFrom ggplot2 aes element_line geom_point ggplot ggtitle labs scale_x_continuous scale_y_continuous scale_color_manual theme theme_minimal
#' @importFrom magrittr %>% 
#' @importFrom purrr map pmap
#' @importFrom tidyr full_seq
#' @importFrom viridis viridis_pal

#' @importFrom tidyr replace_na
#' 
#' @description Plots BCI score over time.
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{birdRichness()}.
#' @param years  A numeric vector. Indicates which years should be graphed.
#' @param points A character vector of point names. Only these points will be used.
#' @param visits A length 1 numeric vector, defaults to NA. Returns data only from the indicated visits. 
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been vistied is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
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


setGeneric(name="BCIPlot",function(object,years=NA, points=NA,visits=NA, times=NA, plot_title=NA, point_num=NA,output="total", ...){standardGeneric("BCIPlot")}, signature="object")


setMethod(f="BCIPlot", signature=c(object="list"),
  function(object,years,points,visits, times, plot_title, point_num, output, ...) {
    # switch(output,
    #   total={graphdata=birdRichness(object, years=years, points=points, byYear=T, output="total", ...)
    #   if(all(is.na(point_num))) point_num<-getVisits(object, years=years,points=points) %>% 
    #       group_by(Year) %>% summarise(Total=n_distinct(Point_Name)) %>% 
    #       right_join(.,data.frame(Year=min(.$Year):max(.$Year)), by="Year") %>% pull(Total) %>% replace_na(0)
    #     return(richnessPlot(object=graphdata, plot_title=plot_title, point_num = point_num))
    #   },
    #   list={
    #     return(lapply(X=object, FUN=richnessPlot, years=years, points=points, plot_title=plot_title, point_num=point_num))
    #   }
    # )
})


setMethod(f="BCIPlot", signature=c(object="NCRNbirds"),
  function(object,years,points,visits, times, plot_title=NA, ...){

    visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
    years<-if(anyNA(years)) getVisits(object, points=points, visits=visits, times=times) %>% 
      pull(Year) %>% unique %>% sort %>% full_seq(1) else years
    
    
    graphdata<-data.frame(Year=years,BCI=NA, BCI_Category=NA)
    
    graphdata$BCI<-(years %>% map(~BCI(object=object, years=.x, points=points, visits=visits, times=times,...),...)) %>%
      map("BCI") %>% map(mean) %>% unlist %>% round(0)
    
    graphdata<-graphdata %>% mutate (BCI_Category=case_when( BCI <40.1 ~"Low Integrity",
                                                          BCI>=40.1 & BCI<52.1 ~ "Medium Integrity",
                                                          BCI>=52.1 & BCI < 60.1 ~ "High Integrity",
                                                          BCI>=60.1 ~ "Highest Integrity" ))
    
    if (all(is.na(point_num))) point_num<-map(visits, function(visits){ 
      map(years, function(years) getVisits(object=object, years=years, visits=visits, times=times) %>% nrow) %>% 
        unlist(F)})
    
    return(BCIPlot(object=graphdata, plot_title=plot_title, point_num = point_num))
})

setMethod(f="BCIPlot", signature=c(object="data.frame"),
  function(object, plot_title, point_num){
   ## comes in as year bci
    BCIColors<-viridis_pal()(4)
    names(BCIColors)<-c("Low Integrety", "Medium Integrity", "High Integrity", "Highest Integrity")
    BCIscale<-scale_color_manual(name="BCI", values=BCIColors)
    
    
    SampEffort<-if(!all(is.na(point_num))) pmap(point_num, paste, sep=",") %>% unlist else NA
    
    integer_breaks<-min(object$Year):max(object$Year)
    YearTicks<- if(!all(is.na(point_num))) paste0(integer_breaks, "\n(", SampEffort,")") else integer_breaks

    
    GraphOut<-ggplot(data=object, aes(x=Year, y=BCI, color=BCI_Category)) +
      geom_point(size=4) +
      BCIscale +
      scale_x_continuous(breaks=integer_breaks, minor_breaks=integer_breaks, labels=YearTicks) +
      scale_y_continuous(limits=c(0,70))+
      labs(y=" Bird Community Index", caption="Values in parentheses indicate the number of points monitored each year.") +
      {if(!is.na(plot_title)) ggtitle(plot_title)} +
      theme_minimal() +
      theme(axis.line=element_line(color="black"))
    
    return(GraphOut)
})
