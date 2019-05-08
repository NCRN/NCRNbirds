#' @include NCRNbirds_Class_def.R
#' @include BCI.R
#'   
#' @title BCIPlot
#'
#' @importFrom dplyr case_when group_by mutate n_distinct pull right_join summarize
#' @importFrom ggplot2 aes annotate element_line geom_pointrange ggplot ggtitle guides guide_legend labs mean_cl_boot
#' @importFrom ggplot2 scale_color_manual scale_x_continuous scale_y_continuous theme theme_classic
#' @importFrom magrittr %>% 
#' @importFrom purrr map map_dbl pmap
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
#' then this will be calculated automatically. If \code{object} is a \code{data.frame} or a \code{list} than this can be provided by the user. 
#' @param type A mandatory length 1 character \code{vector} that indicates the type of BCI to calculate. Can be "Cent_Appal" or "NETN_Forest_BCI"
#' @param output Either "total" (the default) or "list". Only used when \code{object} is a \code{list}. 
#' @param ... Additional arguments passed to \code{\link{birdRichness}}
#' 
#' @details This function produces a graph the Bird community Index  over time. It does this by using the output of 
#' the \code{\link{BCI}} function, and averging the plot values for the park's yearly value. The error bars on the plot represnt 95% 
#' bootstrap confidence intervals. The data is then passed on 
#' to ggplot2 for graphing. Typically this is done automatically by providing an \code{NCRNbirds} object or a \code{list}
#' of such objects. If the user wishes to provide their own \code{data.frame} it should have 3 columns, \code{Year, BCI, BCI_Category}
#' and each row should be data from single year.
#'   
#' @export


setGeneric(name="BCIPlot",function(object,years=NA, points=NA,visits=NA, times=NA, plot_title=NA, point_num=NA, type=NA,output="total", ...){standardGeneric("BCIPlot")}, signature="object")


setMethod(f="BCIPlot", signature=c(object="list"),
  function(object,years,points,visits, times, plot_title, point_num, type, output, ...) {
    
     switch(output,
       total={
         
        visits<-if(anyNA(visits)) getDesign(object,info="visits") %>% unlist %>% max %>% seq else visits
        years<-if(anyNA(years)) getVisits(object, points=points, visits=visits, times=times) %>% 
           pull(Year) %>% unique %>% sort %>% full_seq(1) else years
  
        graphdata<-data.frame(Year=years,BCI=NA, BCI_Category=NA)
    
        BCIlist<-years %>% map(~BCI(object=object, years=.x, points=points, type=type, visits=visits, times=times, output="dataframe",...)) 
      
        graphdata<-graphdata %>% mutate(BCI=BCIlist %>% map("BCI") %>% map_dbl(mean) %>% round(1), 
                                        Low=BCIlist %>% map("BCI") %>% map(mean_cl_boot) %>% map_dbl("ymin"),
                                        High=BCIlist %>% map("BCI") %>% map(mean_cl_boot) %>% map_dbl("ymax"),
                                        BCI_Category=case_when( BCI <40.1 ~"Low Integrity",
                                                                BCI>=40.1 & BCI<52.1 ~ "Medium Integrity",
                                                                BCI>=52.1 & BCI < 60.1 ~ "High Integrity",
                                                                BCI>=60.1 ~ "Highest Integrity" ), 
                                        BCI_Category=factor(BCI_Category, levels=c("Low Integrity","Medium Integrity",
                                                                                   "High Integrity","Highest Integrity")))
        
        if (all(is.na(point_num))) point_num<-map(visits, function(visits){ 
          map(years, function(years) getVisits(object=object, years=years, visits=visits, times=times) %>% nrow) %>% 
            unlist(F)})
        
        return(BCIPlot(object=graphdata, plot_title=plot_title, point_num = point_num))
       },
    
          list={
         return(lapply(X=object, FUN=BCIPlot, years=years, points=points, visits=visits, times=times,
                       plot_title=plot_title, point_num=point_num, type=type))
       }
     )
})


setMethod(f="BCIPlot", signature=c(object="NCRNbirds"),
  function(object,years,points,visits, times, plot_title=NA,type, ...){

    visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
    years<-if(anyNA(years)) getVisits(object, points=points, visits=visits, times=times) %>% 
      pull(Year) %>% unique %>% sort %>% full_seq(1) else years
    
    
    graphdata<-data.frame(Year=years,BCI=NA, BCI_Category=NA)
    BCIlist<-years %>% map(~BCI(object=object, years=.x, points=points, 
                                    visits=visits, times=times,type=type,...)) 
    
    graphdata<-graphdata %>% mutate(BCI=BCIlist %>% map("BCI") %>% map_dbl(mean) %>% round(1), 
                                    Low=BCIlist %>% map("BCI") %>% map(mean_cl_boot) %>% map_dbl("ymin"),
                                    High=BCIlist %>% map("BCI") %>% map(mean_cl_boot) %>% map_dbl("ymax"),
                                    BCI_Category=case_when( BCI <40.1 ~"Low Integrity",
                                                          BCI>=40.1 & BCI<52.1 ~ "Medium Integrity",
                                                          BCI>=52.1 & BCI < 60.1 ~ "High Integrity",
                                                          BCI>=60.1 ~ "Highest Integrity" ), 
                                     BCI_Category=factor(BCI_Category, levels=c("Low Integrity","Medium Integrity",
                                                                                "High Integrity","Highest Integrity")))
    
    if (all(is.na(point_num))) point_num<-map(visits, function(visits){ 
      map(years, function(years) getVisits(object=object, years=years, visits=visits, times=times) %>% nrow) %>% 
        unlist(F)})
    
    plot_title<-if(is.na(plot_title)) paste0("Bird Community Index for ",getParkNames(object, name.class="long")) else plot_title
                                             
    return(BCIPlot(object=graphdata, plot_title=plot_title, point_num = point_num))
})

setMethod(f="BCIPlot", signature=c(object="data.frame"),
  function(object, plot_title, point_num){
  
   BCIColors<-viridis_pal()(4)
   names(BCIColors)<-c("Low Integrity", "Medium Integrity", "High Integrity", "Highest Integrity")
   
    

    SampEffort<-if(!all(is.na(point_num))) pmap(point_num, paste, sep=",") %>% unlist else NA
    
    integer_breaks<-min(object$Year):max(object$Year)
    YearTicks<- if(!all(is.na(point_num))) paste0(integer_breaks, "\n(", SampEffort,")") else integer_breaks

    
    GraphOut<-ggplot(data=object, aes(x=Year, y=BCI, color=BCI_Category)) +
      annotate("rect", ymin=0,  ymax=40, xmin=-Inf, xmax=Inf, fill=BCIColors[1], alpha=0.3) +
      annotate("rect", ymin=40, ymax=52, xmin=-Inf, xmax=Inf, fill=BCIColors[2], alpha=0.3) +
      annotate("rect", ymin=52, ymax=60, xmin=-Inf, xmax=Inf, fill=BCIColors[3], alpha=0.3) +
      annotate("rect", ymin=60, ymax=80, xmin=-Inf, xmax=Inf, fill=BCIColors[4], alpha=0.3) +
      geom_pointrange(aes(ymin=Low, ymax=High),fatten=4, size=1) +
      scale_color_manual(values=BCIColors, drop=FALSE) +
      guides(color=guide_legend(reverse=T, title ="BCI Category"))+
      scale_x_continuous(breaks=integer_breaks, minor_breaks=integer_breaks, labels=YearTicks) +
      scale_y_continuous(limits=c(0,80), expand=c(0,0)) +
      labs(y=" Bird Community Index", caption="Values in parentheses indicate the number of points monitored each visit of each year.") +
      {if(!is.na(plot_title)) ggtitle(plot_title)} +
      theme_classic()# +
    
    return(GraphOut)
})
