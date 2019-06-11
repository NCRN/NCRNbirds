#' @include NCRNbirds_Class_def.R
#' @include CountXVisit.R
#' @include getParkNames.R
#' @include getVisits.R
#'   
#' @title detectsPlot
#'
#' @importFrom dplyr group_by pull summarise
#' @importFrom ggplot2 aes element_text geom_point ggplot ggtitle labs scale_x_continuous theme_classic theme_minimal scale_color_brewer geom_errorbar expand_limits
#' @importFrom magrittr %>% 
#' @importFrom purrr map pmap
#' @importFrom tidyr full_seq gather 
#' 
#' @description Plots bird detections (relative abundance) over time.
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{CountXVisit()}.
#' @param parks A character vector of park codes. Only visits within these parks will be returned.
#' @param years  A numeric vector. Indicates which years should be graphed.
#' @param points A character vector of point names. Only these points will be used.
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been vistied is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param visits A length 1 numeric vector, defaults to NA. Returns data only from the indicated visits.
#' @param max Logical. If \code{FALSE}, the default, then data from each visit will be dispalyed separately. 
#' If \code{TRUE} then the maximum detected across all visits will be displayed.
#' @param plot_title  Optional,  A title for the plot. 
#' @param point_num An optional list of numeric vector indicating the number of points sampled each visit of each year. Each visit is a separte elemnet in the list. If \code{object} is a \code{NCRNbirds} object
#' or a \code{list} of such objects, then this will be calculated automatically. If \code{object} is a \code{data.frame} than this can be
#' provided by the user. 
#' @param se Add Standard error to the plot. \code{TRUE} or \code{FALSE}, defaults to  \code{FALSE}
#' @param output Either "total" (the default) or "list". Only used when \code{object} is a \code{list}. 
#' @param ... Additional arguments passed to \code{\link{CountXVisit}}
#' 
#' @details This function produces a graph of species detections over time. It does this by using the output of the \code{\link{CountXVisit}}
#' function. The data is then passed on to ggplot2 for graphing.
#'   
#' @export


setGeneric(name="detectsPlot",function(object,parks= NA,years=NA,  points=NA, visits=NA, times=NA, max=F, plot_title=NA, point_num= NA, se= FALSE, output="total", ...){standardGeneric("detectsPlot")}, signature="object")


setMethod(f="detectsPlot", signature=c(object="list"),
  function(object,parks,years,points,visits,times, max, plot_title, point_num,se, output, ...) {
    
    switch(output,
       total={    visits<-if(anyNA(visits)) getDesign(object,info="visits") %>% unlist %>% max %>% seq else visits
       
       years<-if(anyNA(years)) getVisits(object, parks=parks, points=points, visits=visits, times=times) %>% 
         pull(Year) %>% unique %>% sort %>% full_seq(1) else years
       
         graphdata<-CountXVisit(object=object, parks= parks, years=years, points=points,visits=visits,times=times, max=max,...)
         graphdata<-if (max){graphdata %>% dplyr::select (Admin_Unit_Code, Point_Name, Year, count=Max) %>% mutate(visit="Maximum") %>% 
            group_by(Year, visit) } else{
                    graphdata %>% gather(visit, count, -Admin_Unit_Code, -Point_Name, -Year) %>%  # rearrange to sum by visit to handle varying visits per point
                    group_by(Year, visit)
           }
          graphdata<-graphdata %>% dplyr::summarise(Mean= round(mean(count, na.rm=T),digits=3), se= round(sd(count, na.rm=T)/sqrt(n()),digits=3))
                   
         if (all(is.na(point_num))) point_num<-purrr::map(visits, function(visits){ 
           purrr::map(years, function(years) getVisits(object=object,parks=parks,  years=years, visits=visits, times=times) %>% nrow) %>% unlist(F)})
         
                   
                 return(detectsPlot(object=graphdata, plot_title=plot_title,point_num=point_num, se=se))
                },
      list={
         return(lapply(X=object, FUN=detectsPlot, years=years, points=points, visits=visits, times=times, max=max,
                       plot_title=plot_title,point_num=point_num, se=se,...))
      }
    )
})


setMethod(f="detectsPlot", signature=c(object="NCRNbirds"),
  function(object,parks,years,points,visits,times, plot_title,point_num, ...){
      
    
    visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
    years<-if(anyNA(years)) getVisits(object, parks=parks, points=points,  visits=visits, times=times) %>% 
      pull(Year) %>% unique %>% sort %>% full_seq(1) else years
    
    
    
    graphdata<-CountXVisit(object=object, parks= parks, years=years, points=points,visits=visits,times=times, max=max,...)
    graphdata<-if (max){graphdata %>% dplyr::select (Admin_Unit_Code, Point_Name, Year, count=Max) %>% mutate(visit="Maximum") %>% 
        group_by(Year, visit) } else{
          graphdata %>% gather(visit, count, -Admin_Unit_Code, -Point_Name, -Year) %>%  # rearrange to sum by visit to handle varying visits per point
            group_by(Year, visit)
        }
    graphdata<-graphdata %>% dplyr::summarise(Mean= round(mean(count, na.rm=T),digits=3), se= round(sd(count, na.rm=T)/sqrt(n()),digits=3))
    
    if(is.na(plot_title)) plot_title<-paste0("Mean number of Birds Detected in ", getParkNames(object,name.class = "long")) 
    
    if (all(is.na(point_num))) point_num<-purrr::map(visits, function(visits){ 
      purrr::map(years, function(years) getVisits(object=object, parks=parks,  years=years, visits=visits, times=times) %>% nrow) %>% unlist(F)})
    
    return(detectsPlot(object=graphdata, plot_title=plot_title,point_num=point_num, se=se))
})

setMethod(f="detectsPlot", signature=c(object="data.frame"),
          function(object, plot_title, point_num, se){
            
            SampEffort<-if(!all(is.na(point_num))) pmap(point_num, paste, sep=",") %>% unlist else NA
            
            integer_breaks<-min(object$Year):max(object$Year)
            YearTicks<- if(!all(is.na(point_num))) paste0(integer_breaks, "\n(", SampEffort,")") else integer_breaks
            
            GraphOut<-ggplot(data=object, aes(x=Year, y=Mean, colour = visit))+ expand_limits(y=0)+
              geom_point(size=4)+scale_color_brewer(palette="Dark2")+
              scale_x_continuous(breaks=integer_breaks, minor_breaks=integer_breaks, labels=YearTicks)+            
              {if(!is.na(plot_title)) ggtitle(plot_title)}+
              theme_classic()+  
              theme(axis.title.y =element_text(size = 14, face ="bold", vjust= 1))+
              theme(axis.title.x =element_text(size = 14, face ="bold", vjust= 1))+
              theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12))+
              theme(axis.text.x = element_text(color="black", size = 10))
              
            if(!se) {GraphOut<- (GraphOut+
              labs(y=" Mean number of birds detected per point", colour= ""))
            }else{
              GraphOut<- (GraphOut+ geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.6)+
                labs(y=" Mean + SE number of birds detected per point", colour= ""))
                }
            
            return(GraphOut)
          })

