#' @include NCRNbirds_Class_def.R 
#'   
#' @title trendsPlot
#' 
#' @importFrom ggplot2 aes expand_limits ggplot geom_errorbar geom_line geom_point geom_ribbon guide_legend guides labs scale_color_manual 
#' @importFrom ggplot2 scale_shape_identity scale_x_continuous theme theme_classic
#' @importFrom purrr map
#' @importFrom rlang !! sym 
# 
#' @description Makes a plot of trends in bird populations
#' 
#' @param object An unmarked fit object with an yearly trend such as those produced by \code{\link{unmarkedBirds}}, a \code{data.frame} produced by 
#' \code{\link{summarizeTrend}}, or a \code{list} of such objects.
#' @param trendtype Either "occu" or "pcount". Indicates if this is from an occupancy model or an n-mixture model. Only required if \code{object} is a data.frame. 
#' @param estimate Either "psi", the default, or "z". Inidicates the type of esimate you wish to display.
#' @param est_symbol Either "point" or "line". Indicates how you wish to display the estimate.
#' @param est_color The color of the estimate.
#' @param est_shape If \code{est_symbol} is "point" this will control the shape of the points. Default is a filled circle.
#' @param obs_color The color of the observed data.
#' @param obs_shape This controls the shape of the observed points. Default is a filled triangle.
#' @param error_symbol This controls how the 95\% CIs are displayed. Either "bar" for error bars or "ribbon" for a lightly shaded region.
#' @param plot_title  Optional,  A title for the plot. 
#' @param legend If \code{TRUE} will include a legend in the plot. 
#' @param plot Logical. Return plot \code{TRUE} (default) or data.frame \code{FALSE}. 


#' @details This function produces plot of trends based on the output of \code{\link{summarizeTrend}}. If the \code{object} is the output of 
#' \code{\link{unmarkedBirds}} then the function will automatically detect if the trend is based on an occupancy or n-mixture model. If the \code{object}
#' is a \code{data.frame} this has to be specified using the \code{trendtype} agrument. 
#' 
#' The plot will display both the naive values and the model estimates. The type of estimate is controlled by the \code{esitmate} argument. If this 
#' is "psi", then the model estimates will be the model coefficients back transformed to occupancy or abundance. This is the estimated 
#' value for  a site given its site. The  other option,"z", takes into account not only site covariates but also the  visit covaraites and 
#' actual detection history for each site.  The estiamte will also display  upper and lower limits for a 95\% CI. 
#'  
#' Several arguments control the graphical display. The naive values are always points, but their color and shape can be selected by the user. Estiamated 
#' values can also be displayed and customized as points, or can be a line instead. Error in the estimates can either be shown as yearly error bars or as
#' a "ribbon" -  a shaded area around the estimate. The color and shape arguments accept standard \code{ggplot2} options.
#'  
#'   
#' @export


setGeneric(name="trendsPlot",function(object, trendtype, estimate="psi", est_symbol="line",est_color="dark green", est_shape=19, obs_color="black",
    obs_shape=17, error_symbol="ribbon",plot_title=NA, legend=T,  plot=TRUE
    ){standardGeneric("trendsPlot")}, signature="object")

setMethod(f="trendsPlot", signature=c(object="list"),
  function(object, trendtype, estimate, est_symbol, est_color, est_shape, obs_color, obs_shape, error_symbol, plot_title, legend, plot ) {

    
return(map(.x=object, .f=trendsPlot, trendtype = trendtype, estimate = estimate, est_symbol = est_symbol, est_color = est_color,est_shape = est_shape,
           obs_color = obs_color, obs_shape = obs_shape, error_symbol = error_symbol, plot_title = plot_title,legend = legend, plot = plot))
})



setMethod(f="trendsPlot", signature=c(object="unmarkedFitPCount"),
          function(object, trendtype, estimate, est_symbol, est_color, est_shape, obs_color, obs_shape, error_symbol, plot_title, legend, plot ){
            
            TrendDF<-summarizeTrend(object)
            trendsPlot(object=TrendDF, trendtype = "pcount", estimate = estimate, est_symbol = est_symbol, est_color = est_color, est_shape = est_shape,
                       obs_color = obs_color, obs_shape = obs_shape, error_symbol = error_symbol, plot_title = plot_title, legend = legend, plot = plot)
})

  
setMethod(f="trendsPlot", signature=c(object="unmarkedFitOccu"),
  function(object, trendtype, estimate, est_symbol, est_color, est_shape, obs_color, obs_shape, error_symbol, plot_title, legend, plot ){
 
    TrendDF<-summarizeTrend(object)
    trendsPlot(object=TrendDF, trendtype = "occu", estimate = estimate, est_symbol = est_symbol, est_color = est_color,est_shape = est_shape,
               obs_color = obs_color, obs_shape = obs_shape, error_symbol = error_symbol, plot_title = plot_title,legend = legend, plot = plot)
})

setMethod(f="trendsPlot", signature=c(object="data.frame"),
  function(object, trendtype, estimate, est_symbol, est_color, est_shape, obs_color, obs_shape, error_symbol, plot_title, legend, plot ){
    
    if(!plot) return(object)
    
    ycol<-sym(switch(trendtype, occu="Naive_Occu", pcount="Naive_Mean_Count"))
    
    Y_label<-switch(trendtype, occu="Occupancy",pcount="Birds per Point" )

    integer_breaks<-min(object$Year):max(object$Year)
    
    Est<-sym(estimate)
    Est_min<-sym(paste0(estimate,"_lower95"))
    Est_max<-sym(paste0(estimate,"_upper95"))

    PlotColors=c("Estimate"=est_color, "Observed"=obs_color)
    
ggplot(data=object,aes(x=Year, y=!!ycol)) +
  geom_point(aes(color="Observed"),size=4, shape=obs_shape) +
  {if(est_symbol=="point") geom_point(aes(y=!!(Est), color="Estimate"), size=4, shape=est_shape ) } +
  {if(est_symbol=="line") geom_line(aes(y=!!(Est), color="Estimate"), size=1 ) } + 
  {if(error_symbol=="bar") geom_errorbar(aes(ymin=!!(Est_min), ymax=!!(Est_max), color="Estimate"), width=.6)} +
  {if(error_symbol=="ribbon") geom_ribbon(aes(ymin=!!(Est_min), ymax=!!(Est_max)), fill=est_color, alpha=0.15)} +
  {if(!is.na(plot_title)) ggtitle(plot_title)} +
  scale_x_continuous(breaks=integer_breaks, minor_breaks=integer_breaks) +
  expand_limits(y=0) +
  scale_color_manual(name=element_blank(), values=PlotColors) +
  scale_shape_identity() +
  guides(fill="none", colour = guide_legend(override.aes = list(shape = c(est_shape,obs_shape))))+
  theme_classic() +  
  theme(axis.title.y =element_text(size = 14, face ="bold", vjust= 1)) +
  theme(axis.title.x =element_text(size = 14, face ="bold", vjust= 1)) +
  theme(axis.text.x = element_text(color="black", size = 10)) +
  theme(legend.position="bottom") +
  labs(x=element_blank(), y=Y_label) +
  {if(!legend) theme(legend.position="none")}
})
