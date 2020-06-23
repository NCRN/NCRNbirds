#' @include NCRNbirds_Class_def.R 
#'   
#' @title trendsPlot
#'
#' @importFrom dplyr case_when mutate
#' 
#' @importFrom ggplot2 aes expand_limits ggplot geom_errorbar geom_line geom_point geom_ribbon guide_legend guides labs scale_color_manual 
#' @importFrom ggplot2 scale_shape_identity scale_x_continuous theme theme_classic
#' 
#' @importFrom magrittr %>% 
#' 
#' @importFrom rlang !! sym
#' 
#' @description Makes a plot of trends in bird populations
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{\link{birdRichness}}.
#' @param trendtype Indicates if this is from an occupancy model "occu" or a n-mixture model. Only required if \code{object} is a data.frame. 
#' @param estiamte Either "psi", the defualt or "z". Inidicate the type of esimate you wish to display.
#' @param est_symbol Either "point" or "line". Indicates how you wish to display the estimate.
#' @param est_color The color of the estimate.
#' @param est_shape If \code{est_symbol} is "point" this will control the shape of the points. Default is a filled circle.
#' @param obs_color The color of the observed data.
#' @param obs_shape This controls the shape of the observed points. Default is a filled triangle.
#' @param error_symbol This controls how the 95\% CIs are displayed. Either "bar" for error bars or "ribbon" for a lightly shaded region.
#' @param plot_title  Optional,  A title for the plot. 
#' @param legend If \code{TRUE} will include a legend in the plot. 
#' @param plot Logical. Return plot \code{TRUE} (default) or data.frame \code{FALSE}. 


#' @details This function produces a barplot of species richness by Park, Year or Point. It does this by using the 
#' output of the \code{\link{birdRichness}} function. The data is then passed on to ggplot2 for graphing. The user can chose to have \code{byPark=T}, 
#' \code{byYear=T} or \code{byPoint=T}, but should only pick one of the three. These can be combined with a guild category using \code{byGuild}. The 
#' \code{includeNA} argument indicates if bird species that are not categorized for a particular guild shoud be included.
#' 
#' Colors:
#' 
#' Colors of the bars are controlled by either \code{colors} or \code{palette} argumnts. If the barplot is not by guild then the color indicated by 
#' \code{colors} will be used. If \code{byGuild=T} then by default the \code{palette} arguement will be used. However, the \code{palette} arguement
#' can be overridden simply by giving \code{colors} a vector of colors with one color for each guild category shown in the barplot.If 
#' \code{includeNA=T} than than the first color will correspond to the NA value.
#'  
#'   
#' @export


setGeneric(name="trendsPlot",function(object, trendtype, estimate="psi", est_symbol="line",est_color="dark green", est_shape=19, obs_color="black",
    obs_shape=17, error_symbol="ribbon",plot_title=NA, legend=T,  plot=TRUE
    ){standardGeneric("trendsPlot")}, signature="object")

 # setMethod(f="trendsPlot", signature=c(object="list"),
 #    function(object, byPark, byYear, byPoint, byGuild, includeNA, colors, palette, plot_title, scale, labels, output, plot,  ...) {
 #      switch(output,
 #        total={
 #           barplotdata=birdRichness(object,byPark=byPark, byYear=byYear, byPoint=byPoint, byGuild = byGuild, output="total",...)
 # 
 #         return(barplotRichness(object=barplotdata, byPark=byPark, byYear=byYear, byPoint=byPoint,byGuild = byGuild, includeNA = includeNA,
 #                                colors=colors, palette=palette, plot_title=plot_title, scale=scale, labels=labels, plot = plot))
 #       },
 #        list={
 #          return(map(object,barplotRichness,byPark=byPark, byYear=byYear,  byPoint=byPoint, byGuild = byGuild, includeNA = includeNA, colors=colors, 
 #                     palette=palette, plot_title=plot_title, scale=scale, labels=labels, plot=plot, ...))
 #        }
 #      )
 #  })
  
  # setMethod(f="trendsPlot", signature=c(object="NCRNbirds"),
  #   function(object, byPark, byYear, byPoint, byGuild, includeNA, colors, palette, plot_title, scale, labels, plot, ...){
  #  barplotdata<-birdRichness(object, byPark=byPark, byYear=byYear, byPoint=byPoint, byGuild=byGuild, ...)
  #  
  #  barplotRichness(object=barplotdata, byPark=byPark, byYear=byYear, byPoint=byPoint, byGuild = byGuild, includeNA = includeNA, colors=colors,
  #                 palette=palette, plot_title = plot_title, scale=scale, labels=labels, plot=plot)
  # 
  # })

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
  #{if(error_symbol=="ribbbon") scale_fill_manual(guide=F)}
  guides(fill="none", colour = guide_legend(override.aes = list(shape = c(est_shape,obs_shape))))+
  theme_classic() +  
  theme(axis.title.y =element_text(size = 14, face ="bold", vjust= 1)) +
  theme(axis.title.x =element_text(size = 14, face ="bold", vjust= 1)) +
  theme(axis.text.x = element_text(color="black", size = 10)) +
  labs(x=element_blank(), y=Y_label) +
  {if(!legend) theme(legend.position="none")}
 

    
})
