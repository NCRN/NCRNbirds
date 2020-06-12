#' @include NCRNbirds_Class_def.R 
#'   
#' @title barplotRichness
#'
#' @importFrom dplyr case_when mutate
#' @importFrom ggplot2 aes coord_flip element_blank element_rect element_text geom_bar ggplot labs scale_fill_manual scale_x_discrete theme
#' @importFrom magrittr %>% 
#' @importFrom rlang !! sym
#' 
#' @description Makes a barplot of species richness
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{\link{birdRichness}}.
#' @param byPark Defaults to \code{FALSE}. If true the barplot will have on bar for each park. 
#' @param byYear Defaults to \code{FALSE}. If true the barplot will have on bar for each year. 
#' @param byPoint Defaults to \code{FALSE}. If \code{TRUE} the barplot will have one bar for each point.
#' @param byGuild Defaults to \code{FALSE}. If \code{TRUE} the barplot will be stacked by Guild.
#' @param includeNA Defaults to \code{TRUE}. If \code{byGuild = T} and \code{includeNA=F} then speices which have a response guild of \code{NA}
#' will not be included in the bar chart. 
#' @param color A color for the bars. 
#' @param plot_title  Optional,  A title for the plot. 
#' @param scale If \code{TRUE} will include a scale to the right of the barplot. 
#' @param labels If\code{TRUE} will label species richness values on the barplot.Labels are never shown wehen plotting by guild.
#' @param output Either "total" (the default) or "list". Only used when \code{object} is a \code{list}
#' @param plot Logical. Return plot \code{TRUE} (default) or data.frame \code{FALSE}. 
#' @param ... Additional arguments passed to \code{\link{birdRichness}}


#' @details This function produces a barplot of species richness by Park, Year or Point. It does this by using the 
#' output of the \code{\link{birdRichness}} function. The data is then passed on to ggplot2 for graphing. The user can chose to have \code{byPark=T}, 
#' \code{byYear=T} or \code{byPoint=T}, but should only pick one of the three. These can be combined with a guild category using \code{byGuild}. The 
#' \code{includeNA} argument indicates if bird species that are not categorized for a particular guild shoud be included.
#'   
#' @export


setGeneric(name="barplotRichness",function(object, byPark=F, byYear=F, byPoint=F, byGuild=F, includeNA=T, color="dark green", plot_title=NA, 
                scale=T, labels=T,output="total", plot=TRUE, ...){standardGeneric("barplotRichness")}, signature="object")

 setMethod(f="barplotRichness", signature=c(object="list"),
    function(object, byPark, byYear, byPoint, byGuild, includeNA, color, plot_title, scale, labels, output, plot,  ...) {
      switch(output,
        total={
           barplotdata=birdRichness(object,byPark=byPark, byYear=byYear, byPoint=byPoint, byGuild = byGuild, output="total",...)
 
         return(barplotRichness(object=barplotdata, byPark=byPark, byYear=byYear, byPoint=byPoint,byGuild = byGuild, includeNA = includeNA,
                                color=color, plot_title=plot_title, scale=scale, labels=labels, plot = plot))
       },
        list={
          return(map(object,barplotRichness,byPark=byPark, byYear=byYear,  byPoint=byPoint, byGuild = byGuild, includeNA = includeNA, color=color, 
                     plot_title=plot_title, scale=scale, labels=labels, plot=plot, ...))
        }
      )
  })
  
  setMethod(f="barplotRichness", signature=c(object="NCRNbirds"),
    function(object, byPark, byYear, byPoint, byGuild, includeNA, color, plot_title, scale, labels, plot, ...){
   barplotdata<-birdRichness(object, byPark=byPark, byYear=byYear, byPoint=byPoint, byGuild=byGuild, ...)
   
   barplotRichness(object=barplotdata, byPark=byPark, byYear=byYear, byPoint=byPoint, byGuild = byGuild, includeNA = includeNA, color=color,
                   plot_title = plot_title, scale=scale, labels=labels, plot=plot)

  })

setMethod(f="barplotRichness", signature=c(object="data.frame"),
  function(object, byPark, byYear, byPoint, byGuild, includeNA, color, plot_title, scale, labels, plot){
    
   guildLevels<-if(byGuild) unique(object$Guild) else NA
    
     object<-object %>% 
       {if(byPark) mutate(., ParkName=factor(ParkName))  else .} %>% 
       {if(byYear) mutate(., Year=factor(Year))  else .} %>% 
       {if(byPoint) mutate(., Point_Name=factor(Point_Name)) else .} %>% 
       {if(!includeNA & byGuild) filter(.,!is.na( Guild ) ) else .} %>% 
       {if(includeNA & byGuild) mutate(.,Guild=ifelse(is.na(Guild), "No Guild", Guild)) else .} %>% 
       {if(byGuild) mutate(., Guild=factor(Guild, levels=c("No Guild", guildLevels[!is.na(guildLevels)]))) }
    
     
     if(!plot) return(object)
     
     xcol<-sym(case_when(byPark~"ParkName", byYear~"Year", byPoint~"Point_Name"))

     # color1<-ifelse(is.na(color1),"seagreen4",color1)
     # color2<-ifelse(is.na(color2),"orange",color2)
     # 
     # #create colors
     # mycolors=c(color1,color2)


ggplot(data=object,aes(x=!!xcol, y=Richness, fill= if(byGuild) Guild else color ))+
  {if(byGuild) geom_bar(position="stack", stat="identity") } +
  {if(!byGuild) geom_bar(stat="identity", fill=color)} + 
  theme(panel.background = element_rect(fill="transparent"),
        panel.border= element_rect(color="black", fill=NA),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))+
  theme(legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),legend.position=c(0.8,0.8))+
  scale_x_discrete(limits=rev(levels(object %>% select(!!xcol))))+
  scale_fill_manual(values=color)+
  labs(x=element_blank(),y="Species Richness")+
  {if(labels & !byGuild) geom_text(aes(x=!!xcol, y=Richness, label=Richness),# group=Type)
            hjust = 1.3, color="white", size = 3.5, fontface="bold",inherit.aes = TRUE)}+
  {if(!is.na(plot_title)) ggtitle(plot_title)}+
  {if(byYear) coord_flip()}+
  {if(byGuild) labs(fill=element_blank())}
    
})
