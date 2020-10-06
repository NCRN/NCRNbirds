#' @include NCRNbirds_Class_def.R getBirds.R getGuilds.R getVisits.R matchParkCodes.R 
#' 
#' @title birdRichness
#' 
#' @importFrom dplyr distinct filter mutate n_distinct pull select summarise ungroup
#' @importFrom magrittr %>% 
#' 
#' @description Returns the number of bird species found in a park, at a point or a collection of points. 
#' 
#' @inheritParams getBirds
#' @param object An NCRNbirds object or a list of such objects or a \code{data.frame} like that created by \code{\link{getBirds}}.
#' @param points A character vector passed to \code{\link{getBirds}}. The names of one or more points where the data was collected.
#'  \code{NA}(the default) measures richness from all points.
#' @param AOU  A character vector passed to \code{\link{getBirds}}. One or more AOU (American Ornothological Union) codes of bird species. 
#' \code{NA} (the default) measures richness based on all species, otherwise only species listed in \code{AOU} will be counted.
#' @param years  A vector of numbers passed to \code{\link{getBirds}}. \code{NA} (the default) will count species regardless of the year
#'  they were monitored, otherwise only data from years listed in \code{years} will be used. See details below for 
#'  how years where no visits took place are handeled.  
#' @param byPark Logical. If \code{FALSe} (the default) the total species richness across all parks will be returned as a single numeric value,
#'  if \code{TRUE} a data.frame will be returned with each row a different park and its corresponding species richness.
#' @param byYear Logical. If \code{FALSE} (the default) the total species richness across all years will be returned a single numeric value,
#'  if \code{TRUE} a data.frame will be returned with each row a different year and its corresponding species richness.
#' @param byPoint Logical ,if \code{FALSE} (the default) the total species richness across all points will be returned a single numeric value,
#'  if \code{TRUE} a data.frame will be returned with each row a different plot and its corresponding species richness.
#' @param byGuild Logical,if \code{FALSE} (the default) the total species richness across all points will be returned a single numeric value,
#'  if \code{TRUE} a data.frame will be returned with each row a different response guild plot and its corresponding species richness. The 
#'  guild will be determined by the \code{guildType} and \code{guildCateogory} arguments.
#' @param guildType The type of guild as determined by the BCI. Passed on to the \code{type} argument of \code{\link{getGuilds}} 
#' @param guildCategory The guild category as determined by the BCI. Passed on to the \code{categories} argument of \code{\link{getGuilds}}. 
#'  Should be only one category.
#' @param output Either "total" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function, 
#' when \code{object} is a \code{list}. "total" will give the number of distinct species found across all parks. 
#' "list" will return a list, with each entry to the list corresponding to the species richness of one of the \code{NCRNbirds} objects in the input list.  
#' @param wide Defaults to \code{FALSE}. If \code{TRUE} and \code{byYear} is \code{TRUE} then there will be a column for each year and row for each park 
#' or point. Otherwise there will be a single \code{Year} column to indicate the year.
#' @param name.class Indicates if park names should appear as "code" the defaults or as "short" or "long" names from \code{\link{getParkNames}}
#' @param ... additional arguments passed to \code{\link{getBirds}}. Any argument which is a valid argument for \code{\link{getBirds}} can be used here.
#' 
#' @details This function calculates the species richness for a park, group of parks, group of monitoring points etc. 
#' Species richness is the number of different species found, regardless of their abundance. The function works by first getting the monitoring data by 
#' using the \code{\link{getBirds}} function and then counting the number of unique birds found. All of the options for combining or subsetting 
#' data in \code{\link{getBirds}} will work for \code{birdRichness} including subsetting by year and monitoring point. If there is no data, due to 
#' no sampling occuring at hte specified time and place, \code{NA} will be returned. 
#' 
#' How years with no visits are handled depends on wth is supplied in \code{object}. If the \code{object} is a \code{data.frame} and 
#'  \code{years} are specified, it is assumed that result is wanted for all of those years. Therefore if some years are not present in the data, 
#'  they will be assigned a richness of zero. If the \code{object} is a \code{NCRNbirds} object or a list of such objects, then 
#' \code{\link{getVisits}} will be called to determine which years samplign took place in. Only years with vistis will be returned, and 
#' years where no birds were observed will be given a richenss of zero. 
#' 
#' @export

setGeneric(name="birdRichness",function(object,points=NA,AOU=NA,years=NA,visits=NA, byPark=FALSE, byYear=FALSE, byPoint=FALSE, 
                                        byGuild=FALSE, guildType=NA, guildCategory=NA, wide=FALSE, 
                                        name.class="short", output="total",...){standardGeneric("birdRichness")}, signature="object")

setMethod(f="birdRichness", signature=c(object="list"),

  function(object, points, AOU, years, byPark, byYear, byPoint, byGuild, guildType, guildCategory, wide, name.class, output,...) {
    switch(output,
      list= return(
        lapply(X=object, FUN=birdRichness, points=points,AOU=AOU,years=years,visits=visits, byPark=byPark, byYear=byYear, byPoint=byPoint, 
               byGuild=ByGuild, guildType=guildType, guildCategory=guildCategory, wide=wide, name.class=name.class, output=output,...)
      ),
      total={
        Data<-getBirds(object=object,points=points,AOU=AOU,years=years,visits= visits, output="dataframe",...) %>% 
          mutate(ParkName=matchParkCodes(object, Admin_Unit_Code, name.class = name.class))
        years<-getVisits(object=object, points=points, years=years, visits= visits, output="dataframe") %>% distinct(Year) %>% pull() 
        
        if(byGuild){
          Guilds<- getGuilds(object=object, type=guildType, categories = guildCategory, output = "dataframe") %>% 
            select(AOU_Code, Guild=Response_Guild)
          Data<-Data %>% left_join(Guilds)
        }
        
        return(birdRichness(object=Data, years=years, byPark=byPark, byYear=byYear, byPoint=byPoint, 
                            byGuild=byGuild,wide=wide, output=output)      
      )}
    )
})


setMethod(f="birdRichness", signature=c(object="NCRNbirds"),
  function(object, points, AOU, years, byPark, byYear, byPoint, byGuild, guildType, guildCategory, wide, name.class, ...){

    Data<-getBirds(object=object,points=points,AOU=AOU,years=years,visits= visits, output="dataframe",...) %>% 
      mutate(ParkName=matchParkCodes(object, Admin_Unit_Code, name.class = name.class)) 
    years<-getVisits(object=object, points=points, years=years, output="dataframe") %>% distinct(Year) %>% pull() 
    
    if(byGuild){
      Guilds<- getGuilds(object=object, type=guildType, categories = guildCategory) %>% select(AOU_Code, Guild=Response_Guild)
      Data<-Data %>% left_join(Guilds)
    }
    
    return(birdRichness(object=Data, years=years, byPark=byPark, byYear=byYear, byPoint=byPoint, byGuild=byGuild, wide=wide))
    
})


setMethod(f="birdRichness", signature=c(object="data.frame"),
  function(object, years, byPark, byYear, byPoint, byGuild, wide){
    
  Count<-object %>% 
    {if(all(is.na(years))) . else filter(., Year %in% years)} %>% 
    {if(byPark) group_by(., Admin_Unit_Code,ParkName) else . } %>% 
    {if(byYear) group_by(.,Year, .add=TRUE) else .} %>% 
    {if(byPoint) group_by(., Point_Name, .add=TRUE) else .} %>% 
    {if(byGuild) group_by(., Guild, .add=TRUE) else .} %>% 
    summarise(Richness=n_distinct(AOU_Code)) %>% 
    {if(!byYear & !byPark & !byPoint & !byGuild) pull(., Richness) else .} %>% 
    {if(wide & byYear) pivot_wider(., names_from = Year, values_from = Richness, names_sort=T) else .} %>% 
    {if (is.data.frame(.)) ungroup(.) else .}

            
  return(Count)
})