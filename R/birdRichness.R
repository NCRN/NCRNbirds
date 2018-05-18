#' @include NCRNbirds_Class_def.R getBirds.R
#' @include getVisits.R
#' @include getBirds.R
#' 
#' @title birdRichness
#' 
#' @importFrom dplyr distinct filter n_distinct pull summarise
#' @importFrom magrittr %>% 
#' 
#' @description Returns the number of bird species found in a park, at a point or a collection of points. 
#' 
#' @param object An NCRNbirds object or a list of such objects or a \code{data.frame} like that created by \code{\link{getBirds}}.
#' @param points A character vector passed to \code{\link{getBirds}}. The names of one or more points where the data was collected.
#'  \code{NA}(the default) measures richness from all points.
#' @param AOU  A character vector passed to \code{\link{getBirds}}. One or more AOU (American Ornothological Union) codes of bird species. 
#' \code{NA} (the default) measures richness based on all species, otherwise only species listed in \code{AOU} will be counted.
#' @param years  A vector of numbers passed to \code{\link{getBirds}}. \code{NA} (the default) will count species regardless of the year
#'  they were monitored, otherwise only data from years listed in \code{years} will be used. See details below for 
#'  how years where no visits took place are handeled.  
#' @param byYear Logical. If \code{FALSE} (the default) the total species richness across all years will be returned a single numeric value,
#'  if \code{TRUE} a data.frame will be returned with each row a different year and its correspoding species richnes
#' @param output Either "total" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function, 
#' when \code{object} is a \code{list}. "total" will give the number of distinct species found across all parks. 
#' "list" will return a list, with each entry to the list corresponding to the species richness of one of the \code{NCRNbirds} objects in the input list.  
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

setGeneric(name="birdRichness",function(object,points=NA,AOU=NA,years=NA,byYear=FALSE,
                                        output="total",...){standardGeneric("birdRichness")}, signature="object")

setMethod(f="birdRichness", signature=c(object="list"),
  function(object,points,AOU,years,byYear,output,...) {
    switch(output,
      list= return(
        lapply(X=object, FUN=birdRichness, points=points,AOU=AOU,years=years,byYear=byYear,output=output,...)
      ),
      total={
        Data<-getBirds(object=object,points=points,AOU=AOU,years=years,output="dataframe",...)
        years<-getVisits(object=object, points=points, years=years, output="dataframe") %>% distinct(Year) %>% pull() 
        return(birdRichness(object=Data, years=years, byYear=byYear, output=output)      
      )}
    )
})


setMethod(f="birdRichness", signature=c(object="NCRNbirds"),
  function(object,points,AOU,years,byYear,output,...){

    Data<-getBirds(object=object,points=points,AOU=AOU,years=years,output="dataframe",...)
    years<-getVisits(object=object, points=points, years=years, output="dataframe") %>% distinct(Year) %>% pull() 
    return(birdRichness(object=Data, years=years, byYear=byYear))
})



setMethod(f="birdRichness", signature=c(object="data.frame"),
  function(object,years,byYear){
            
    if(all(is.na(years))) { #years is NA, so just get years from data
      count<-object %>% {if(byYear) group_by(., Year) else .} %>% summarise(Richness=n_distinct(AOU_Code))
      if(!byYear) count<-pull(count)
    } else {               # years is specified, so use them.
      object<-object %>% filter(Year %in% years)
      if(!byYear) {count<-pull(object,AOU_Code) %>% n_distinct() 
      } else {
        count<-data.frame(Year=years, Richness=sapply(years, FUN=function(x){object %>% 
        filter(Year==x) %>% pull(AOU_Code) %>%  n_distinct()})) 
      }
    }
  return(count)
})