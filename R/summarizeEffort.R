#' @include NCRNbirds_Class_def.R getVisits.R
#' 
#' @title summarizeEffort
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr distinct group_by n rename select summarise
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr pivot_wider
#' 
#' @description Produces a table indicating how many visits were made to parks and points.
#' 
#' 
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param byPark Defaults to \code{TRUE}. Indicates if data should be separated by park (\code{TRUE}) or combined (\code{FALSE})
#' @param byYear Defaults to \code{TRUE}. Indicates if data should be separated by year (\code{TRUE}) or combined (\code{FALSE})
#' @param byPoint Defaults to \code{FALSE}. Indicates if data should be separated by point (\code{TRUE}) or combined (\code{FALSE})
#' @param effort Indicates if effort should be measured by "points" (# of points visited) or "visits" (total # of visits).
#' @param wide Defaults to \code{FALSE}. If \code{TRUE} and \code{byYear} is \code{TRUE} then there will be a column for each year and row for each park 
#' or point. Otherwise there will be a single \code{Year} column to indicate the year. 
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{getVisits}
#' 
#' @details This produces a summary of the amoutn of effort put into monitoring either measured as the number points visited or the number of 
#' visits made to the points. The data on effort is retrieved from the \code{Visits} slot of the \code{NCRNbirds} object(s). The output can either be 
#' in long format - suitable for further use in a map or graph, or in wide format which is easier for viewing. Any argument which is valid for 
#' \code{getVisits} is also valid for this function. 
#' 
#'  
#' @export


########################


setGeneric(name="summarizeEffort", function(object, byPark=T, byYear=T, byPoint=F, effort="points", wide=F, output="dataframe", ...){
  standardGeneric("summarizeEffort")},  signature="object")

setMethod(f="summarizeEffort", signature=c(object="list"),
           function(object, byPark, byYear, byPoint, effort, wide, output, ...) {
  
# Get the correct data from getVisits and pass it on to the data.frame method
             
             VisitData<-getVisits(object=object, output=output, ...)
               
             switch(output,
                    list= return(map(VisitData, summarizeEffort, byPark=byPark, byYear=byYear, byPoint=byPoint, effort=effort, wide=wide)),
                    dataframe= return(summarizeEffort(VisitData, byPark=byPark, byYear=byYear, byPoint=byPoint, effort=effort, wide=wide))              
              )
})
 


setMethod(f="summarizeEffort", signature=c(object="NCRNbirds"),
           function(object, byPark, byYear, byPoint, effort, wide, ...){
             
# Get the correct data from getVisits and pass it on to the data.frame method
             
             VisitData<-getVisits(object, ...)
             
             return(summarizeEffort(object=VisitData, byPark=byPark, byYear=byYear, byPoint=byPoint, effort=effort, wide=wide))
})

setMethod(f="summarizeEffort", signature=c(object="data.frame"),
    function(object, byPark, byYear, byPoint, effort, wide){
             
    effort_table<-object %>% 
      dplyr::select(Admin_Unit_Code,Point_Name,Year) %>% 
      {if(effort=="points") distinct(.) else .} %>% 
      {if(byPark) group_by(., Admin_Unit_Code) else . }%>% 
      {if(byYear) group_by(., Year, add=T) else . } %>%
      {if(byPoint) group_by(., Point_Name, add=T) else . } %>%
      summarise(Point_Counts=n())  
      
    
    if(wide & byYear) effort_table<-effort_table %>% pivot_wider(names_from = Year, values_from = Point_Counts)
    
    if(!wide & effort=="points") effort_table<-effort_table %>% rename(Points=Point_Counts) 
    
    
    return(effort_table)         
})