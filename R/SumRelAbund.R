#' @include NCRNbirds_Class_def.R getVisits.R getBirds.R getDesign.R
#' 
#' @title SumRelAbund
#' 
#' @description Produces a summary of raw detections by species for plotting and analysis.
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr arrange group_by left_join mutate n select slice summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyr complete nesting
#' 
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' Detections will be summed by each individual species.
#' @param years  A vector of numbers. Will return only data from the indicated years.
#' @param times  A numeric vector of length 1 passed on to \code{\link{getVisits}}. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param band  A numeric vector. Defaults to 1. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits  The visits that will be used for the matrix. Defautls to \code{NA}. See Details below,
#' @param CalcByYear  If \code{TRUE}, will calculate mean detections across all visits per year. Defaults to calculating per visit.
#' @param SortRelAbund If \code{TRUE}, when multiple species are selected it will calculate and sort relative abundance per species. See \code{abund}.
#' @param abund When \code{SortRelAbund} = \code{TRUE}, used to provide a numeric value to select the most abundant species.
#' E.g., abund = 10 will return mean detections of the top 10 species. You can use the returned  \code{data.frame} to provide species AOU.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{getBirds}
#' 
#' @details Summaries relative abundance by species (mean detections per point) for a \code{NCRNbirds} object or a \code{list} of such objects. 
#'  
#'  If \code{visits} is left as \code{NA} then the visits used will be 1 through the number of visits indicated in the \code{visits} slot. 
#'  Otherwise a numeric vectore e.g. c(1,2) can be used to select which visits are used. 
#'  
#'  \code{SumRelAbund} requires at least 2 monitoring points or at least 2 years of data to be specified, unless both \code{CalcByYear} and
#'   \code{SortRelAbund} are \code{TRUE}. 
#'  
#'  If \code{SortRelAbund} is \code{TRUE}, then data will be combined accross the visits, points and years indicated by the other arguments. 
#'     
#' @export

setGeneric(name="SumRelAbund",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=NA, CalcByYear= FALSE,
    SortRelAbund=FALSE, abund = 10, output="dataframe",...){standardGeneric("SumRelAbund")}, signature="object")

setMethod(f="SumRelAbund", signature=c(object="list"),
          function(object, points, AOU, years,times, band, visits, CalcByYear,SortRelAbund, abund, output,...) {
            OutMat<-lapply(X=object, FUN=SumRelAbund, points=points, AOU=AOU, years=years, 
                           times=times,band=band,visits=visits, CalcByYear=CalcByYear,SortRelAbund=SortRelAbund, abund=abund, output=output, ...)
            switch(output,
                   list= return(OutMat),
                   dataframe= return(rbindlist(OutMat, use.names=TRUE, fill=TRUE)) #return(bind_rows(OutMat))
            )
          })


setMethod(f="SumRelAbund", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,band,visits,CalcByYear,SortRelAbund, abund, output, ...){

# construct bird data matrix
birddata<- getBirds(object=object, points=points, AOU=AOU, years=years, band=band, ...) %>% #
  dplyr::select(Admin_Unit_Code, AOU_Code, Point_Name, EventDate, Year, Bird_Count) %>% 
  group_by(Admin_Unit_Code, AOU_Code, Point_Name, EventDate,Year) %>% 
  dplyr::summarise(value = sum(Bird_Count, na.rm= TRUE))# sum counts per point, date, and year

# construct Visit matrix
visits<- getVisits(object=object,points=points,years=years,times=times,visits=visits) %>% # find number of points sampled
  dplyr::select(Admin_Unit_Code,Point_Name,EventDate,Visit,Year) 
  
# combine, add missing values (0s), and calculate mean and se of raw detections
df<- visits %>%  
  dplyr::left_join(birddata) %>% 
  dplyr::mutate(visit=paste0("Visit",Visit)) %>% 
  dplyr::select(Admin_Unit_Code,Point_Name,visit,AOU_Code,Year,value) %>%  
  tidyr::complete(Admin_Unit_Code,Point_Name,AOU_Code, nesting( visit, Year),fill = list(value = 0)) # add in explicit missing values (0) per visit
  

if(!CalcByYear){ df <-df %>% 
  dplyr::group_by(Admin_Unit_Code,AOU_Code,visit, Year)} else{
    
   df <-df%>% dplyr::group_by(Admin_Unit_Code,AOU_Code, Year)} 

df <-df %>%
  dplyr::summarise(Total= sum(value, na.rm=T), Mean= round(mean(value, na.rm=T),digits=3),
                   se= round(sd(value, na.rm=T)/sqrt(n()),digits=3), n=n()) %>% 
  na.omit()
  

if(!SortRelAbund){
  
  return(ungroup(df))
  
}else{
  df<- df%>% 
    group_by(Admin_Unit_Code,AOU_Code) %>% 
    dplyr::summarise(Mean_total = mean(Mean)) %>% 
    arrange(desc(Mean_total)) %>% 
    slice(1:abund)
  return(ungroup(df))
}
  

 

}
) 
 