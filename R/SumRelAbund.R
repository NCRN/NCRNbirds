#' @include NCRNbirds_Class_def.R getChecklist.R CountXVisit.R getBirds.R getVisits.R
#' 
#' @title SumRelAbund
#' 
#' @description Produces a summary of raw detections by species for plotting and analysis.
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr arrange group_by  mutate n select slice summarize ungroup filter
#' @importFrom magrittr %>%
#' @importFrom tidyr  gather
#' @importFrom purrr map_dfr 
#' @importFrom rlang set_names
#' 
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param parks A character vector of park codes. Only visits within these parks will be returned.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species. Detections will be summed by each individual species.
#' @param years  A vector of numbers. Will return only data from the indicated years.
#' @param times  A numeric vector of length 1 passed on to \code{\link{getVisits}} and \code{\link{getBirds}}. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param band  A numeric vector. Defaults to 1. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits  The visits that will be used for the matrix. Defautls to \code{NA}. See Details below,
#' @param max Logical, if \code{TRUE} (default), relative abundance will be calculated from the maximum count among visits in a given year.
#' @param CalcByYear  Logical, if \code{TRUE}, will calculate mean detections across all visits per year. Defaults to \code{FALSE}, calculating per visit.
#' @param sort Logical, if \code{TRUE}, when multiple species are selected it will calculate and sort relative abundance per species. See \code{abund}.
#' @param abund Numeric, When \code{sort} = \code{TRUE}, used to provide a numeric value to select the most abundant species.
#' E.g., abund = 10 will return mean detections of the top 10 species. You can use the returned  \code{data.frame} to provide species AOU.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{getChecklist}
#' 
#' @details Summarizes relative abundance by species (mean detections per point) for a \code{NCRNbirds} object or a \code{list} of such objects. 
#'  
#'  If \code{visits} is left as \code{NA} then the visits used will be 1 through the number of visits indicated in the \code{visits} slot. 
#'  Otherwise a numeric vectore e.g. c(1,2) can be used to select which visits are used. 
#'  
#'  \code{SumRelAbund} requires at least 2 monitoring points or at least 2 years of data to be specified, unless both \code{CalcByYear} and
#'   \code{sort} are \code{TRUE}. 
#'  
#'  If \code{sort} is \code{TRUE}, then data will be combined accross the visits, points and years indicated by the other arguments. 
#'     
#' @export

setGeneric(name="SumRelAbund",function(object,parks= NA, points=NA,AOU=NA,years=NA,times=NA,band=1,visits=NA, CalcByYear= FALSE,max=TRUE,
                                       sort=FALSE, abund = 10, output="dataframe",...){standardGeneric("SumRelAbund")}, signature="object")

setMethod(f="SumRelAbund", signature=c(object="list"),
          function(object, parks, points, AOU, years,times, band, visits, CalcByYear,max, sort, abund, output,...) {
            OutMat<-lapply(X=object, FUN=SumRelAbund, parks=parks, points=points, AOU=AOU, years=years, 
                           times=times,band=band,visits=visits, CalcByYear=CalcByYear,max=max, sort=sort, abund=abund, output=output, ...)            
            switch(output,
                   list= return(OutMat),
                   dataframe= return(rbindlist(OutMat, use.names=TRUE, fill=TRUE)) #return(bind_rows(OutMat))
            )
          })


setMethod(f="SumRelAbund", signature=c(object="NCRNbirds"),

          function(object,parks, points,AOU,years,times,band, visits,CalcByYear,max, sort, abund, output, ...){
            
            # create vector of bird names
            
            BirdNames<-getChecklist(object=object, points=points, AOU=AOU, years=years, times=times, band=band, ..., output = "dataframe") %>% 
              as.vector()
            
            # create detection matrix per species, year, and visit and calc mean, se and N
            
            data<-map_dfr(.x=set_names(BirdNames, nm=BirdNames),.id= "AOU_Code", 
              ~CountXVisit(object=object,AOU= .x, parks=parks, points=points, times=times, visits=visits, years=years, band=band, max=max)) 
            
            # check to see if detections exist
            if(nrow(data) == 0){
              cat("No detections are available for those specific survey criteria.  ")
            }else{
              data<- data %>%  
                tidyr::gather(visit, value, -AOU_Code, -Admin_Unit_Code, -Point_Name,-Year)  %>%  #reshape
              {if(max) dplyr::filter(. , visit %in% "Max") else dplyr::filter(., !visit %in% "Max") } %>%    # select the visit(s) and set grouping to summarize data by
              {if(CalcByYear)  dplyr::group_by(.,Admin_Unit_Code,AOU_Code, Year) else # sum across all visits and years
              dplyr::group_by(., Admin_Unit_Code,AOU_Code, visit, Year)} %>%  # sum across all visits by year
              dplyr::summarize(.,Total= sum(value, na.rm=TRUE), Mean= round(mean(value, na.rm=TRUE),digits=3), 
                             se= round(sd(value, na.rm=TRUE)/sqrt(n()),digits=3), n=n())  # calc mean and se
            
            }
            
            # do you want to also sort data and return most common species?
            
            if(!sort){
              return(ungroup(data))
              
            }else{
              df<- data %>% 
                group_by(Admin_Unit_Code,AOU_Code) %>% 
                dplyr::summarise(Mean_total = mean(Mean, na.rm=TRUE)) %>% 
                arrange(desc(Mean_total)) %>% 
                slice(1:abund)
              return(ungroup(df))
            }
            
            
            
            
          }
) 
