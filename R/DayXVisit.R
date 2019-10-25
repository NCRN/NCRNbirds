#' @include NCRNbirds_Class_def.R getVisits.R getDesign.R
#' @inheritParams getVisits
#' 
#' @title DayXVisit
#' 
#' @description Produces a Day X Visit matrix for use in analyses
#' 
#' @importFrom dplyr group_by mutate select summarize ungroup
#' @importFrom lubridate yday
#' @importFrom magrittr %>%
#' @importFrom tidyr spread 
#' 
#' @param object A \code{data.frame},  \code{NCRNbirds} object or a list of such objects. If the input is a \code{data.frame} in should be in the 
#' same format at the ouptut of \code{\link{getVisits}}
#' 
#' @details This produces a Day X Visit matrix for a \code{NCRNbirds} object or a \code{list} of such objects. Each row of the matrix
#'  will correspond to a different point in a different year. The columns of the matrix will be the park code, the point name, the year 
#'  visited, and a column for each visit that indicates the ordinal day of the visit. These columns will be called "Day1", "Day2" etc. 
#'  
#'  If \code{visits} is left as \code{NA} then the visits used will be 1 through the number of visits indicated in the \code{visits} slot. 
#'  Otherwise a numeric vectore e.g. c(1,2) can be used to select which visits are used. 
#'  
#'  If the input \code{object} is an \code{NCRNbirds} object or a list of such objects, the function will use the \code{\link{getVisits}} function to 
#'  retrieve the visit data. Currently if you supply a \code{data.frame} as the \code{object} the filtering by parks, points etc. will not occur, 
#'  all visits present in the input \code{data.frame} will be present it the ouput \code{data.frame}
#'     
#' @export


########################


setGeneric(name="DayXVisit",function(object,parks=NA,points=NA,years=NA,times=NA,visits=NA,reps=NA,
                                     output="dataframe"){standardGeneric("DayXVisit")}, signature="object")

setMethod(f="DayXVisit", signature=c(object="list"),
  function(object, parks, points, years, times, visits, reps,output) {
    visits<-if(anyNA(visits)) 1:{getDesign(object,info="visits") %>% unlist %>% max} else visits
    switch(output,
      list= lapply(X=object, FUN=DayXVisit, parks=parks, points=points, years=years, times=times,visits=visits,reps=reps,output="dataframe"),
      dataframe= DayXVisit(getVisits(object=object,parks=parks, points=points,years=years,times=times,visits=visits, reps=reps,output="dataframe"))
    )
})


setMethod(f="DayXVisit", signature=c(object="NCRNbirds"),
  function(object,parks,points,years,times,visits,reps, output){
    visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
    DayMat<-getVisits(object=object, parks=parks, points=points, years=years, times=times, visits=visits, reps=reps, output="dataframe")
    DayXVisit(DayMat)
})

setMethod(f="DayXVisit", signature=c(object="data.frame"),
  function(object){
    DayMat<-object %>%
      dplyr::select(Admin_Unit_Code,Point_Name,EventDate,Visit,Year) %>%
      mutate(Visit=paste0("Day",Visit), Ord_Day=yday(EventDate)) %>%
      group_by(Admin_Unit_Code,Point_Name, Year, Visit) %>%
      summarize(Day=max(Ord_Day)) %>%
      spread(key=Visit,value=Day,fill=NA) %>% 
      ungroup  # to fix errors with dplyr when maniplating grouped tables
    
    return(DayMat)
})