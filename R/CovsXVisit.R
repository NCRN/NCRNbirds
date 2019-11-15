#' @include NCRNbirds_Class_def.R getVisits.R getDesign.R
#' @inheritParams getVisits
#' 
#' @title CovsXVisit
#' 
#' @description Produces a Covariate X Visit matrix for use in analyses
#' 
#' @importFrom dplyr distinct left_join mutate
#' @importFrom lubridate hour minute second yday
#' @importFrom magrittr %>%
#' @importFrom purrr compact reduce
#' @importFrom tidyr pivot_wider
#' 
#' @param object A \code{data.frame},  \code{NCRNbirds} object or a list of such objects. If the input is a \code{data.frame} in should be in the 
#' same format at the ouptut of \code{\link{getVisits}}
#' @param covs A character vector of one or more covariate names. Options inlcude:
#' \describe{
#' \item{"day"}{Provides the Julian day (aka ordinal day) for each visit.}
#' \item{"humidity"}{Provides the humidity measured for each visit.}
#' \item{"observer"}{Provides the observer for each visit.}
#' \item{"sky"}{Provides the sky condiiton (e.g. Clear, Cloudy, Fog etc.) for each visit.}
#' \item{"temp"}{Provides the temperature for each visit.}
#' \item{"time"}{Provides the time of day when each visit began. This is expressed in minutes since the start of the day.}
#' \item{"visit}{Indicates if a visit occurred or not. Each visit column will either have the visit number if a visit occured or \code{NA}
#' if it did not.}
#' }
#' 
#' @details This produces a Covariate(s) X Visit matrix for a \code{NCRNbirds} object or a \code{list} of such objects. Each row of the matrix
#'  will correspond to a different point in a different year. The columns of the matrix will be the park code, the point name, the year 
#'  visited, and a column for each covariate at each visit. These columns will be called "Day1", "Day2"  or  "Humidity1", "Humidity2" etc. to indicate
#'  the covariate and the visit.  
#'  
#'  If \code{visits} is left as \code{NA} then the visits used will be 1 through the number of visits indicated in the \code{visits} slot. 
#'  Otherwise a numeric vector e.g. c(1,2) can be used to select which visits are used. 
#'  
#'  If the input \code{object} is an \code{NCRNbirds} object or a list of such objects, the function will use the \code{\link{getVisits}} function to 
#'  retrieve the visit data. Currently if you supply a \code{data.frame} as the \code{object} the filtering by parks, points etc. will not occur, 
#'  all visits present in the input \code{data.frame} will be present it the ouput \code{data.frame}
#'     
#' @export


########################


setGeneric(name="CovsXVisit",function(object,covs,parks=NA,points=NA,years=NA,times=NA,visits=NA,reps=NA,
                                     output="dataframe"){standardGeneric("CovsXVisit")}, signature="object")

setMethod(f="CovsXVisit", signature=c(object="list"),
  function(object, covs, parks, points, years, times, visits, reps, output) {
    visits<-if(anyNA(visits)) 1:{getDesign(object,info="visits") %>% unlist %>% max} else visits
    switch(output,
      list= return(lapply(X=object, FUN=CovsXVisit, covs=covs,  parks=parks, points=points, years=years, times=times, visits=visits,
                          reps=reps)),
      dataframe= CovsXVisit(covs=covs, getVisits(object=object,parks=parks, points=points,years=years,times=times,visits=visits, reps=reps))
    )
})


setMethod(f="CovsXVisit", signature=c(object="NCRNbirds"),
  function(object,covs,parks,points,years,times,visits,reps){
    visits<-if(anyNA(visits)) 1:getDesign(object,info="visits") else visits
    CovMat<-getVisits(object=object, parks=parks, points=points, years=years, times=times, visits=visits, reps=reps)
    CovsXVisit(CovMat, covs=covs)
})

setMethod(f="CovsXVisit", signature=c(object="data.frame"),
  function(object,covs){
    
    if("time" %in% covs) {object<-object %>% mutate(StartTimeDec = 60*hour(StartTime) + minute(StartTime) + second(StartTime)/60 )}
    
    OutMats<-list(
      distinct(object, Admin_Unit_Code, Point_Name, Year),
      {if ("day" %in% covs) pivot_wider(data = object, id_cols = c(Admin_Unit_Code, Point_Name, Year), names_from=Visit, 
                                      names_prefix="Day",values_from=EventDate, values_fn=list(EventDate=yday) )},
      {if ("humidity" %in% covs) pivot_wider(data = object, id_cols = c(Admin_Unit_Code, Point_Name, Year), names_from=Visit, 
                                      names_prefix = "Humidity",values_from=Humidity )},
      {if ("observer" %in% covs) pivot_wider(data = object, id_cols = c(Admin_Unit_Code, Point_Name, Year), names_from=Visit, 
                                             names_prefix = "Observer",values_from=Observer )},
      {if ("sky" %in% covs) pivot_wider(data = object, id_cols = c(Admin_Unit_Code, Point_Name, Year), names_from=Visit, 
                                             names_prefix = "Sky",values_from=Sky )},
      {if ("temp" %in% covs) pivot_wider(data = object, id_cols = c(Admin_Unit_Code, Point_Name, Year), names_from=Visit, 
                                        names_prefix = "Temperature",values_from=Temperature )},
      {if ("time" %in% covs) pivot_wider(data = object, id_cols = c(Admin_Unit_Code, Point_Name, Year), names_from=Visit, 
                                         names_prefix = "Time",values_from=StartTimeDec)},
      {if ("visit" %in% covs) pivot_wider(data = object, id_cols = c(Admin_Unit_Code, Point_Name, Year), names_from=Visit, 
                                         names_prefix = "Visit",values_from=Visit)}
      )
    OutMats<-compact(OutMats)
    
    CovMat<-if(length(OutMats)==1) OutMats[[1]] else OutMats %>% reduce(left_join)
    
    return(CovMat)
})