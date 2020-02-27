#' @include NCRNbirds_Class_def.R
#'  
#' @title getPoints
#' 
#' @importFrom  dplyr group_by filter mutate select tbl_df
#' 
#' 
#' @description Returns the contents of the \code{Points} slot of an NCRNbirds object. The returned data can be filtered to meet various criteria.
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param times A numeric vector of lenght 1. Returns only data from points where the number of years that a point has been vistied is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param years A numeric vector. Returns data only from points where the years the point was visited  matches one of the values in \code{years} The year a visit takes place is determined by the \code{Year} column in the \code{visits} slot which is dervied from the imformation in the \code{EventDate} column.
#' @param site Character. Select sites in "Forest" or "Grassland" habitats. Defaults to selecting both site types.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#' @details This function returns point data either from a single NCRNbirds object or a list of such objects. The default output is a\code{data.frame}. However, if \code{object} is a list and \code{output} is "list" then a list of \code{data.frame}s will be returned. The name of each element in this list will correspond to the \code{ParkCode} in each NCRNbirds object. 
#'  
#'  
#' @export


setGeneric(name="getPoints",function(object,times=NA, years=NA, points=NA, site= NA, output="dataframe"){standardGeneric("getPoints")}, signature="object")


setMethod(f="getPoints", signature=c(object="list"),
          function(object,times,years,points,site, output) {
            OutPoints<-lapply(X=object, FUN=getPoints, times=times, years=years, points=points, site=site)
            switch(output,
                   list={#names(OutPoints)<-getNames(object,name.class="code")
                         return(OutPoints)},
                   dataframe=return(do.call("rbind",OutPoints))
            )
          })


setMethod(f="getPoints", signature=c(object="NCRNbirds"),
      function(object,times,years,points,site,output){
        
        XPoints<-object@Points
        
        if(!anyNA(times))      {
          X<-tbl_df(object@Visits) %>%
            group_by(Point_Name) %>%
            mutate(Times=length(unique(Year))) %>%
            filter(Times>=times) %>%
            dplyr::select(Point_Name) %>%
            unique
          XPoints<-XPoints[XPoints$Point_Name %in% X$Point_Name,]
          
        }
        
        if(!anyNA(years))XPoints<-XPoints[XPoints$Point_Name %in% object@Visits$Point_Name[object@Visits$Year %in% years],]
        
        #########################
        if(!anyNA(points)) XPoints<-XPoints[XPoints$Point_Name %in% points,]
        
        if(!anyNA(site)) XPoints<-XPoints[XPoints$Survey_Type %in% site,]
        
        return(XPoints)
        
      }
)

