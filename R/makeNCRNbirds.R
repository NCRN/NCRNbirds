#' @include NCRNbirds_Class_def.R getPoints.R getVisits.R getBirds.R
#' 
#' @title makeNCRNBbirds
#' 
#' @description makes a new \code{NCRNbirds} object from one or more existing objects
#' 
#' @param object Either an \code{NCRNbirds} object or a \code{list} of such objects
#' @param ParkCode The parkcode for the new \code{NCRNbirds} object
#' @param ShortName The short name for the new \code{NCRNbirds} object
#' @param LongName  The long name for the new \code{NCRNbirds} object 
#' @param Network  The network code for the new \code{NCRNbirds} object
#' @param points An optional charcter vector with point names. When specificied, only data from the given poits will be included in the new \code{NCRNbirds} Object.
#' 
#' @details This function creates new \code{NCRNbirds} objects by combining two or more previously existing objects and/or by subsetting exisitng objects. If more than one object is provided then the data for these objects is combined. Providing a \code{points} argument will indicate which plots are in the new object. The user must indicate the new network code, park code and park names. 
#' 
#' @export


setGeneric(name="makeNCRNbirds",function(object,ParkCode,ShortName,LongName,Network,points=NA,...){standardGeneric("makeNCRNbirds")}, signature="object")

setMethod(f='makeNCRNbirds', signature=c(object="list"),
          function(object,...){
            if( all( sapply(X=object,FUN=class)=="NCRNbirds" ) ){
              new("NCRNbirds",
                  ParkCode=ParkCode,
                  ShortName=ShortName,
                  LongName=LongName,
                  
                  Network=Network,
                  
                  Points=getPoints(object, points=points, output="dataframe"),
                  Visits=getVisits(object, points=points, output="dataframe"),
                  Birds=getBirds(object, points=points, output="dataframe")
              )
            }
          }
)

setMethod(f='makeNCRNbirds', signature=c(object="NCRNbirds"),
          function(object,...){
            
            
            
            new("NCRNbirds",
                ParkCode=ParkCode,
                ShortName=ShortName,
                LongName=LongName,
                
                Network=Network,
                
                Points=getPoints(object,points=points),
                Visits=getVisits(object,points=points),
                Birds=getBirds(object,points=points)                                              
            )  
          }
)