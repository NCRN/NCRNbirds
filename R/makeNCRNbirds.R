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
#' @slot VisitNumber An optional  \code{numeric} vector of length 1. The number of visits that is typically made to each point. This serves as the default number of visits for other functions.
#' @slot Bands An optional \code{data.frame} with metadata for the distance bands used during monitoring. Includes a name, min distance and max distance for each band.
#' @slot Intervals An optional \code{data.frame} with mmeatdata for the time intervals used during monitroing. Includes a name start time in minutes and end time in minutes for each interval.
#' @param points An optional charcter vector with point names. When specificied, only data from the given poits will be included in the new \code{NCRNbirds} Object.
#' 
#' @details This function creates new \code{NCRNbirds} objects by combining two or more previously existing objects and/or by 
#' subsetting exisitng objects. If more than one object is provided then the data for these objects is combined. Providing a \code{points} 
#' argument will indicate which points are in the new object. The user must indicate the new network code, park code and park names. 
#' 
#' The \code{VisitNumber}, \code{Bands}, \code{Intervals}, \code{Species} and \code{Guilds} arguments are all optional. If they are not specified,
#' and \code{object} is a single \code{NCRNbirds} object then the contents of these slots will be inherited from the original \code{object}. If 
#' \code{object} is a \code{list} of \code{NCRNbirds} objects then the default is to use the content of the first object in the \code{list}
#' to fill the output \code{object}.
#' 
#' 
#' @export


setGeneric(name="makeNCRNbirds",function(object,ParkCode,ShortName,LongName,Network,VisitNumber=NA,Bands=NA, Intervals=NA, Species=NA, 
                                         Guilds=NA, points=NA){standardGeneric("makeNCRNbirds")}, signature="object")

setMethod(f='makeNCRNbirds', signature=c(object="list"),
          function(object,ParkCode,ShortName,LongName,Network,VisitNumber,Bands,Intervals,Species,Guilds,points){
            if( all( sapply(X=object,FUN=class)=="NCRNbirds" ) ){
              new("NCRNbirds",
                  ParkCode=ParkCode,
                  ShortName=ShortName,
                  LongName=LongName,
                  
                  Network=Network,
                  
                  VisitNumber= if(is.na(VisitNumber)) object[[1]]@VisitNumber else VisitNumber,
                  Bands=if(is.na(Bands)) object[[1]]@Bands else Bands,
                  Intervals=if(is.na(Intervals)) object[[1]]@Intervals else Intervals,
                  
                  Points=getPoints(object, points=points, output="dataframe"),
                  Visits=getVisits(object, points=points, output="dataframe"),
                  Birds=getBirds(object, points=points, output="dataframe"),
                  
                  Species=if(is.na(Species)) object[[1]]@Species else Species,
                  Guilds=if(is.na(Guilds)) object[[1]]@Guilds else Guilds
              )
            }
          }
)

setMethod(f='makeNCRNbirds', signature=c(object="NCRNbirds"),
          function(object,ParkCode,ShortName,LongName,Network,VisitNumber,Bands,Intervals,Species,Guilds,points){
            
            
            
            new("NCRNbirds",
                ParkCode=ParkCode,
                ShortName=ShortName,
                LongName=LongName,
                
                Network=Network,
                
                VisitNumber= if(is.na(VisitNumber)) object@VisitNumber else VisitNumber,
                Bands=if(is.na(Bands)) object@Bands else Bands,
                Intervals=if(is.na(Intervals)) object@Intervals else Intervals,
                
                Points=getPoints(object,points=points),
                Visits=getVisits(object,points=points),
                Birds=getBirds(object,points=points),
                
                Species=if(is.na(Species)) object@Species else Species,
                Guilds=if(is.na(Guilds)) object@Guilds else Guilds
            )  
          }
)