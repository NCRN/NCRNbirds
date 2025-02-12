#' @include NCRNbirds_Class_def.R
#' 
#' @title importCNNAPbirds
#' 
#' @importFrom lubridate year mdy
#' @importFrom readr read_csv
#' 
#' @description  This function imports data from the standard CNNAP .csv files and saves it as \code{NCRNbirds} objects. 
#' The required .csv files are: Points, Visits, FieldData, CNNAPbands, CNNAPintervals, BirdSpecies, and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns 1 \code{NCRNbirds} object.
#' 
#' @export


importCNNAPbirds<-function(Dir){
  
  
  InBands<-read_csv(paste(Dir,"CNNAPbands.csv", sep="/"))
  InIntervals<-read_csv(paste(Dir,"CNNAPintervals.csv", sep="/"))
  
  InPoints<-read_csv(paste(Dir,"Points.csv", sep="/"))
  
  InVisits<-read_csv(paste(Dir,"Visits.csv",sep="/"))
  InVisits$EventDate<-mdy(InVisits$EventDate)
  InVisits$Year<-year(InVisits$EventDate)
  
  
  InFieldData<-read_csv(paste(Dir,"FieldData.csv", sep="/"))
  InFieldData$EventDate<-mdy(InFieldData$EventDate)
  InFieldData$Year<-year(InFieldData$EventDate)
  
  InSpecies<-read_csv(paste(Dir,"BirdSpecies.csv", sep="/"))  
  InGuilds<-read_csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"))
  
  
  CNNAP<-new("NCRNbirds",
             ParkCode="CNNAP",
             ShortName="Crow's Nest",
             LongName="Crow's Nest Natural Area Preserve", 
             Network="CNNAP", 
             
             VisitNumber=4,
             Bands=InBands,
             Intervals=InIntervals,
             
             Points=InPoints[InPoints$Admin_Unit_Code=="CNNAP",], 
             Visits=InVisits[InVisits$Admin_Unit_Code=="CNNAP",],
             Birds=InFieldData[InFieldData$Admin_Unit_Code=="CNNAP",],
             Species=InSpecies,
             Guilds=InGuilds
  ) 
  
  
  return(c(CNNAP))
}
