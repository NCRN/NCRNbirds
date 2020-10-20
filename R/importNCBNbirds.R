#' @include NCRNbirds_Class_def.R
#' 
#' @title importNCBNbirds
#' 
#' @importFrom lubridate year mdy
#' @importFrom readr read_csv
#' 
#' @description  This function imports data from the standard NCBN .csv files and saves it as \code{NCRNbirds} objects. 
#' The required .csv files are: Points, Visits, FieldData, NCBNbands, NCBNintervals, BirdSpecies, and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 6 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importNCBNbirds<-function(Dir){

  
  InBands<-read_csv(paste(Dir,"NCBNbands.csv", sep="/"))
  InIntervals<-read_csv(paste(Dir,"NCBNintervals.csv", sep="/"))
  
  InPoints<-read_csv(paste(Dir,"Points.csv", sep="/"))
  
  InVisits<-read_csv(paste(Dir,"Visits.csv",sep="/"))
  InVisits$EventDate<-mdy(InVisits$EventDate)
  InVisits$Year<-year(InVisits$EventDate)

  
  InFieldData<-read_csv(paste(Dir,"FieldData.csv", sep="/"))
  InFieldData$EventDate<-mdy(InFieldData$EventDate)
  InFieldData$Year<-year(InFieldData$EventDate)
  
  InSpecies<-read_csv(paste(Dir,"BirdSpecies.csv", sep="/"))  
  InGuilds<-read_csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"))
  
  COLO<-new("NCRNbirds", 
            ParkCode="COLO", 
            ShortName="Colonial", 
            LongName="Colonial National Historical Park", 
            Network="NCBN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="COLO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="COLO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="COLO",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  
  
  return(c(COLO))
}
