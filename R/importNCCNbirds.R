#' @include NCRNbirds_Class_def.R
#' 
#' @title importNCCNbirds
#' 
#' @importFrom lubridate year mdy
#' @importFrom readr read_csv
#' 
#' @description  This function imports data from the standard NCCN .csv files and saves it as \code{NCRNbirds} objects. 
#' The required .csv files are: Points, Visits, FieldData NCCNbands, NCCNintervals, BirdSpecies, and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 6 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importNCCNbirds<-function(Dir){

  
  InBands<-read_csv(paste(Dir,"NCCNbands.csv", sep="/"))
  InIntervals<-read_csv(paste(Dir,"NCCNintervals.csv", sep="/"))
  
  InPoints<-read_csv(paste(Dir,"Points.csv", sep="/"))
  
  InVisits<-read_csv(paste(Dir,"Visits.csv",sep="/"))
  InVisits$EventDate<-mdy(InVisits$EventDate)
  InVisits$Year<-year(InVisits$EventDate)

  
  InFieldData<-read_csv(paste(Dir,"FieldData.csv", sep="/"))
  InFieldData$EventDate<-mdy(InFieldData$EventDate)
  InFieldData$Year<-year(InFieldData$EventDate)
  
  InSpecies<-read_csv(paste(Dir,"BirdSpecies.csv", sep="/"))  
  InGuilds<-read_csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"))
  
  LEWI<-new("NCRNbirds", 
            ParkCode="LEWI", 
            ShortName="Lewis and Clark", 
            LongName="Lewis and Clark National Historical Park ", 
            Network="NCCN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="LEWI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="LEWI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="LEWI",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  MORA<-new("NCRNbirds", 
            ParkCode="MORA", 
            ShortName="Mount Rainier", 
            LongName="Mount Rainier National Park", 
            Network="NCCN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MORA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MORA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MORA",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  NOCA<-new("NCRNbirds", 
            ParkCode="NOCA", 
            ShortName="North Cascades", 
            LongName="North Cascades National Park", 
            Network="NCCN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="NOCA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="NOCA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="NOCA",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  OLYM<-new("NCRNbirds", 
            ParkCode="OLYM", 
            ShortName="Olympic", 
            LongName="Olympic National Park", 
            Network="NCCN", 
            
            VisitNumber=4 ,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="OLYM",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="OLYM",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="OLYM",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  SAJH<-new("NCRNbirds", 
            ParkCode="SAJH", 
            ShortName="San Juan Island", 
            LongName="San Juan Island National Historical Park", 
            Network="NCCN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="SAJH",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SAJH",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SAJH",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  
  return(c(LEWI,MORA,NOCA,OLYM,SAJH))
}
