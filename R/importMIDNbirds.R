#' @include NCRNbirds_Class_def.R
#' 
#' @title importMIDNbirds
#' 
#' @importFrom lubridate year mdy
#' 
#' @description  This function imports data from the standard MIDN .csv files and saves it as \code{NCRNbirds} objects. The required .csv files are: Points, Visits, FieldData and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 6 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importMIDNbirds<-function(Dir){

  
  InBands<-read.csv(paste(Dir,"MIDNbands.csv", sep="/"),as.is=T, header = T)
  
  InIntervals<-read.csv(paste(Dir,"MIDNintervals.csv", sep="/"),as.is=T, header = T)
  
  InPoints<-read.csv(paste(Dir,"Points.csv", sep="/"),as.is=T, header=T)
  
  InVisits<-read.csv(paste(Dir,"Visits.csv",sep="/"),as.is=T, header=T)
  InVisits$EventDate<-mdy(InVisits$EventDate)
  InVisits$Year<-year(InVisits$EventDate)

  
  InFieldData<-read.csv(paste(Dir,"FieldData.csv", sep="/"),as.is=T, header=T)
  InFieldData$EventDate<-mdy(InFieldData$EventDate)
  InFieldData$Year<-year(InFieldData$EventDate)
  
  InSpecies<-read.csv(paste(Dir,"BirdSpecies.csv", sep="/"), as.is=T, header=T )
  InGuilds<-read.csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"), as.is=T, header=T )
  
  APCO<-new("NCRNbirds", 
            ParkCode="APCO", 
            ShortName="Appomattox", 
            LongName="Appomattox Court House National Historical Park", 
            Network="MIDN", 
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="APCO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="APCO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="APCO",],
            Species=InSpecies,
            Guilds= InGuilds
  )
  
  
  BOWA<-new("NCRNbirds", 
            ParkCode="BOWA", 
            ShortName="Booker T.", 
            LongName="Booker T. Washington National Monument", 
            Network="MIDN", 
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="BOWA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="BOWA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="BOWA",],
            Species=InSpecies,
            Guilds= InGuilds
  )
  
  FRSP<-new("NCRNbirds", 
            ParkCode="FRSP", 
            ShortName="Fredericksburg", 
            LongName="Fredericksburg and Spotsylvania National Military Park", 
            Network="MIDN", 
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="FRSP",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="FRSP",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="FRSP",],
            Species=InSpecies,
            Guilds= InGuilds
  )
  
  PETE<-new("NCRNbirds", 
            ParkCode="PETE", 
            ShortName="Petersburg", 
            LongName="Petersburg National Battlefield", 
            Network="MIDN", 
            
            VisitNumber=2 ,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="PETE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="PETE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="PETE",],
            Species=InSpecies,
            Guilds= InGuilds
  )
  
  RICH<-new("NCRNbirds", 
            ParkCode="RICH", 
            ShortName="Richmond", 
            LongName="Richmond National Battlefield Park", 
            Network="MIDN", 
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="RICH",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="RICH",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="RICH",],
            Species=InSpecies,
            Guilds= InGuilds
  )
  
  VAFO<-new("NCRNbirds",
            ParkCode="VAFO",
            ShortName="Valley Forge",
            LongName="Valley Forge National Historical Park", 
            Network="MIDN", 
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="VAFO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="VAFO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="VAFO",],
            Species=InSpecies,
            Guilds= InGuilds
  ) 
  
  
  return(c(APCO,BOWA,FRSP,PETE,RICH,VAFO))
}
