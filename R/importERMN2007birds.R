#' @include NCRNbirds_Class_def.R
#' 
#' @title importERMN2007birds
#' 
#' @importFrom lubridate year mdy
#' @importFrom readr read_csv
#' 
#' @description  This function imports data from the standard ERMN.csv files and saves it as \code{NCRNbirds} objects. 
#' The required .csv files are: Points, Visits, FieldData , ERMNbands, ERMNintervals, BirdSpecies and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data are found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 6 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importERMN2007birds<-function(Dir){
 
  InBands<-read_csv(paste(Dir,"ERMNbands.csv", sep="/"))
  
  InIntervals<-read_csv(paste(Dir,"ERMNintervals.csv", sep="/"))
  
  InPoints<-read_csv(paste(Dir,"ERMNpoints.csv", sep="/"))
  
  InVisits<-read_csv(paste(Dir,"ERMNvisits.csv",sep="/"))
  InVisits$EventDate<-as.Date(as.character(InVisits$EventDate), format="%m/%d/%Y")
  InVisits$Year<-year(InVisits$EventDate)
  
  InFieldData<-read_csv(paste(Dir,"ERMNFieldData.csv", sep="/"))
  InFieldData$EventDate<-as.Date(as.character(InFieldData$EventDate), format="%m/%d/%Y")
  InFieldData$Year<-year(InFieldData$EventDate)
  
  InSpecies<-read_csv(paste(Dir,"BirdSpecies.csv", sep="/"))  
  InGuilds<-read_csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"))
  
  DEWA<-new("NCRNbirds", 
            ParkCode="DEWA", 
            ShortName="Delaware Water Gap NRA", 
            LongName="Delaware Water Gap National Recreation Area", 
            Network="ERMN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="DEWA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="DEWA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="DEWA",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  ALPO<-new("NCRNbirds", 
            ParkCode="ALPO", 
            ShortName="Allegheny Portage Railroad NHS", 
            LongName="Allegheny Portage Railroad National Historic Site", 
            Network="ERMN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ALPO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ALPO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ALPO",],
            Species=InSpecies,
            Guilds=InGuilds
            
  )
  
  
  FONE<-new("NCRNbirds", 
            ParkCode="FONE", 
            ShortName="Fort Necessity NB", 
            LongName="Fort Necessity National Battlefield", 
            Network="ERMN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="FONE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="FONE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="FONE",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  FRHI<-new("NCRNbirds", 
            ParkCode="FRHI", 
            ShortName="Friendship Hill NHS", 
            LongName="Friendship Hill National Historic Site", 
            Network="ERMN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="FRHI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="FRHI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="FRHI",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  GARI<-new("NCRNbirds", 
             ParkCode="GARI", 
             ShortName="Gauley River NRA", 
             LongName="Gauley River National Recreation Area", 
             Network="ERMN", 
             
             VisitNumber=4,
             Bands=InBands,
             Intervals=InIntervals,
             
             Points=InPoints[InPoints$Admin_Unit_Code=="GARI",], 
             Visits=InVisits[InVisits$Admin_Unit_Code=="GARI",],
             Birds=InFieldData[InFieldData$Admin_Unit_Code=="GARI",],
             Species=InSpecies,
             Guilds=InGuilds
   )
  
  NERI<-new("NCRNbirds", 
            ParkCode="NERI", 
            ShortName="New River Gorge NR", 
            LongName="New River Gorge National River", 
            Network="ERMN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="NERI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="NERI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="NERI",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  BLUE<-new("NCRNbirds", 
            ParkCode="BLUE", 
            ShortName="Bluestone NSR", 
            LongName="Bluestone National Scenic River", 
            Network="ERMN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="BLUE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="BLUE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="BLUE",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  return(c(ALPO,BLUE,DEWA,FONE,FRHI,NERI,GARI))
}