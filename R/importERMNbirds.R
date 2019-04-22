#' @include NCRNbirds_Class_def.R
#' 
#' @title importERMNbirds
#' 
#' @importFrom lubridate year mdy
#' 
#' @description  This function imports data from the standard ERMN.csv files and saves it as \code{NCRNbirds} objects. The required .csv files are: Points, Visits, FieldData and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data are found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 6 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importERMNbirds<-function(Dir){
 
  InBands<-read.csv(paste(Dir,"ERMNbands.csv", sep="/"),as.is=T, header = T)
  
  InIntervals<-read.csv(paste(Dir,"ERMNintervals.csv", sep="/"),as.is=T, header = T)
  
  InPoints<-read.csv(paste(Dir,"ERMNpoints.csv", sep="/"),as.is=T, header=T)
  
  InVisits<-read.csv(paste(Dir,"ERMNvisits.csv",sep="/"),as.is=T, header=T)
  InVisits$EventDate<-as.Date(as.character(InVisits$EventDate), format="%m/%d/%Y")
  InVisits$Year<-year(InVisits$EventDate)
  
  InFieldData<-read.csv(paste(Dir,"ERMNFieldData.csv", sep="/"),as.is=T, header=T)
  InFieldData$EventDate<-as.Date(as.character(InFieldData$EventDate), format="%m/%d/%Y")
  InFieldData$Year<-year(InFieldData$EventDate)
  
  InSpecies<-read.csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"), as.is=T, header=T )
  
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
            #Guilds= InGuilds
  )
  
  
  ALPO<-new("NCRNbirds", 
            ParkCode="ALPO", 
            ShortName="Allagheny Portage Railroad NHS", 
            LongName="Allagheny Portage Railroad National Historic Site", 
            Network="ERMN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ALPO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ALPO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ALPO",],
            Species=InSpecies
            
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
            Species=InSpecies
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
            Species=InSpecies
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
            Species=InSpecies
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
            Species=InSpecies
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
            Species=InSpecies
  )
  
  
  return(c(ALPO,BLUE,DEWA,FONE,FRHI,GARI,NERI))
}