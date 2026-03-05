#' @include NCRNbirds_Class_def.R
#' 
#' @title importEGRPbirds
#' 
#' @importFrom lubridate year mdy
#' @importFrom readr read_csv
#' 
#' @description  This function imports data for the Eastern Grassland Restoration Project (EGRP) from the standard MIDN .csv files and saves it as \code{NCRNbirds} objects. 
#' The required .csv files are: Points, Visits, FieldData, MIDNbands, MIDNintervals, BirdSpecies, and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 18 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importEGRPbirds<-function(Dir){
  
  
  InBands<-read_csv(paste(Dir,"MIDNbands.csv", sep="/"))
  InIntervals<-read_csv(paste(Dir,"MIDNintervals.csv", sep="/"))
  
  InPoints<-read_csv(paste(Dir,"Points.csv", sep="/"))
  
  InVisits<-read_csv(paste(Dir,"Visits.csv",sep="/"))
  InVisits$EventDate<-mdy(InVisits$EventDate)
  InVisits$Year<-year(InVisits$EventDate)
  
  
  InFieldData<-read_csv(paste(Dir,"FieldData.csv", sep="/"))
  InFieldData$EventDate<-mdy(InFieldData$EventDate)
  InFieldData$Year<-year(InFieldData$EventDate)
  
  InSpecies<-read_csv(paste(Dir,"BirdSpecies.csv", sep="/"))  
  InGuilds<-read_csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"))
  
  ## ERMN Parks  ##
  
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
  
  FLNI<-new("NCRNbirds", 
            ParkCode="FLNI", 
            ShortName="Flight 93 NM", 
            LongName="Flight 93 National Monument", 
            Network="ERMN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="FLNI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="FLNI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="FLNI",],
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
  
 ## MIDN  Parks ##
  
  BOWA<-new("NCRNbirds", 
            ParkCode="BOWA", 
            ShortName="Booker T.", 
            LongName="Booker T. Washington National Monument", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="BOWA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="BOWA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="BOWA",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  CEBE<-new("NCRNbirds", 
            ParkCode="CEBE", 
            ShortName="Cedar Creek and Belle Grove NHP", 
            LongName="Cedar Creek and Belle Grove National Historical Park", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="CEBE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="CEBE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="CEBE",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  FRSP<-new("NCRNbirds", 
            ParkCode="FRSP", 
            ShortName="Fredericksburg", 
            LongName="Fredericksburg and Spotsylvania National Military Park", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="FRSP",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="FRSP",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="FRSP",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  GETT<-new("NCRNbirds",
            ParkCode="GETT",
            ShortName="Gettysburg",
            LongName="Gettysburg National Military Park", 
            Network="MIDN", 
            
            VisitNumber=8,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="GETT",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="GETT",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="GETT",],
            Species=InSpecies,
            Guilds=InGuilds
  ) 
  
  PETE<-new("NCRNbirds", 
            ParkCode="PETE", 
            ShortName="Petersburg", 
            LongName="Petersburg National Battlefield", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="PETE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="PETE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="PETE",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  RICH<-new("NCRNbirds", 
            ParkCode="RICH", 
            ShortName="Richmond", 
            LongName="Richmond National Battlefield Park", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="RICH",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="RICH",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="RICH",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  SHEN<-new("NCRNbirds", 
            ParkCode="SHEN", 
            ShortName="Shenandoah", 
            LongName="Shenandoah National Park", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="SHEN",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SHEN",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SHEN",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  VAFO<-new("NCRNbirds",
            ParkCode="VAFO",
            ShortName="Valley Forge",
            LongName="Valley Forge National Historical Park", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="VAFO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="VAFO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="VAFO",],
            Species=InSpecies,
            Guilds=InGuilds
  ) 
  
  
  ## NETN Parks ##
  
  APPA<-new("NCRNbirds", 
            ParkCode="APPA", 
            ShortName="Appalachian NST", 
            LongName="Appalachian National Scenic Trail", 
            Network="NETN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="APPA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="APPA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="APPA",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  MABI<-new("NCRNbirds", 
            ParkCode="MABI", 
            ShortName="Marsh-Billings-Rockefeller", 
            LongName="Marsh-Billings-Rockefeller National Historical Park", 
            Network="NETN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MABI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MABI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MABI",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  MIMA<-new("NCRNbirds", 
            ParkCode="MIMA", 
            ShortName="Minute Man", 
            LongName="Minute Man National Historical Park", 
            Network="NETN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MIMA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MIMA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MIMA",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  SAGA<-new("NCRNbirds", 
            ParkCode="SAGA", 
            ShortName="Saint-Gaudens", 
            LongName="Saint-Gaudens National Historic Site", 
            Network="NETN",
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="SAGA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SAGA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SAGA",],
            Species=InSpecies,
            Guilds=InGuilds
  ) 
  
  SARA<-new("NCRNbirds", 
            ParkCode="SARA", 
            ShortName="Saratoga", 
            LongName="Saratoga National Historical Park", 
            Network="NETN",
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="SARA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SARA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SARA",],
            Species=InSpecies,
            Guilds=InGuilds
  ) 
  
  
  return(c(DEWA,FLNI,FONE,FRHI,NERI,BOWA,CEBE,FRSP,GETT,PETE,RICH,SHEN,VAFO,APPA,MABI,MIMA,SAGA,SARA))
  
}
