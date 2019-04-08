#' @include NCRNbirds_Class_def.R
#' 
#' @title getParkData
#' 
#' @importFrom lubridate year
#' 
#' @description  This function imports data from the standard Networks or Park Units as .csv files and saves it as \code{NCRNbirds} objects. The required .csv files are: Points, Visits, FieldData and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' @param UnitCode  A character string (e.g., NCRN) to retrieve and assemble data for a desired park unit or network.

#' @return Returns a list of 13 \code{NCRNbirds} objects, one for each park.
#' 
#' @export



getParkData<-function(Dir,UnitCode){
  
  InBands<-read.csv(paste(Dir,"NCRNbands.csv", sep="/"),as.is=T, header = T)
  
  InIntervals<-read.csv(paste(Dir,"NCRNintervals.csv", sep="/"),as.is=T, header = T)
  
  InPoints<-read.csv(paste(Dir,"Points.csv", sep="/"),as.is=T, header=T)
 
  InVisits<-read.csv(paste(Dir,"Visits.csv",sep="/"),as.is=T, header=T)
  InVisits$EventDate<-as.Date(as.character(InVisits$EventDate), format="%m/%d/%Y")
  InVisits$Year<-year(InVisits$EventDate)
  InVisits$Month<-months(InVisits$EventDate)
  InVisits$Ord.Day<-as.integer(format(as.Date(InVisits$EventDate, format = "%m/%d/%Y"), "%j"))
  
  InFieldData<-read.csv(paste(Dir,"FieldData.csv", sep="/"),as.is=T, header=T)
  InFieldData$EventDate<-as.Date(as.character(InFieldData$EventDate))

  InSpecies<-read.csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"), as.is=T, header=T )
  
  NCRN<-new("NCRNbirds", 
            
            ParkCode="NCRN", 
            ShortName="Nat. Cap. Region", 
            LongName="National Capital Region Network", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints, 
            Visits=InVisits,
            Birds=InFieldData,
            Species=InSpecies
  )
  
  
  
  ANTI<-new("NCRNbirds", 
            
            ParkCode="ANTI", 
            ShortName="Antietam", 
            LongName="Antietam National Battlefield", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ANTI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ANTI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ANTI",],
            Species=InSpecies
  )
  
  
  CATO<-new("NCRNbirds", 
            ParkCode="CATO", 
            ShortName="Catoctin", 
            LongName="Catoctin Mountain Park", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="CATO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="CATO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="CATO",],
            Species=InSpecies
  )
  
  CHOH<-new("NCRNbirds", 
            ParkCode="CHOH", 
            ShortName="C&O Canal", 
            LongName="Chesapeake & Ohio Canal National Historical Park", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="CHOH",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="CHOH",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="CHOH",],
            Species=InSpecies
  )
  
  GWMP<-new("NCRNbirds", 
            ParkCode="GWMP", 
            ShortName="GW Parkway", 
            LongName="George Washington Memorial Parkway", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
             
            Points=InPoints[InPoints$Admin_Unit_Code=="GWMP",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="GWMP",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="GWMP",],
            Species=InSpecies
  )
  
  GREE<-new("NCRNbirds", 
            ParkCode="GREE", 
            ShortName="Greenbelt", 
            LongName="Greenbelt Park", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="GREE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="GREE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="GREE",],
            Species=InSpecies
  )
  
  
  HAFE<-new("NCRNbirds", 
            ParkCode="HAFE", 
            ShortName="Harpers Ferry", 
            LongName="Harpers Ferry National Histroical Park", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="HAFE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="HAFE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="HAFE",],
            Species=InSpecies
  )
  
  MANA<-new("NCRNbirds",
            ParkCode="MANA",
            ShortName="Manassas",
            LongName="Manassas National Battlefield Park", 
            Network="NCRN", 
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MANA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MANA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MANA",],
            Species=InSpecies
  ) 
  
  MONO<-new("NCRNbirds",
            ParkCode="MONO",
            ShortName="Monocacy",
            LongName="Monocacy National Battlefield", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MONO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MONO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MONO",],
            Species=InSpecies
  ) 
  
  NACE<-new("NCRNbirds",
            ParkCode="NACE",
            ShortName="Nat.Cap.Parks - East",
            LongName="National Captial Parks-East", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="NACE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="NACE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="NACE",],
            Species=InSpecies
  ) 
  
  PISC_FOWA<-new("NCRNbirds",
            ParkCode="PISC_FOWA",
            ShortName="Pisc./Ft. Wash.",
            LongName="Piscataway/Fort Washington", 
            Network="NCRN",
                 
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="PISC_FOWA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="PISC_FOWA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="PISC_FOWA",],
            Species=InSpecies
  ) 
  
  
  PRWI<-new("NCRNbirds",
            ParkCode="PRWI",
            ShortName="Prince William",
            LongName="Prince William Forest Park", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="PRWI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="PRWI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="PRWI",],
            Species=InSpecies
  ) 
  
  ROCR<-new("NCRNbirds",
            ParkCode="ROCR",
            ShortName="Rock Creek",
            LongName="Rock Creek Park", 
            Network="NCRN", 
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ROCR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ROCR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ROCR",],
            Species=InSpecies
  ) 
  
  WOTR<-new("NCRNbirds",
            ParkCode="WOTR",
            ShortName="Wolf Trap",
            LongName="Wolf Trap National Park for the Performing Arts", 
            Network="NCRN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="WOTR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="WOTR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="WOTR",],
            Species=InSpecies
  )
  
  out<-switch(UnitCode,
     "NCRN" = NCRN,
     "ANTI" = ANTI,
     "CATO" = CATO,
     "CHOH" = CHOH,
     "GWMP" = GWMP,
     "GREE" = GREE,
     "HAFE" = HAFE,
     "PISC_FOWA" = PISC_FOWA,
     "MANA" = MANA,
     "MONO" = MONO,
     "NACE" = NACE,
     "PRWI" = PRWI,
     "ROCR" = ROCR,
     "WOTR" = WOTR,
     NCRN)
  
  return(out)
  
}
