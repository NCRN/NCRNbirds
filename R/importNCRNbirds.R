#' @include NCRNbirds_Class_def.R
#' 
#' @title importNCRNbirds
#' 
#' @importFrom lubridate year
#' 
#' @description  This function imports data from the standard NCRN .csv files and saves it as \code{NCRNbirds} objects. The required .csv files are: Points, Visits, FieldData and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found
#' 
#' @return Returns a list of 11 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importNCRNbirds<-function(Dir){
  OldDir<-getwd()
  setwd(Dir)  
  
  InPoints<-read.csv("Points.csv",as.is=T, header=T)
 
  InVisits<-read.csv("Visits.csv",as.is=T, header=T)
  InVisits$EventDate<-as.Date(as.character(InVisits$EventDate), format="%m/%d/%Y")
  InVisits$Year<-year(InVisits$EventDate)
  
  
  InFieldData<-read.csv("FieldData.csv",as.is=T, header=T)
  InFieldData$EventDate<-as.Date(as.character(InFieldData$EventDate), format="%m/%d/%Y")
  InFieldData$Year<-year(InFieldData$EventDate)

  
  InSpecies<-read.csv("BirdGuildAssignments.csv", as.is=T, header=T )
  
  setwd(OldDir)

  
  ANTI<-new("NCRNbirds", 
            ParkCode="ANTI", 
            ShortName="Antietam", 
            LongName="Antietam National Battlefield", 
            Network="NCRN", 
            
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
             
            Points=InPoints[InPoints$Admin_Unit_Code=="GWMP",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="GWMP",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="GWMP",],
            Species=InSpecies
  )
  
  HAFE<-new("NCRNbirds", 
            ParkCode="HAFE", 
            ShortName="Harpers Ferry", 
            LongName="Harpers Ferry National Histroical Park", 
            Network="NCRN", 
            
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
            
            Points=InPoints[InPoints$Admin_Unit_Code=="NACE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="NACE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="NACE",],
            Species=InSpecies
  ) 
  
  PRWI<-new("NCRNbirds",
            ParkCode="PRWI",
            ShortName="Prince William",
            LongName="Prince William Forest Park", 
            Network="NCRN", 
            
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
            
            Points=InPoints[InPoints$Admin_Unit_Code=="WOTR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="WOTR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="WOTR",],
            Species=InSpecies
  )
  
  
  return(c(ANTI,CATO,CHOH,GWMP,HAFE,MANA,MONO,NACE,PRWI,ROCR,WOTR))
}
