#' @title importNCRNbirds
#' 
#' @description  This function imports data from the standard NCRN .csv files and saves it as \code{NCRNbirds} objects. The required .csv files are: Points, Visits and FieldData.
#' 
#' @param Dir  The directory where the data is found
#' 
#' @return Returns a list of 11 \code{NCRNbirds} objects, one for each park, named using the standard 4 letter park code (e.g. ANTI, CATO etc.).
#' 
#' @export
#' 


importNCRNbirds<-function(Dir){
  OldDir<-getwd()
  setwd(Dir)  
  
  InPoints<-read.csv("Points.csv",as.is=T, header=T)

#   InPlots$Event_Earliest<-as.Date(as.character(InPlots$Event_Earliest), format="%Y%m%d")
#   InPlots$Event_Latest<-as.Date(as.character(InPlots$Event_Latest),format="%Y%m%d")
 
  InVisits<-read.csv("Visits.csv",as.is=T, header=T)
#  InEvents$Event_Date<-as.Date(as.character(InEvents$Event_Date_Txt), format="%Y%m%d")
  
  
  InFieldData<-read.csv("FieldData.csv",as.is=T, header=T)

  
  ANTI<-new("NCRNbirds", 
            ParkCode="ANTI", 
            ShortName="Antietam", 
            LongName="Antietam National Battlefield", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ANTI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ANTI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ANTI",]
  )
  
  
  CATO<-new("NCRNbirds", 
            ParkCode="CATO", 
            ShortName="Catoctin", 
            LongName="Catoctin Mountain Park", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="CATO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="CATO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="CATO",]
  )
  
  CHOH<-new("NCRNbirds", 
            ParkCode="CHOH", 
            ShortName="C&O Canal", 
            LongName="Chesapeake & Ohio Canal National Historical Park", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="CHOH",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="CHOH",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="CHOH",]
  )
  
  GWMP<-new("NCRNbirds", 
            ParkCode="GWMP", 
            ShortName="GW Parkway", 
            LongName="George Washington Memorial Parkway", 
            Network="NCRN", 
             
            Points=InPoints[InPoints$Admin_Unit_Code=="GWMP",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="GWMP",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="GWMP",]
  )
  
  HAFE<-new("NCRNbirds", 
            ParkCode="HAFE", 
            ShortName="Harpers Ferry", 
            LongName="Harpers Ferry National Histroical Park", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="HAFE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="HAFE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="HAFE",]
  )
  
  MANA<-new("NCRNbirds",
            ParkCode="MANA",
            ShortName="Manassas",
            LongName="Manassas National Battlefield Park", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MANA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MANA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MANA",]
  ) 
  
  MONO<-new("NCRNbirds",
            ParkCode="MONO",
            ShortName="Monocacy",
            LongName="Monocacy National Battlefield", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MONO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MONO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MONO",]
  ) 
  
  NACE<-new("NCRNbirds",
            ParkCode="NACE",
            ShortName="Nat.Cap.Parks - East",
            LongName="National Captial Parks-East", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="NACE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="NACE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="NACE",]
  ) 
  
  PRWI<-new("NCRNbirds",
            ParkCode="PRWI",
            ShortName="Prince William",
            LongName="Prince William Forest Park", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="PRWI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="PRWI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="PRWI",]
  ) 
  
  ROCR<-new("NCRNbirds",
            ParkCode="ROCR",
            ShortName="Rock Creek",
            LongName="Rock Creek Park", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ROCR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ROCR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ROCR",]
  ) 
  
  WOTR<-new("NCRNbirds",
            ParkCode="WOTR",
            ShortName="Wolf Trap",
            LongName="Wolf Trap National Park for the Performing Arts", 
            Network="NCRN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="WOTR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="WOTR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="WOTR",]
  )
  
  
  return(c(ANTI,CATO,CHOH,GWMP,HAFE,MANA,MONO,NACE,PRWI,ROCR,WOTR))
}
