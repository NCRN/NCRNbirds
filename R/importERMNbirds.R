#' @include NCRNbirds_Class_def.R
#' 
#' @title importERMNbirds
#' 
#' @importFrom lubridate year
#' 
#' @description  This function imports data from the standard NCRN.csv files and saves it as \code{NCRNbirds} objects. The required .csv files are: Points, Visits, FieldData and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data are found
#' 
#' @return Returns a list of 6 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importERMNbirds<-function(Dir){
  OldDir<-getwd()
  setwd(Dir)  
  
  InPoints<-read.csv("Points.csv",as.is=T, header=T)
  
  InVisits<-read.csv("Visits.csv",as.is=T, header=T)
  InVisits$EventDate<-mdy(InVisits$EventDate)
  InVisits$Year<-year(InVisits$EventDate)
  InVisits$Visit<-as.character(InVisits$Visit)
  
  
  InFieldData<-read.csv("FieldData.csv",as.is=T, header=T)
  InFieldData$EventDate<-mdy(InFieldData$EventDate)
  InFieldData$Year<-year(InFieldData$EventDate)
  
  
  InSpecies<-read.csv("BirdGuildAssignments.csv", as.is=T, header=T )
  
  setwd(OldDir)
  
  
  DEWA<-new("NCRNbirds", 
            ParkCode="DEWA", 
            ShortName="Delaware Water Gap NRA", 
            LongName="Delaware Water Gap National Recreation Area", 
            Network="ERMN",  
            
            Points=InPoints[InPoints$Admin_Unit_Code=="DEWA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="DEWA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="DEWA",],
            Species=InSpecies
  )
  
  
  ALPO<-new("NCRNbirds", 
            ParkCode="ALPO", 
            ShortName="Allagheny Portage Railroad NHS", 
            LongName="Allagheny Portage Railroad National Historic Site", 
            Network="ERMN", 
            
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
            
            Points=InPoints[InPoints$Admin_Unit_Code=="FRHI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="FRHI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="FRHI",],
            Species=InSpecies
  )
  
  NERI<-new("NCRNbirds", 
            ParkCode="NERI", 
            ShortName="New River Gorge NR", 
            LongName="New River Gorge National River", 
            Network="ERMN", 
            
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
            
            Points=InPoints[InPoints$Admin_Unit_Code=="BLUE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="BLUE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="BLUE",],
            Species=InSpecies
  )

  
  return(c(DEWA,ALPO,FONE,FRHI,NERI,BLUE))
}