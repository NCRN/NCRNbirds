#' @include NCRNbirds_Class_def.R
#' 
#' @title importMIDNbirds
#' 
#' @importFrom lubridate year
#' 
#' @description  This function imports data from the standard MIDN .csv files and saves it as \code{NCRNbirds} objects. The required .csv files are: Points, Visits, FieldData and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 11 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importMIDNbirds<-function(Dir){
  
  InPoints<-read.csv(paste(Dir,"Points.csv", sep="/"),as.is=T, header=T)
  
  InVisits<-read.csv(paste(Dir,"Visits.csv",sep="/"),as.is=T, header=T)
  InVisits$EventDate<-as.Date(as.character(InVisits$EventDate), format="%m/%d/%Y")
  InVisits$Year<-year(InVisits$EventDate)
  
  
  InFieldData<-read.csv(paste(Dir,"FieldData.csv", sep="/"),as.is=T, header=T)
  InFieldData$EventDate<-as.Date(as.character(InFieldData$EventDate), format="%m/%d/%Y")
  InFieldData$Year<-year(InFieldData$EventDate)
  
  InSpecies<-read.csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"), as.is=T, header=T )
  
  APCO<-new("NCRNbirds", 
            ParkCode="APCO", 
            ShortName="Appomattox", 
            LongName="Appomattox Court House National Historical Park", 
            Network="MIDN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="APCO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="APCO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="APCO",],
            Species=InSpecies
            )
  
  
  BOWA<-new("NCRNbirds", 
            ParkCode="BOWA", 
            ShortName="Booker T.", 
            LongName="Booker T. Washington National Monument", 
            Network="MIDN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="BOWA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="BOWA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="BOWA",],
            Species=InSpecies
  )
  
  FRSP<-new("NCRNbirds", 
            ParkCode="FRSP", 
            ShortName="Fredericksburg", 
            LongName="Fredericksburg and Spotsylvania National Military Park", 
            Network="MIDN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="FRSP",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="FRSP",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="FRSP",],
            Species=InSpecies
  )
  
  PETE<-new("NCRNbirds", 
            ParkCode="PETE", 
            ShortName="Petersburg", 
            LongName="Petersburg National Battlefield", 
            Network="MIDN", 
             
            Points=InPoints[InPoints$Admin_Unit_Code=="PETE",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="PETE",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="PETE",],
            Species=InSpecies
  )
  
  RICH<-new("NCRNbirds", 
            ParkCode="RICH", 
            ShortName="Richmond", 
            LongName="Richmond National Battlefield Park", 
            Network="MIDN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="RICH",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="RICH",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="RICH",],
            Species=InSpecies
  )
  
  VAFO<-new("NCRNbirds",
            ParkCode="VAFO",
            ShortName="Valley Forge",
            LongName="Valley Forge National Historical Park", 
            Network="MIDN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="VAFO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="VAFO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="VAFO",],
            Species=InSpecies
  ) 
  

  return(c(APCO,BOWA,FRSP,PETE,RICH,VAFO))
}
