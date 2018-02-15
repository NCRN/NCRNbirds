#' @include NCRNbirds_Class_def.R
#' 
#' @title importNETNbirds
#' 
#' @importFrom lubridate year
#' 
#' @description  This function imports data from the standard NCRN .csv files and saves it as \code{NCRNbirds} objects. The required .csv files are: Points, Visits, FieldData and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found
#' 
#' @return Returns a list of 12 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importNETNbirds<-function(Dir){
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
  
  
  ACAD<-new("NCRNbirds", 
            ParkCode="ACAD", 
            ShortName="Acadia", 
            LongName="Acadia National Park", 
            Network="NETN",  
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ACAD",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ACAD",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ACAD",],
            Species=InSpecies
  )
  
  
  ELRO<-new("NCRNbirds", 
            ParkCode="ELRO", 
            ShortName="Eleanor Roosevelt NHS", 
            LongName="Eleanor Roosevelt National Historic Site", 
            Network="NETN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ELRO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ELRO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ELRO",],
            Species=InSpecies
  )
  
  
  HOFR<-new("NCRNbirds", 
            ParkCode="HOFR", 
            ShortName="Home Of Franklin D Roosevelt", 
            LongName="Home Of Franklin D Roosevelt National Historic Site", 
            Network="NETN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="HOFR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="HOFR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="HOFR",],
            Species=InSpecies
  )
  
  
  MABI<-new("NCRNbirds", 
            ParkCode="MABI", 
            ShortName="Marsh-Billings-Rockefeller", 
            LongName="Marsh-Billings-Rockefeller National Historical Park", 
            Network="NETN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MABI",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MABI",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MABI",],
            Species=InSpecies
  )
  
  MIMA<-new("NCRNbirds", 
            ParkCode="MIMA", 
            ShortName="Minute Man", 
            LongName="Minute Man National Historical Park", 
            Network="NETN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MIMA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MIMA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MIMA",],
            Species=InSpecies
  )
  
  MORR<-new("NCRNbirds", 
            ParkCode="MORR", 
            ShortName="Morristown", 
            LongName="Morristown National Historical Park", 
            Network="NETN", 
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MORR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MORR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MORR",],
            Species=InSpecies
  )
  
  # ROVA<-new("NCRNbirds",
  #           ParkCode="ROVA",
  #           ShortName="Roosevelt-Vanderbilt",
  #           LongName="Roosevelt-Vanderbilt National Historic Sites",
  #           Network="NETN",
  # 
  #           Points=InPoints[InPoints$Admin_Unit_Code=="ROVA",],
  #           Visits=InVisits[InVisits$Admin_Unit_Code=="ROVA",],
  #           Birds=InFieldData[InFieldData$Admin_Unit_Code=="ROVA",],
  #           Species=InSpecies
  # )
  # 
  SAGA<-new("NCRNbirds", 
            ParkCode="SAGA", 
            ShortName="Saint-Gaudens", 
            LongName="Saint-Gaudens National Historic Site", 
            Network="NETN",
            
            Points=InPoints[InPoints$Admin_Unit_Code=="SAGA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SAGA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SAGA",],
            Species=InSpecies
  ) 
  
  SARA<-new("NCRNbirds", 
            ParkCode="SARA", 
            ShortName="Saratoga", 
            LongName="Saratoga National Historical Park", 
            Network="NETN",
            
            Points=InPoints[InPoints$Admin_Unit_Code=="SARA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SARA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SARA",],
            Species=InSpecies
  ) 
  
  SAIR<-new("NCRNbirds", 
            ParkCode="SAIR", 
            ShortName="Saugus Iron Works", 
            LongName="Saugus Iron Works National Historic Site", 
            Network="NETN",
            
            Points=InPoints[InPoints$Admin_Unit_Code=="SAIR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SAIR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SAIR",],
            Species=InSpecies
  ) 
  
  VAMA<-new("NCRNbirds", 
            ParkCode="VAMA", 
            ShortName="Vanderbilt Mansion", 
            LongName="Vanderbilt Mansion National Historic Site", 
            Network="NETN",
            
            Points=InPoints[InPoints$Admin_Unit_Code=="VAMA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="VAMA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="VAMA",],
            Species=InSpecies
  )
  
  WEFA<-new("NCRNbirds", 
            ParkCode="WEFA", 
            ShortName="Weir Farm", 
            LongName="Weir Farm National Historic Site", 
            Network="NETN",  
            
            Points=InPoints[InPoints$Admin_Unit_Code=="WEFA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="WEFA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="WEFA",],
            Species=InSpecies
  ) 
  
  
  return(c(ACAD,ELRO,HOFR,MABI,MIMA,MORR,SAGA,SARA,SAIR,VAMA,WEFA))
}