#' @include NCRNbirds_Class_def.R
#' 
#' @title importNETNbirds
#' 
#' @importFrom lubridate year
#' @importFrom readr read_csv
#' 
#' @description  This function imports data from the standard NETN .csv files and saves it as \code{NCRNbirds} objects. 
#' The required .csv files are: Points, Visits, FieldData, NETNbands, NETNintervals, BirdSpecies and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 12 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importNETNbirds<-function(Dir){
  
   
  # import  data package and filter NETN records
  
  BirdData<- read_csv(paste(Dir,"MIDN_NCBN_NETN_Landbirds_20240909.csv", sep ="/")) %>% 
    filter(GroupCode == "NETN")
  
 
  # create following df objects from imported data package
  
  # create data frame of unique Events
  
  InVisits<- BirdData %>% filter(ExcludeEvent == 0) %>% # remove QC events
    select(Admin_Unit_Code = UnitCode, Transect_Name = PointGroupName, Point_Name = PointCode, Survey_Type= HabitatType,
                                Year= EventYear, EventDate, StartTime, Visit= VisitNumber, ObserverID ) %>% 
    distinct()
    
  # create data frame of point count data
  
  InFieldData<-BirdData %>% filter(ExcludeEvent == 0) %>% # remove QC events
    select(Admin_Unit_Code = UnitCode, Transect_CODE= GroupCode, Transect_Name = PointGroupName, Point_Name = PointCode, EventDate, Year= EventYear, 
                                   Visit= VisitNumber,Survey_Type= HabitatType, 
                                   AOU_Code= SpeciesCode, Common_Name= CommonName, Scientific_Name = ScientificName, Distance,
                                   Interval = IntervalObserved, Bird_Count = BirdCount, IdentificationMethod, ObserverID ) %>% 
            filter(!IdentificationMethod %in% "Incidental" ) %>% # remove incidentals
            mutate(Bird_Count = as.numeric(Bird_Count)) %>% # force to numeric bc of characters added to vector
            mutate(Distance_id = case_when(Distance == "0-50" ~ 1, # relabel distance ID band
                                           Distance == "> 50" ~ 2))
              

  # create data frame of unique Events
  InPoints<-BirdData %>% select(Admin_Unit_Code = UnitCode, Transect_CODE= PointGroupCode, Transect_Name = PointGroupName, Point_Name = PointCode,  Longitude, Latitude, Survey_Type = HabitatType) %>% 
    distinct()
  
  # directly import csv look up tables
  
  InBands<-read_csv(paste(Dir,"NETNbands.csv", sep="/"))
  
  InIntervals<-read_csv(paste(Dir,"NETNintervals.csv", sep="/"))
  
  InSpecies<-read_csv(paste(Dir,"BirdSpecies.csv", sep="/"))  
  InGuilds<-read_csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"))
  
  ACAD<-new("NCRNbirds", 
            ParkCode="ACAD", 
            ShortName="Acadia", 
            LongName="Acadia National Park", 
            Network="NETN",  
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ACAD",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ACAD",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ACAD",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  ELRO<-new("NCRNbirds", 
            ParkCode="ELRO", 
            ShortName="Eleanor Roosevelt NHS", 
            LongName="Eleanor Roosevelt National Historic Site", 
            Network="NETN", 
            
            VisitNumber=1,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="ELRO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="ELRO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="ELRO",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  HOFR<-new("NCRNbirds", 
            ParkCode="HOFR", 
            ShortName="Home Of Franklin D Roosevelt", 
            LongName="Home Of Franklin D Roosevelt National Historic Site", 
            Network="NETN", 
            
            VisitNumber=1,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="HOFR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="HOFR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="HOFR",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
  MABI<-new("NCRNbirds", 
            ParkCode="MABI", 
            ShortName="Marsh-Billings-Rockefeller", 
            LongName="Marsh-Billings-Rockefeller National Historical Park", 
            Network="NETN", 
            
            VisitNumber=2,
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
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MIMA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MIMA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MIMA",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  MORR<-new("NCRNbirds", 
            ParkCode="MORR", 
            ShortName="Morristown", 
            LongName="Morristown National Historical Park", 
            Network="NETN", 
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            
            Points=InPoints[InPoints$Admin_Unit_Code=="MORR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="MORR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="MORR",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  ROVA<-new("NCRNbirds",
            ParkCode="ROVA",
            ShortName="Roosevelt-Vanderbilt",
            LongName="Roosevelt-Vanderbilt National Historic Sites",
            Network="NETN",

            Points=InPoints[InPoints$Admin_Unit_Code %in% c("ELRO", "HOFR", "VAMA"),] %>% mutate(Admin_Unit_Code = "ROVA") ,
            Visits=InVisits[InVisits$Admin_Unit_Code %in% c("ELRO", "HOFR", "VAMA"),] %>% mutate(Admin_Unit_Code = "ROVA"),
            Birds=InFieldData[InFieldData$Admin_Unit_Code %in% c("ELRO", "HOFR", "VAMA"),] %>% mutate(Admin_Unit_Code = "ROVA"),
            Species=InSpecies
  )

  SAGA<-new("NCRNbirds", 
            ParkCode="SAGA", 
            ShortName="Saint-Gaudens", 
            LongName="Saint-Gaudens National Historic Site", 
            Network="NETN",
            
            VisitNumber=2,
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
            
            VisitNumber=1,
            Bands=InBands,
            Intervals=InIntervals,
          
            Points=InPoints[InPoints$Admin_Unit_Code=="SARA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SARA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SARA",],
            Species=InSpecies,
            Guilds=InGuilds
  ) 
  
  SAIR<-new("NCRNbirds", 
            ParkCode="SAIR", 
            ShortName="Saugus Iron Works", 
            LongName="Saugus Iron Works National Historic Site", 
            Network="NETN",
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="SAIR",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="SAIR",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="SAIR",],
            Species=InSpecies,
            Guilds=InGuilds
  ) 
  
  VAMA<-new("NCRNbirds", 
            ParkCode="VAMA", 
            ShortName="Vanderbilt Mansion", 
            LongName="Vanderbilt Mansion National Historic Site", 
            Network="NETN",
            
            VisitNumber=1,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="VAMA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="VAMA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="VAMA",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  WEFA<-new("NCRNbirds", 
            ParkCode="WEFA", 
            ShortName="Weir Farm", 
            LongName="Weir Farm National Historic Site", 
            Network="NETN",  
            
            VisitNumber=2,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="WEFA",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="WEFA",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="WEFA",],
            Species=InSpecies,
            Guilds=InGuilds
            
  ) 
  
  
  return(c(ACAD,ELRO,HOFR,MABI,MIMA,MORR,ROVA,SAGA,SARA,SAIR,VAMA,WEFA))
}
