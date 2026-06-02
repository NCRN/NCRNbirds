#' @include NCRNbirds_Class_def.R
#' 
#' @title importMIDNbirds
#' 
#' @importFrom lubridate year mdy
#' @importFrom readr read_csv
#' 
#' @description  This function imports data from the standard MIDN .csv files and saves it as \code{NCRNbirds} objects. 
#' The required .csv files are: Points, Visits, FieldData MIDNbands, MIDNintervals, BirdSpecies, and BirdGuildAssignments.
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @return Returns a list of 8 \code{NCRNbirds} objects, one for each park.
#' 
#' @export


importMIDNbirds<-function(Dir){
  
   
  # import data package CSV; Import ignores date in file name; filter to keep MIDN records and remove Excluded and Incidental records
 
  BirdData <- readr::read_csv(
    list.files(Dir, pattern = "^MIDN_NCBN_NETN_Landbirds.*\\.csv$", full.names = TRUE) %>%
      { .[which.max(file.info(.)$mtime)] }
  ) %>%
    dplyr::filter(
      GroupCode == "MIDN",
      ExcludeEvent == 0,       #excludes NAs as well
      ExcludeObservation == 0, #excludes NAs as well   
      Incidental == 0          #excludes NAs as well
    )
  
  InVisits <- BirdData %>%
    select(
      Admin_Unit_Code = UnitCode,
      SubUnitName,
      Point_Name = PointCode,
      Survey_Type = HabitatType,
      Event_ID = EventID,
      Year = EventYear,
      EventDate,
      StartTime,
      Visit = VisitNumber,
      Observer = ObserverID
    ) %>%
    distinct(Event_ID, .keep_all = TRUE) %>%
    mutate(
      EventDate = as.Date(EventDate),   
      Year      = year(EventDate)
    )
  
    
  # create data frame of Point Count data
  
  InFieldData<-BirdData %>% 
    select(
      Admin_Unit_Code = UnitCode,
      SubUnitName,
      Point_Name = PointCode,
      EventDate,
      Year= EventYear,
      Visit= VisitNumber,
      Survey_Type= HabitatType,
      ObservationID,
      AOU_Code= SpeciesCode,
      Common_Name= CommonName,
      Scientific_Name = ScientificName,
      Distance,
      Interval = IntervalObserved,
      FirstThreeMinutes,
      FirstFiveMinutes,
      Bird_Count = BirdCount,
      IdentificationMethod,
      Observer = ObserverID
    ) %>% 
    distinct() %>%  
      mutate(Bird_Count = as.numeric(Bird_Count)) %>% # force to numeric bc of characters added to vector
      mutate(Distance_id = case_when(Distance == "0-50" ~ 1, # relabel distance ID band
                                         Distance == "> 50" ~ 2)) %>% 
          mutate(EventDate = as.Date(EventDate), Year = year(EventDate) 
     )
      

  # create data frame of unique Points
  
  InPoints<-BirdData %>% select(Admin_Unit_Code = UnitCode, UnitName, SubUnitName, Point_Name = PointCode, IsActive, Longitude, Latitude, Datum, XYAccuracy) %>% 
    
    distinct(Point_Name, .keep_all = TRUE)
  
  # directly import csv look up tables
  
  
  
  InBands<-read_csv(paste(Dir,"MIDNbands.csv", sep="/"))
  InIntervals<-read_csv(paste(Dir,"MIDNintervals.csv", sep="/"))
  InSpecies<-read_csv(paste(Dir,"BirdSpecies.csv", sep="/"))  
  InGuilds<-read_csv(paste(Dir,"BirdGuildAssignments.csv", sep="/"))
  
  
  
  APCO<-new("NCRNbirds", 
            ParkCode="APCO", 
            ShortName="Appomattox", 
            LongName="Appomattox Court House National Historical Park", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="APCO",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="APCO",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="APCO",],
            Species=InSpecies,
            Guilds=InGuilds
  )
  
  
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
  
  
  HOFU<-new("NCRNbirds",
            ParkCode="HOFU",
            ShortName="Hopewell",
            LongName="Hopewell Furnace National Historic Site", 
            Network="MIDN", 
            
            VisitNumber=4,
            Bands=InBands,
            Intervals=InIntervals,
            
            Points=InPoints[InPoints$Admin_Unit_Code=="HOFU",], 
            Visits=InVisits[InVisits$Admin_Unit_Code=="HOFU",],
            Birds=InFieldData[InFieldData$Admin_Unit_Code=="HOFU",],
            Species=InSpecies,
            Guilds=InGuilds
  ) 
  
  PETE<-new("NCRNbirds", 
            ParkCode="PETE", 
            ShortName="Petersburg", 
            LongName="Petersburg National Battlefield", 
            Network="MIDN", 
            
            VisitNumber=4 ,
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
  
  
  return(c(APCO,BOWA,FRSP,GETT,HOFU,PETE,RICH,VAFO))
  
}
