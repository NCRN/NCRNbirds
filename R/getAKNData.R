#' @include importNETNbirds.R
#' @include NCRNbirds_Class_def.R
#' 
#' @title getAKNData
#'
#' @importFrom dplyr distinct anti_join tally recode slice
#' @importFrom magrittr %>% 
#' @importFrom data.table setDT
#' @importFrom stringr str_sub str_detect
#' @importFrom lubridate year 
#' 
#' @description Takes NETN's AKN point count survey data download and updates Visits and FieldData files for NCRNbirds package import. Also has option to import into workspace as NCRNbirds object ("NETN").
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' @param import Logical. If \code{TRUE} will import data as "NETN" NCRNbirds object.
#' @details  The following files will be saved in the folder denoted in the in argument \code{Dir}: FieldData.csv: Raw bird detection data by location and date; Visits.csv: List of unique surveys by date for each point count station.
#' Please see associated Data Defintions for field names and descriptions. During the conversion from AKN's data format into a format that is readable by \code{importNETNbirds}, \code{getAKNData} filters out detections of mammals and unidentified species, checks AOU Codes against the most recent list 
#' supplied by the look-up table BirdSpecies.csv, and renames and recodes a variety of data columns (e.g., detection type, distance) for standardization within the the package. Detections are aggregated into two distance bands (less than or greater than 50m from the observer), or are not recorded or considered as flyovers. 
#' @export


getAKNData<- function(Dir, import= FALSE){
  
  #### Import data downloaded from AKN (http://data.prbo.org/science/biologists/index.php)
  
  PointCounts<-read.csv(paste(Dir,"AKN/AKNPointCountObs.csv", sep="/"),as.is=T, header = T) %>% # Entire point count data downloaded from AKN annually
    slice(-1) %>% ## to remove AKN's blank row on line 2 (excl. header)
    rename(Point_Name= Point) %>% 
    mutate(AOU_Code = Spp)

  SiteCond <- read.csv(paste(Dir,"AKN/AKNSiteConditions.csv", sep="/"),as.is=T, header = T) %>%  # Entire site condition data downloaded from AKN annually
    slice(-1) ## to remove AKN's blank row on line 2 (excl. header)
    
  NETNintervals<-read.csv(paste(Dir,"NETNintervals.csv", sep="/"),as.is=T, header = T) %>%  # look up file 
    mutate(Time.Bin.ID = as.character(Time.Bin.ID ))
  
  NETNID_methods<-read.csv(paste(Dir,"tlu_ID_Methods.csv", sep="/"),as.is=T, header = T)
  
  DistBands<-read.csv(paste(Dir,"NETNbands.csv", sep="/"),as.is=T, header = T)
  
  sites<-read.csv(paste(Dir,"Points.csv", sep="/"),as.is=T, header = T)
  
  tlu_Obs<-read.csv(paste(Dir,"tbl_NETN_Observers.csv", sep="/"),as.is=T, header = T)
  
  Species_tlu<-read.csv(paste(Dir,"BirdSpecies.csv", sep="/"),as.is=T, header = T)
  
  ####### Generate Visits file----
  #### CAUTION: note that the data downloaded from AKN contains many errors in regards to the "Visit" field.
  # In many cases the actual number under visit is not correct (1 for repeat visits and a 14!). QAQC visit ==9. 
  # Therefore, instead of using that field I am going to derive it below from the actual data to determine first and repeat visits within a year.
  
  
  # First, create a df of the unique visits for each point in each year to determine visit per event
  visits<-PointCounts %>% 
    filter(!Visit %in% c(9)) %>% # remove QAQC visits
    dplyr::select(Transect, Point_Name, Date, Start.Time, End.Time, Researcher) %>%
    dplyr::mutate(Date= ymd(Date)) %>% 
    dplyr::mutate(Year= year(Date)) %>% 
    dplyr::distinct(.) 
  
  visits<-setDT(visits)   ## change format
  
  # assign visit number in each year based on number of events per year per point.
  #(the line below doesn't work any longer so replaced with line 60, which seems to be working)
  #visits<-visits[, Visit:= seq.int(from = 1, along = list(Transect,Point, Date), by = 1), by= c("Point","Year")] 
  
  visits<-visits[ , Visit := seq(.N), by = c("Point_Name","Year")]
  
  # rename columnsto align with R package
  visits_clean<-visits %>% 
    dplyr::mutate(Park= stringr::str_sub(Point_Name, 1, 4)) %>%
    dplyr::left_join(.,sites[,c("Point_Name", "Survey_Type")],by= "Point_Name" ) %>% ## add survey type (forest vs grassland)
    dplyr::select(Admin_Unit_Code= Park, Transect_Name= Transect, Point_Name, Survey_Type,Year,EventDate= Date, StartTime= Start.Time, EndTime= End.Time, Visit, Observer=Researcher)
  
  
  write.table(visits_clean, paste(Dir,"Visits.csv", sep="/"), sep= ",", row.names = FALSE)
  
  ##### Check to make sure that Species names in AKN match  current taxonomy----
  ## NETN AOU codes
  NETN_AOU<-PointCounts %>% select(AOU_Code, Common_Name=Common.Name,Scientific_Name=Scientific.Name) %>%
    dplyr:: filter(!AOU_Code %in% c("CHIP","RESQ","EGSQ","GRSQ", "UNSQ", "---","NAPO")) %>% 
    dplyr:: filter(!str_detect(AOU_Code, "^UN") & !AOU_Code %in% "UAHA") %>% #remove Unidentified species
    dplyr:: filter(!str_detect(AOU_Code, "^UN")) %>% # remove unidentified species
    dplyr:: select(AOU_Code,Common_Name,Scientific_Name) %>% 
    dplyr:: distinct(.)
  
  # Bird Species tax
  
  Species_AOU<-Species_tlu %>% select(AOU_Code, Common_Name, Scientific_Name)
  
  # Which species codes are not found in the updated taxonomy table?
  
  ## Which species in ERMN's taxonomy table don't match NETN AKN and guild data
  SppMatch<-anti_join(NETN_AOU,Species_AOU, by="AOU_Code")
  cat("The following AOU codes in the AKN download  don't match the current AOU Codes in the NCRNBirds package but have been updated in the data.")
  
  print(SppMatch)
  
  ##ADD HERE TO RETURN THE NO. OF SPECIES NOT MATCHED AND THIER AOUs
  #View(SppMatch)  
  
  # now rename species codes to match taxonomy
  # changes to AOU COdes det on March 28 2019 
  # By creating the vector AOU_Code above during import of PointCounts you can see the changes from Spp to AOU_Code
  PointCounts<-PointCounts %>% 
    mutate(AOU_Code= recode(AOU_Code, "MYWA"= "YRWA","SCJU"="DEJU","YSFL"="NOFL","SOVI"= "BHVI","CAGO"="CANG"))
  
  ####### Generate FieldData file----
  
  data<- PointCounts %>%  
    dplyr::filter(!Spp %in% c("CHIP","RESQ","EGSQ","GRSQ", "UNSQ", "---","NAPO")) %>% # remove mammals
    dplyr::filter(!str_detect(AOU_Code, "^UN")& !AOU_Code %in% "UAHA") %>% #remove Unidentified species
    dplyr::filter(!Data.Status %in% "RESTRICTED") %>% # remove data that has been flagged as not suitable for analysis
    dplyr::filter(!Visit %in% c(9)) %>% # remove QAQC visits
    select(-Visit) %>%  # now drop AKN's bad visit column to bind in derived visit nums (visits) from above
    dplyr::left_join(.,NETNintervals[1:2],by= "Time.Bin.ID") %>% # add in stadardized codes for detection time interval
    dplyr::mutate(Date= ymd(Date)) %>% # force vector to Date type
    dplyr::left_join(.,visits[,c("Point_Name", "Date", "Visit")],by= c("Point_Name", "Date") )%>% # add visit no. to data
    # add in field to distinguish between obs made within first 3 min of the count
    dplyr::mutate (Initial_Three_Min_Cnt= ifelse(Time.Bin.ID %in% c(7,8,9), 1, 0)) %>% # check with unique(data[c("Time.Bin.ID","Initial_Three_Min_Cnt")])
    dplyr::mutate(Detection.Cue=recode(Detection.Cue, .missing = "NR")) %>% # change few detections with missing codes to "NR"
    dplyr::left_join(.,NETNID_methods,by= "Detection.Cue" ) %>%  # standardize cues from look up table
    dplyr::mutate(Distance.Bin.ID= recode(Distance.Bin.ID, "L10" = "L50", "L25"= "L50", "L50" = "L50")) %>% 
    dplyr::left_join(.,DistBands,by= "Distance.Bin.ID" ) %>% # add in distance band info to denote visual vs auditory detection
    dplyr::mutate(Flyover_Observed= ifelse(Distance.Bin.ID == "FLY",1,0)) %>% # create new field to denote flyover
    dplyr::left_join(.,sites[,c("Point_Name", "Survey_Type")],by= "Point_Name" ) %>% ## add survey type (forest vs grassland)
    dplyr::left_join(.,tlu_Obs, by= "Researcher") %>% # add in observer skill
    dplyr::mutate(Park= stringr::str_sub(Point_Name, 1, 4)) %>% ## Add ParkCode to data
    dplyr::select(Admin_Unit_Code= Park, Transect_Name= Transect,Point_Name, EventDate= Date, Visit,AOU_Code, Bird_Count= Count,
                  Scientific_Name= Scientific.Name, Common_Name= Common.Name, Interval= Time.Bin.ID, Interval_Length, ID_Method_Code, ID_Method, Distance_id, Distance =Label, 
                  Flyover_Observed, Initial_Three_Min_Cnt, Survey_Type, Data.Status, Point.Note, Observer= pwrc_ObsID, Skill_Level, Skill_Notes)  # rename and select cols
  
  
#### Check species name matching after inital match ---- 
  NETN_AOU<-data %>% select(AOU_Code, Common_Name,Scientific_Name) %>%
    dplyr:: distinct(.)
  
  SppMatch1<-anti_join(NETN_AOU,Species_AOU, by="AOU_Code")
  
  cat("Any remaining AOU Codes that don't match the updated taxonomy table?")
  
  print(table(SppMatch1))
  
  ## Check and print detection id codes that aren't flyovers ----
  data_detections<-data %>%
    filter(Flyover_Observed %in% 0) %>% # remove flyovers
    select(ID_Method_Code, ID_Method) %>% 
    group_by(ID_Method_Code) %>% tally(.)
  
  cat("Tally of detection codes that are not flyovers:")
  
  print(table(data_detections))
   
 # print which park these obs NA's are from
  NA_Det<-data %>% 
   filter(Flyover_Observed %in% 0 & is.na(ID_Method_Code))
  cat("Park where missing detection codes were recorded from:")
  
  print(table(unique(NA_Det[,c("Admin_Unit_Code","Point_Name")])))
 
### write data to directory and import as NCRNbirds object----
  write.table(data, paste(Dir,"FieldData.csv", sep="/"), sep= ",", row.names = FALSE)
  
 if(import == TRUE){
   NETN<-importNETNbirds(Dir)}
  
  
}
