#' @include importNETNbirds.R
#' @include NCRNbirds_Class_def.R
#' 
#' @title getAKNData
#'
#' @importFrom dplyr distinct
#' @importFrom magrittr %>% 
#' @importFrom data.table setDT
#' @importFrom stringr str_sub
#' @importFrom lubridate year
#' 
#' @description Takes NETN's AKN point count survey data download and updates Visits and FieldData files for NCRNbirds package import. Also imports in as NCRNbirds object
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' @export


getAKNData<- function(Dir){
  
  #### Import data downloaded from AKN (http://data.prbo.org/science/biologists/index.php)
  
  PointCounts<-read.csv(paste(Dir,"AKN/AKNPointCountObs.csv", sep="/"),as.is=T, header = T) %>% 
    mutate(AOU_Code = Spp)
  
  SiteCond <- read.csv(paste(Dir,"AKN/AKNSiteConditions.csv", sep="/"),as.is=T, header = T)
  
  NETNintervals<-read.csv(paste(Dir,"NETNintervals.csv", sep="/"),as.is=T, header = T)
  
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
    dplyr::select(Transect, Point, Date, Start.Time, End.Time, Researcher) %>%
    dplyr::mutate(Date= mdy(Date)) %>% 
    dplyr::mutate(Year= year(Date))%>% 
    dplyr::distinct(.) 
  
  visits<-setDT(visits)   ## change format
  
  # assign visit number in each year based on number of events per year per point.
  #(the line below doesn't work any longer so replaced with line 60, which seems to be working)
  #visits<-visits[, Visit:= seq.int(from = 1, along = list(Transect,Point, Date), by = 1), by= c("Point","Year")] 
  
  visits<-visits[ , Visit := seq(.N), by = c("Point","Year")]
  
  # rename columnsto align with R package
  visits_clean<-visits %>% 
    dplyr::mutate(Park= stringr::str_sub(Point, 1, 4)) %>%
    
    dplyr::select(Admin_Unit_Code= Park, Transect_Name= Transect, Point_Name = Point, Year,EventDate= Date, StartTime= Start.Time, EndTime= End.Time, Visit, Observer=Researcher)
  
  
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
  CommMatch<-anti_join(NETN_AOU,Species_AOU, by="AOU_Code")
  print(CommMatch)
  
  ##ADD HERE TO RETURN THE NO. OF SPECIES NOT MATCHED AND THIER AOUs
  #View(CommMatch)  
  
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
    dplyr::mutate(Date= as.Date(Date, "%m/%d/%Y")) %>% # force vector to Date type
    dplyr::left_join(.,visits[,c("Point", "Date", "Visit")],by= c("Point", "Date") )%>% # add visit no. to data
    # add in field to distinguish between obs made within first 3 min of the count
    dplyr::mutate (Initial_Three_Min_Cnt= ifelse(Time.Bin.ID %in% c(7,8,9), 1, 0)) %>% # check with unique(data[c("Time.Bin.ID","Initial_Three_Min_Cnt")])
    dplyr::mutate(Detection.Cue=recode(Detection.Cue, .missing = "NR")) %>% # change few detections with missing codes to "NR"
    dplyr::left_join(.,NETNID_methods,by= "Detection.Cue" ) %>%  # standardize cues from look up table
    dplyr::left_join(.,DistBands,by= "Distance.Bin.ID" ) %>% # add in distance band info to denote visual vs auditory detection
    dplyr::mutate(Flyover_Observed= ifelse(Distance.Bin.ID == "FLY",1,0)) %>% # create new field to denote flyover
    dplyr::rename(Point_Name = Point)%>% # renaming to suport join in next step
    dplyr::left_join(.,sites[,c("Point_Name", "Survey_Type")],by= "Point_Name" ) %>% ## add survey type (forest vs grassland)
    dplyr::left_join(.,tlu_Obs, by= "Researcher") %>% # add in observer skill
    dplyr::mutate(Park= stringr::str_sub(Point_Name, 1, 4)) %>% ## Add ParkCode to data
    dplyr::select(Admin_Unit_Code= Park, Transect_Name= Transect,Point_Name, EventDate= Date, Visit,AOU_Code, Bird_Count= Count,
                  Scientific_Name= Scientific.Name, Common_Name= Common.Name, Interval= Time.Bin.ID, Interval_Length, ID_Method_Code, ID_Method, Distance_id, Distance =Label, 
                  Flyover_Observed, Initial_Three_Min_Cnt, Survey_Type, Data.Status, Point.Note, Observer= pwrc_ObsID, Skill_Level, Skill_Notes)  # rename and select cols
  
  
#### Check species name matching after inital match ---- 
  NETN_AOU<-data %>% select(AOU_Code, Common_Name,Scientific_Name) %>%
    dplyr:: distinct(.)
  
  CommMatch<-anti_join(NETN_AOU,Species_AOU, by="AOU_Code")
  print(CommMatch)
  
  ## Check and print detection id codes that aren't flyovers ----
  data_detections<-data %>%
    filter(Flyover_Observed %in% 0) %>% # remove flyovers
    select(ID_Method_Code, ID_Method) %>% 
    group_by(ID_Method_Code) %>% tally(.)
  
  print(data_detections) 
   
 # print which park these obs are from
  NA_Det<-data %>% 
   filter(Flyover_Observed %in% 0 & is.na(ID_Method_Code))
  print(unique(NA_Det[,c("Admin_Unit_Code","Point_Name")]))
 
### write data to directory ----
  write.table(data, paste(Dir,"FieldData.csv", sep="/"), sep= ",", row.names = FALSE)
  
  NETN<-importNETNbirds(Dir)
  
  
}
