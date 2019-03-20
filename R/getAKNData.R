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
#' @description Takes NETN's AKN data download and updates Visits and FieldData files for NCRNbirds package import. Also imports in as NCRNbirds object
#' 
#' @param Dir  The directory where the data is found. You should omit the trailing slash ("/") in the directory name.
#' 
#' #' @export


getAKNData<- function(Dir){

#### Import data downloaded from AKN (http://data.prbo.org/science/biologists/index.php)

PointCounts<-read.csv(paste(Dir,"AKNPointCountObs.csv", sep="/"),as.is=T, header = T)

SiteCond <- read.csv(paste(Dir,"AKNSiteConditions", sep="/"),as.is=T, header = T)

NETNintervals<-read.csv(paste(Dir,"NETNintervals.csv", sep="/"),as.is=T, header = T)

NETNID_methods<-read.csv(paste(Dir,"tlu_ID_Methods.csv", sep="/"),as.is=T, header = T)

DistBands<-read.csv(paste(Dir,"NETNbands.csv", sep="/"),as.is=T, header = T)

sites<-read.csv(paste(Dir,"Points.csv", sep="/"),as.is=T, header = T)

####### Generate Visits file----
#### CAUTION: note that the data downloaded from AKN contains many errors in regards to the "Visit" field.
# In many cases the actual number under visit is not correct (1 for repeat visits and a 14!). QAQC visit ==9. 
# Therefore, instead of using that field I am going to derive it below from the actual data to determine first and repeat visits within a year.


# First, create a df of the unique visits for each point in each year to determine visit per event
visits<-PointCounts %>% 
  filter(!Visit %in% c(9)) %>% # remove QAQC visits
  dplyr::select(Transect, Point, Date, Start.Time, End.Time, Researcher) %>%
  dplyr::mutate(Date= as.Date(Date, "%m/%d/%Y")) %>% 
  dplyr::mutate(Year= year(Date))%>% 
  dplyr::distinct(.) 

visits<-setDT(visits)   ## change format
# assign visit number per year based on number of events per year per point.
visits<-visits[, Visit:=seq(from = 1, along = list(Transect,Point, Date), by = 1), by= c("Point","Year")] 

# rename columnsto align with R package
visits_clean<-visits %>% 
  dplyr::mutate(Park= stringr::str_sub(Point, 1, 4)) %>%
  
  dplyr::select(Admin_Unit_Code= Park, Transect_Name= Transect, Point_Name = Point, Year,EventDate= Date, StartTime= Start.Time, EndTime= End.Time, Visit, Observer=Researcher)
  

  Write.table(visits_clean, paste(Dir,"Visits.csv", sep="/"), sep= ",", row.names = FALSE)
####### Generate FieldData file----


data<- PointCounts %>% 
  dplyr::filter(!Spp %in% c("CHIP","RESQ","EGSQ","GRSQ", "UNSQ", "---")) %>%  #remove mammal data and missing species codes
  dplyr::filter(!Visit %in% c(9)) %>% # remove QAQC visits
  select(-Visit) %>%  # now drop AKN's bad visit column to bind in derived visit nums from above
  dplyr::left_join(.,NETNintervals[1:2],by= "Time.Bin.ID" ) %>% 
  dplyr::mutate(Date= as.Date(Date, "%m/%d/%Y")) %>% 
  dplyr::left_join(.,visits[,c("Point", "Date", "Visit")],by= c("Point", "Date") )%>% # add visit no. to data
  # add in field to distinguish between obs made within first 3 min of the count
  dplyr::mutate (Initial_Three_Min_Cnt= ifelse(Time.Bin.ID %in% c(7,8,9), 1, 0)) %>%# check with unique(data[c("Time.Bin.ID","Initial_Three_Min_Cnt")])
  dplyr::left_join(.,NETNID_methods,by= "Detection.Cue" ) %>%  # standardize cues from look up table
  dplyr::left_join(.,DistBands,by= "Distance.Bin.ID" ) %>% 
  dplyr::mutate(Flyover_Observed= ifelse(Distance.Bin.ID == "FLY",1,0)) %>% 
  dplyr::rename(Point_Name = Point)%>%
  dplyr::left_join(.,sites[,c("Point_Name", "Survey_Type")],by= "Point_Name" ) %>% 
  dplyr::mutate(Park= stringr::str_sub(Point_Name, 1, 4)) %>%
  dplyr::select(Admin_Unit_Code= Park, Transect_Name= Transect,Point_Name, EventDate= Date, Visit,AOU_Code= Spp, Bird_Count= Count,
                Scientific_Name= Scientific.Name, Common_Name= Common.Name, Interval= Time.Bin.ID, Interval_Length, ID_Method_Code, ID_Method, Distance_id, Distance =Label, 
                Flyover_Observed, Initial_Three_Min_Cnt, Survey_Type)  # rename and select cols
  
  
  
Write.table(data, paste(Dir,"FieldData.csv", sep="/"), sep= ",", row.names = FALSE)

NETN<-importNETNbirds(Dir)


    }
