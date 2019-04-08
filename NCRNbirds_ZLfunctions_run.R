#NCRNbirds functions testing

#clear working environment
rm(list=ls())

#set working directory
setwd("YOUR WORKING DIRECTORY")

#load packages
library(devtools)
#install_github("NCRN/NCRNbirds")

########################################################################################################################################################
#use roxygen2 to convert function to documentation (.Rd)

#use document() function to generate corresponding .Rd files for each .R script contianing function
document()
#create a tar.gz file using command line (Terminal from within R)
#tar -zcvf tar-NCRNbirds.tar.gz "/Users/zach/Dropbox (ZachTeam)/Projects/NPS/NCRN/NCRNbirds_R_package/NCRNbirds-master"

#install the package
install.packages("./", repos = NULL, type="source")

#load package
library(NCRNbirds)

#############################################################################################################################
#test estimateOccupancy function

#get all network data
NCRN<-getParkData("./Data/",UnitCode="NCRN")

#Load park or network-wide data
ANTI<-getParkData("./Data/",UnitCode="ANTI")
CATO<-getParkData("./Data/",UnitCode="CATO") 
CHOH<-getParkData("./Data/",UnitCode="CHOH")
GWMP<-getParkData("./Data/",UnitCode="GWMP")
GREE<-getParkData("./Data/",UnitCode="GREE") 
HAFE<-getParkData("./Data/",UnitCode="HAFE")
MANA<-getParkData("./Data/",UnitCode="MANA")
MONO<-getParkData("./Data/",UnitCode="MONO")
NACE<-getParkData("./Data/",UnitCode="NACE")
PISC_FOWA<-getParkData("./Data/",UnitCode="PISC_FOWA") 
PRWI<-getParkData("./Data/",UnitCode="PRWI") 
ROCR<-getParkData("./Data/",UnitCode="ROCR")
WOTR<-getParkData("./Data/",UnitCode="WOTR")

#####################################################################################################################
#generate table of Park unit names and codes
park.table<-ParkTable(object=NCRN)
park.table<-ParkTable(object=ANTI)

#generate table of unique species detected in network, park, and subset by points, years, visits, etc.
species.table<-SpeciesTable(object=NCRN, Dir="./Data")
species.table<-SpeciesTable(object=ANTI, Dir="./Data")

#generate a table of total number of survey points within each park unit and network-wide
point.table<-PointsTable(object=NCRN)
point.table<-PointsTable(object=ANTI)

#generate a table of total number of visits to each point by park unit and year
visit.table<-PointsXVisitsTable(object=NCRN)

#update species info
getUpdatedSpeciesInfo(Dir="./Data")

#check species info within data
CheckSpeciesInfo(object=NCRN,Dir="./Data")
#"Eastern Tufted Titmouse, Solitary Vireo, Whip-poor-will, Wilson's Storm-Petrel"

#create updated speciesList
myUpdatedSpeciesList<-"Tufted Titmouse, Blue-headed Vireo, Eastern Whip-poor-will, Wilson's Storm-Petrel"

#update species info
UpdateSpeciesInfo(object=NCRN, Dir="./Data")

sort(unique(NCRN@Birds$Common_Name))

#load NCRN object with updated data
NCRNnew<-getParkData(Dir="./Data",UnitCode="NCRN")

sort(unique(NCRNnew@Birds$Common_Name))

#check species again in NCRNnew
CheckSpeciesInfo(object=NCRNnew,Dir="./Data")

#make species richniess figure
SpeciesRichnessByParkFig(object=NCRN, Dir="./Data",color1="skyblue3")

#generate tables and heatmaps of species richness by park and year
SpeciesRichnessByParkTable(object=NCRN, Dir="./Data", Figure=TRUE, color="goldenrod")
SpeciesRichnessByParkTable(object=ANTI, Dir="./Data", Figure=TRUE, color="goldenrod")

#generate tables and heatmaps of species richness by point and year
SpeciesRichnessByPointTable(object=NCRN, Dir="./Data", Figure=TRUE, color="royalblue3")
SpeciesRichnessByPointTable(object=ANTI, Dir="./Data", Figure=TRUE, color="royalblue3")

#map species richness
SpeciesRichnessMap(object=NCRN, Dir="./Data",APIkey ="YOUR_API_CODE_HERE", color="red")

#estimate Occupancy
estimateOccupancy(object=NCRN, AOU="NOCA", site.covs="Year",color="royalblue3")
estimateOccupancy(object=ANTI, AOU="NOCA", site.covs="Year",color="royalblue3")

#map Occupancy (models have difficult time fitting at Network level)
mapOccupancy(object=NCRN, AOU="AMRO", mixture="P",color="red",APIkey ="YOUR_API_CODE_HERE")

#estimate Abundance
estimateAbundance(object=NCRN, AOU="NOCA", site.covs="Year", mixture="P",color="royalblue3") 
estimateAbundance(object=ANTI, AOU="NOCA", site.covs="Year", mixture="P",color="royalblue3") 

#map Abundance (models have hard time fitting when passing NCRN as object, but parks work OK.)
mapAbundance(object=NCRN, AOU="NOCA", mixture="P",color="royalblue3", APIkey ="YOUR_API_CODE_HERE")
mapAbundance(object=ANTI, AOU="NOCA", mixture="P",color="royalblue3", APIkey ="YOUR_API_CODE_HERE")

#map BCI
BCIMap(object=ANTI, Dir="./Data",color="royalblue3")




