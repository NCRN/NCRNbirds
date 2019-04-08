#' @include NCRNbirds_Class_def.R 
#' 
#' @title SpeciesRichnessMap
#' 
#' @description Generates a map of point-level species richness.
#' 
#' @importFrom reshape2 melt dcast
#' @importFrom stats aggregate
#' @importFrom ggmap ggmap register_google get_googlemap get_map ggmap_credentials showing_key 
#' @importFrom ggplot2 ggplot ggsave geom_point element_rect ggtitle scale_color_gradient
#'  
#' @param object An NCRNbirds object or a list of such objects.
#' @param Dir A filepath directory to where the data resides.
#' @param PIFspecies Logical with default TRUE. Determines whether to present independent species richness map for Partners in Flight (PIF) watchlist species.
#' @param color A character string naming a valid color for figures. Default is grayscale.
#' @param APIkey A character string of a Google API key used for generating maps with Google basemaps. If no APIkey is provided, basemaps default to stamen map.
#' @param ... Additonal arguments passed to \code{\link{SpeciesRichnessMap}}.
#' 
#' @details This function determines the maximum count of unique species at each sampling point, and generates maps for either or both all unique species and PIF watchlist species across sampling years. 
#' 
#' 
#' @export


########################


setGeneric(name="SpeciesRichnessMap",function(object,Dir,PIFspecies=TRUE,color=NA,APIkey=NA,...){standardGeneric("SpeciesRichnessMap")}, signature="object")


############ need to add list method I

setMethod(f="SpeciesRichnessMap", signature=c(object="list"),
          function(object,Dir,Allspecies,PIFspecies,color,APIkey,...) {
            
            return(lapply(object=object,Dir=Dir,PIFspecies=PIFspecies,color=color,APIkey=APIkey, FUN=SpeciesRichnessMap,...)
                   )
          }
  )


setMethod(f="SpeciesRichnessMap", signature=c(object="NCRNbirds"),
          function(object,Dir,PIFspecies,color,APIkey,...){
            

            data=object@Birds
            
            #add Year
            data$Year<-year(data$EventDate)
            
            #read in updated species info
            species.updated<-read.csv(paste(Dir,"AOS_Codes","SpeciesList_out.csv",sep="/"),as.is=TRUE)  
            
            species.updated.sub<-species.updated[,c("AOU_Code","Common_Name","Scientific_Name","Continental_Concern","IUCN_Red_List_2016")]
            
            #condense unique birds within data
            unique.birds<-unique(data[,c("Common_Name","AOU_Code")])
            
            #remove blanks and any "unidentified" codes
            unique.birds<-subset(unique.birds, AOU_Code !="")
            
            #get list of Unidentified spp.
            UnidentifiedList<-unique.birds[grep("Unidentified", unique.birds$Common_Name),]
            
            #species to remove
            removeList<-UnidentifiedList$AOU_Code
            
            #remove Unidentified species from data
            data<-subset(data, ! AOU_Code %in% removeList)
            
            #now merge with PIF status
            species.merge<-merge(data, species.updated.sub, by=c("AOU_Code","Common_Name"),all.x=TRUE)
            species.merge$PIF<-ifelse(species.merge$Continental_Concern !="",1,0)
            
            species.merge.df<-na.omit(unique(species.merge[,c("Admin_Unit_Code","Point_Name","AOU_Code","Year","Continental_Concern","IUCN_Red_List_2016","PIF")]))
            ######################################################################
            #First All Species
            
            #levels(data$Survey_Type)
            #subset data to inclued unique GRTS.Year rows
            data.all.sub<-unique(species.merge.df[,c("Admin_Unit_Code","Year","Point_Name","AOU_Code")])
            
            #make frequency table of num. visits per GRTS.Year.Survey_Type
            tableAll<-aggregate(data.all.sub$AOU_Code ~ data.all.sub$Point_Name + data.all.sub$Year, FUN="length", drop=FALSE)
            colnames(tableAll)<-c("Point_Name","Year","CountOfAOU_Code")
            
            #convert NAs to zero
            #tableAll$CountOfAOU_Code[is.na(tableAll$CountOfAOU_Code)]<-0
            
            #melt and cast (pivot data)
            tableAll.melt<-melt(tableAll, id.vars=c("Point_Name","Year"), measure.vars="CountOfAOU_Code")
            tableAll.cast<-dcast(tableAll.melt, Point_Name ~ Year, FUN=max)

            #get species count by park
            park.species.count.all<-aggregate(species.merge$AOU_Code ~ species.merge$Point_Name, FUN=function(x){length(unique(x))})
            colnames(park.species.count.all)<-c("Point_Name","Total")
            
            #combine Network and park species count totals
            species.count.all<-park.species.count.all
            
            #now merge with species richness table
            species.count.merge.all<-merge(tableAll.melt, species.count.all, by="Point_Name",sort=FALSE)
            
            #add park names
            park.df<-unique(data.frame(Point_Name=object@Points$Point_Name, Admin_Unit_Code=object@Points$Admin_Unit_Code, LongName=object@Points$LongName))

            #merge with species.count.merge.all
            species.count.all.out<-merge(species.count.merge.all, park.df, by="Point_Name",all.x=TRUE,sort=FALSE)
            
            #reorganize table
            species.count.all.out<-cbind(LongName=species.count.all.out$LongName, Admin_Unit_Code=species.count.all.out$Admin_Unit_Code, species.count.all.out[, !(names(species.count.all.out) %in% c("LongName","Admin_Unit_Code"))])
            
            ################################################################################
            #PIF species
            
            #subset PIF species
            data.pif<-subset(species.merge, PIF==1)
            
            pif.merge.df<-unique(data.pif[,c("Admin_Unit_Code","Point_Name","AOU_Code","Year","Continental_Concern","IUCN_Red_List_2016","PIF")])
            
            #subset data to inclued unique GRTS.Year rows
            data.pif.sub<-unique(pif.merge.df[,c("Admin_Unit_Code","Point_Name","Year","AOU_Code")])
            
            #make frequency table of num. visits per GRTS.Year.Survey_Type
            tablePIF<-aggregate(data.pif.sub$AOU_Code ~ data.pif.sub$Point_Name + data.pif.sub$Year, FUN="length",drop=FALSE)
            colnames(tablePIF)<-c("Point_Name","Year","CountOfAOU_Code")
            
            
            #create table of all points and years
            allPointsYears<-unique(tableAll[,c("Point_Name","Year")])
            
            #merge with all points
            tablePIF<-merge(tablePIF, allPointsYears, by=c("Point_Name","Year"),all.y=TRUE)
            
            #Replace points not sampled with NAs
            tablePIF.NAs<-merge(tablePIF, tableAll, by=c("Point_Name","Year"),all.x=TRUE)
            
            #convert NAs to zero
            tablePIF.NAs$CountOfAOU_Code.x[is.na(tablePIF.NAs$CountOfAOU_Code.x)]<-0

            #Map NAs from All to PIF counts
            tablePIF.NAs$CountOfAOU_Code.x<-ifelse(is.na(tablePIF.NAs$CountOfAOU_Code.y),NA,tablePIF.NAs$CountOfAOU_Code.x)
            tablePIF.NAs$CountOfAOU_Code.y<-NULL
            names(tablePIF.NAs)[names(tablePIF.NAs)=="CountOfAOU_Code.x"]<-"CountOfAOU_Code"
            
            tablePIF<-tablePIF.NAs

            #melt and cast (pivot data)
            tablePIF.melt<-melt(tablePIF, id.vars=c("Point_Name","Year"), measure.vars="CountOfAOU_Code")
            tablePIF.cast<-dcast(tablePIF.melt, Point_Name ~ Year, FUN=max,drop=FALSE)

            #get species count by park
            park.species.count.pif<-aggregate(pif.merge.df$AOU_Code ~ pif.merge.df$Point_Name, FUN=function(x){length(unique(x))})
            colnames(park.species.count.pif)<-c("Point_Name","Total")
            
            #combine Network and park species count totals
            species.count.pif<-park.species.count.pif
            
            #now merge with species richness table
            species.count.merge.pif<-merge(tablePIF.cast, species.count.pif, by="Point_Name",sort=FALSE)
            
            #merge with species.count.merge.all
            species.count.pif.out<-merge(species.count.merge.pif, park.df, by="Point_Name",all.x=TRUE, sort=FALSE)
            
            #reorganize table
            species.count.pif.out<-cbind(LongName=species.count.pif.out$LongName, Admin_Unit_Code=species.count.pif.out$Admin_Unit_Code,species.count.pif.out[, !(names(species.count.pif.out) %in% c("LongName","Admin_Unit_Code"))])
            
            ################################################################################
            #make Heatmaps of tables
            
            #get min and max of range of species richness
            maxSpecies<-max(tableAll$CountOfAOU_Code,na.rm=TRUE)
            
            #All specie richness
            #add park LongNames
            tableAllmerge<-merge(tableAll, park.df, by="Point_Name",all.x=TRUE)
            
            #reorder factor leves for LongName
            parkOrder<-sort(unique(tableAllmerge$LongName))
            tableAllmerge$LongName<-factor(tableAllmerge$LongName, levels=parkOrder)
            levels(tableAllmerge$LongName)
            
            pointOrder<-sort(unique(tableAllmerge$Point_Name))
            tableAllmerge$Point_Name<-factor(tableAllmerge$Point_Name, levels=rev(pointOrder))
            levels(tableAllmerge$Point_Name)
            
            color<-ifelse(is.na(color),I("gray30"), color)
            
            parkList<-unique(as.character(tableAllmerge$LongName))
            
            #create list with ParkCodes
            parkCodeList<-unique(as.character(tableAllmerge$Admin_Unit_Code))
            
            #map of species richness by point
            point.data<-object@Points
            coords<-unique(point.data[,c("Point_Name","Latitude","Longitude")])
            
            tableAll.coords<-merge(tableAllmerge, coords, by="Point_Name",all.x=TRUE)
            
            
            #create map of abundance among points
            mean.x<-mean(tableAll.coords$Longitude, na.rm=TRUE)
            mean.y<-mean(tableAll.coords$Latitude, na.rm=TRUE)
            
            
            parkZoom<-switch(object@ParkCode,
                             "NCRN"=9,
                             "ANTI"=14,
                             "CATO"=13,
                             "CHOH"= 9,
                             "GREE"= 15,
                             "GWMP"=11,
                             "HAFE"=13,
                             "MANA"=13,
                             "MONO"=13,
                             "NACE"=13,
                             "PISC_FOWA"=13,
                             "PRWI"=13,
                             "ROCR"=13,
                             "WOTR"=15)
            
            #get center of map
            map.center<-c(mean.x,mean.y)
            
            #if APIkey=NA use stamen map, if user has a google API key, use google map
            
            if(is.na(APIkey)){
              park.map <- get_map(location = c(lon=map.center[1], lat=map.center[2]), zoom = parkZoom, maptype="toner",source="stamen")
            } else {
              #set my Google API key
              myAPIkey = APIkey
              register_google(key=myAPIkey , write = TRUE)
              
              park.map <- get_googlemap(center = map.center, zoom = parkZoom, maptype="terrain",
                                        style = 'feature:administrative.country|element:labels|visibility:off',color="bw" )
            }
            
            color=ifelse(is.na(color),"black",color)
            
            tableAll.coords$CountOfAOU_Code<-as.numeric(as.character(tableAll.coords$CountOfAOU_Code))
            

            park.map.out.all <- ggmap(park.map, darken = c(0.3, "white"))+
              #geom_polygon(data=park.map.df, aes(x=long, y=lat, group=group),fill="darkgreen",color="black",alpha=0.6)+
              #geom_point(data=Abun.Table, shape=1,aes(x=Longitude,y=Latitude, size=Predicted+SE),color="red",alpha=0.9)+
              geom_point(data=tableAll.coords, size=1.5, aes(x=Longitude,y=Latitude, color=CountOfAOU_Code), alpha=0.6)+
              scale_color_gradient(low = "white", high = color,na.value="gray",
                                  limits=c(0, ceiling(maxSpecies)),
                                  breaks=c(round(seq(0,maxSpecies, length.out=5),0)))+
              #scale_color_viridis(option="D")+
              theme(panel.border=element_rect(fill="transparent",color="black"))+
              theme(panel.background=element_rect(fill='white',color="black"))+
              theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
              theme(axis.line=element_line(color="black"))+
              theme(axis.text.x = element_text(size=10, color="black"),
                    axis.text.y = element_text(size=10, color="black"),
                    axis.title.x = element_text(size=11, hjust=0.5, vjust=1.9),
                    axis.title.y = element_text(angle = 90, vjust=1.2, size=11),
                    title=element_text(size=14))+
              labs(x="Longitude",y="Latitude", size=paste("Species Richness"))+
              ggtitle(paste(strwrap(paste("Map of species richness in ", object@LongName," from ", min(tableAll.coords$Year,na.rm=TRUE), " to ", max(tableAll.coords$Year, na.rm=TRUE)), width=70),collapse = "\n"))
            park.map.out.all
            
            #facet map by year
            #park.map.facet.all<-park.map.out.all+facet_wrap(~Year)

            #PIF species richness
            #add park LongNames
            tablePIFmerge<-merge(tablePIF, park.df, by="Point_Name",all.x=TRUE)
            
            #reorder factor leves for LongName
            parkOrder<-sort(unique(tablePIFmerge$LongName))
            tablePIFmerge$LongName<-factor(tablePIFmerge$LongName, levels=parkOrder)
            levels(tablePIFmerge$LongName)
            
            pointOrder<-sort(unique(tablePIFmerge$Point_Name))
            tablePIFmerge$Point_Name<-factor(tablePIFmerge$Point_Name, levels=rev(pointOrder))
            levels(tablePIFmerge$Point_Name)
            
            #get min and max of range of species richness
            maxSpeciesPIF<-max(tablePIFmerge$CountOfAOU_Code,na.rm=TRUE)
            
            #Map PIF species richness
            tablePIF.coords<-merge(tablePIFmerge, coords, by="Point_Name",all.x=TRUE)
            
            #if APIkey=NA use stamen map, if user has a google API key, use google map
            
            if(is.na(APIkey)){
              park.map <- get_map(location = c(lon=map.center[1], lat=map.center[2]), zoom = parkZoom, maptype="toner",source="stamen")
            } else {
              #set my Google API key
              myAPIkey = APIkey
              register_google(key=myAPIkey , write = TRUE)
              
              park.map <- get_googlemap(center = map.center, zoom = parkZoom, maptype="terrain",
                                        style = 'feature:administrative.country|element:labels|visibility:off',color="bw" )
            }
            
            color=ifelse(is.na(color),"black",color)
            
            tableAll.coords$CountOfAOU_Code<-as.numeric(as.character(tableAll.coords$CountOfAOU_Code))
            
            park.map.out.pif <- ggmap(park.map, darken = c(0.3, "white"))+
              #geom_polygon(data=park.map.df, aes(x=long, y=lat, group=group),fill="darkgreen",color="black",alpha=0.6)+
              #geom_point(data=Abun.Table, shape=1,aes(x=Longitude,y=Latitude, size=Predicted+SE),color="red",alpha=0.9)+
              geom_point(data=tablePIF.coords, size=1.5,aes(x=Longitude,y=Latitude, color=CountOfAOU_Code), alpha=0.6)+
              scale_color_gradient(low = "white", high = color,na.value="gray",
                                   limits=c(0, ceiling(maxSpeciesPIF)),
                                   breaks=c(seq(0,maxSpeciesPIF, length.out=5)))+
              #scale_color_viridis(option="D")+
              theme(panel.border=element_rect(fill="transparent",color="black"))+
              theme(panel.background=element_rect(fill='white',color="black"))+
              theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
              theme(axis.line=element_line(color="black"))+
              theme(axis.text.x = element_text(size=10, color="black"),
                    axis.text.y = element_text(size=10, color="black"),
                    axis.title.x = element_text(size=11, hjust=0.5, vjust=1.9),
                    axis.title.y = element_text(angle = 90, vjust=1.2, size=11),
                    title=element_text(size=14))+
              labs(x="Longitude",y="Latitude", size=paste("Species Richness"))+
              ggtitle(paste(strwrap(paste("Map of PIF species richness in ", object@LongName," from ", min(tablePIF.coords$Year,na.rm=TRUE), " to ", max(tablePIF.coords$Year, na.rm=TRUE)), width=70),collapse = "\n"))
            park.map.out.pif
            
            #facet map by year
            #park.map.facet.pif<-park.map.out.pif+facet_wrap(~Year)
            
            
            ###############
            response<-readline(prompt="Would you like to your map? (y/n)")
                     ifelse(response=="y",
                            {
                              suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))
                            
                              #save map
                              ggsave(park.map.out.all, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(object@ParkCode, "MapSpeciesRichnessPoint",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=10, units="in", dpi=300)
                              
                              ggsave(park.map.out.pif, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(object@ParkCode, "MapSpeciesRichnessPoint_PIF",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=10, units="in", dpi=300)
                              
                              
                              #return table and fig
                              if(isTRUE(PIFspecies)){
                                return(list(
                                  park.map.out.all,
                                  park.map.out.pif
                                ))
                              }else{
                                if(isFALSE(PIFspecies)){
                                  park.map.out.all
                                }}
                                
                            },
                            #return table and fig
                            if(isTRUE(PIFspecies)){
                              return(list(
                                park.map.out.all,
                                park.map.out.pif
                              ))
                            }else{
                              if(isFALSE(PIFspecies)){
                                park.map.out.all
                              }}
                  )
                     
                     


          }
)
            