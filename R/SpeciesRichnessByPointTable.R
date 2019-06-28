#' @include NCRNbirds_Class_def.R 
#' 
#' @title SpeciesRichnessByPointTable
#' 
#' @description Generates a table and exports a .csv file, and generates a heatmap figure of point-level species richness.
#' 
#' @importFrom reshape2 melt dcast
#' @importFrom ggplot2 ggplot ggsave geom_point element_rect ggtitle scale_fill_gradient geom_tile scale_x_discrete geom_text unit
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param Dir A filepath directory to where the data resides.
#' @param Allspecies Logical with default TRUE. Determines whether to present independent species richness table and heatmap for all unique species.
#' @param PIFspecies Logical with default TRUE. Determines whether to present independent species richness table and heatmap for Partners in Flight (PIF) watchlist species.
#' @param color A character string naming a valid color for figures. Default is grayscale.
#' @param ... Additonal arguments passed to \code{\link{SpeciesRichnessByPointTable}}.
#' 
#' @details This function generates tables (and heatmaps) containing network- and park-level species richness at each sampling point and year.
#' 
#' @export


########################


setGeneric(name="SpeciesRichnessByPointTable",function(object,Dir,PIFspecies=TRUE,color=NA,Figure=TRUE,...){standardGeneric("SpeciesRichnessByPointTable")}, signature="object")


############ need to add list method I

setMethod(f="SpeciesRichnessByPointTable", signature=c(object="list"),
          function(object,Dir,Allspecies,PIFspecies,color,Figure,...) {
            
            return(lapply(object=object,Dir=Dir,PIFspecies=PIFspecies,color=color,Figure=Figure, FUN=SpeciesRichnessByPointTable,...)
                   )
          }
  )


setMethod(f="SpeciesRichnessByPointTable", signature=c(object="NCRNbirds"),
          function(object,Dir,PIFspecies,color,Figure,...){
            

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
            
            heat.map.all.list<-list()
            for(i in 1:length(parkCodeList)){
              
              temp.park<-subset(tableAllmerge, Admin_Unit_Code==parkCodeList[i])
              
              heat.map.all<-ggplot(data=temp.park, aes(x=as.factor(Year), y=Point_Name, fill=CountOfAOU_Code)) + 
              geom_tile(color="black")+
              theme(panel.background = element_rect(fill="transparent"),
                    panel.border= element_rect(color="white", fill="transparent"),
                    axis.text.x = element_text(size=10),
                    axis.text.y = element_text(size=10),
                    title = element_text(size=12),
                    legend.key.width=unit(2,"lines"),
                    legend.key.height=unit(3,"lines"))+
              scale_fill_gradient(low = "white", high = color,na.value="gray",
                                  limits=c(0, ceiling(maxSpecies)),
                                  breaks=c(round(seq(0,maxSpecies, length.out=5),0)))+
              scale_x_discrete(name = "Year", breaks = as.character(seq(min(tableAll$Year), max(tableAll$Year))))+
              labs(x="Year", y="Point Name")+
              geom_text(aes(label = round(CountOfAOU_Code, 0)), size = 3,color="black")+
              ggtitle(paste(strwrap(paste("Heatmap of species richness for all species in ", unique(temp.park$LongName), " among years.",sep=""), width=70),collapse = "\n"))
            
            heat.map.all
            
            heat.map.all.list[[i]]<-heat.map.all
            }
            

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
            
            heat.map.pif.list<-list()
            for(i in 1:length(parkCodeList)){
              
            temp.park<-subset(tablePIFmerge, Admin_Unit_Code==parkCodeList[i])
              
            heat.map.pif<-ggplot(data=temp.park, aes(x=as.factor(Year), y=Point_Name, fill=CountOfAOU_Code)) + 
              geom_tile(color="black")+
              theme(panel.background = element_rect(fill="transparent"),
                    panel.border= element_rect(color="white", fill="transparent"),
                    axis.text.x = element_text(size=10),
                    axis.text.y = element_text(size=10),
                    title = element_text(size=12),
                    legend.key.width=unit(2,"lines"),
                    legend.key.height=unit(3,"lines"))+
              scale_fill_gradient(low = "white", high = color, na.value="gray",
                                  limits=c(0, ceiling(maxSpeciesPIF)),
                                  breaks=c(round(seq(0,maxSpeciesPIF, length.out=5),0)))+
              scale_x_discrete(name = "Year", breaks = as.character(seq(min(tableAll$Year), max(tableAll$Year))))+
              labs(x="Year", y="Park Unit")+
              geom_text(aes(label = round(CountOfAOU_Code, 0)), size = 3,color="black")+
              ggtitle(paste(strwrap(paste("Heatmap of species richness for PIF species in ", unique(temp.park$LongName), " among years.",sep=""), width=70),collapse = "\n"))
            
            heat.map.pif
            
            heat.map.pif.list[[i]]<-heat.map.pif
            }
            
            
            #get info for creating file names
            parkCodes<-paste(unique(object@ParkCode),collapse="_")

            ###############
            response<-readline(prompt="Would you like to save these results? (y/n)")
                     ifelse(response=="y",
                            {
                              suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))
                              
                              #save table of species richness for all species
                              write.csv(species.count.all.out, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_SpeciesRichnessByPointAndYear",parkCodes,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                              
                              #save table of species richness for PIF species
                              write.csv(species.count.pif.out, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_SpeciesRichnessByPointAndYear_PIF",parkCodes,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                              
                              for(j in 1:length(parkCodeList)){
                              ggsave(heat.map.all.list[[j]], file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(parkCodeList[j], "HeatMapSpeciesRichnessPointAndYear",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=14, units="in", dpi=300)
                              }
                              
                              for(j in 1:length(parkCodeList)){
                              ggsave(heat.map.pif.list[[j]], file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(parkCodeList[j], "HeatMapSpeciesRichnessPointAndYear_PIF",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=14, units="in", dpi=300)
                            }
                              
                              #return table and fig
                              #return table and fig
                              if(isTRUE(PIFspecies) & isFALSE(Figure)){
                                return(list(
                                       "Table of count of unique species (species richness) per point for all species." = species.count.all.out,
                                       "Table of count of unique species (species richness) per point for PIF species." = species.count.pif.out))
                              }else{
                                if(isFALSE(PIFspecies) & isTRUE(Figure)){
                                  return(list(
                                    "Table of count of unique species (species richness) per point for all species." = species.count.all.out,
                                      heat.map.all.list
                                    ))
                                }else{
                                  if(isFALSE(PIFspecies) & isFALSE(Figure)){
                                    return(list(
                                     "Table of count of unique species (species richness) per point for all species." = species.count.all.out))
                                  }else{
                                    return(list(
                                      "Table of count of unique species (species richness) per point for all species." = species.count.all.out,
                                      "Table of count of unique species (species richness) per point for PIF species."= species.count.pif.out, 
                                        heat.map.all.list, 
                                        heat.map.pif.list
                                      ))
                                    
                                  }}}
                            },
                            #return table and fig
                            if(isTRUE(PIFspecies) & isFALSE(Figure)){
                              return(list(
                                "Table of count of unique species (species richness) per point for all species." = species.count.all.out,
                                "Table of count of unique species (species richness)per point for PIF species." = species.count.pif.out))
                            }else{
                              if(isFALSE(PIFspecies) & isTRUE(Figure)){
                                return(list(
                                  "Table of count of unique species (species richness) per point for all species." = species.count.all.out, 
                                    heat.map.all.list
                                  ))
                              }else{
                                if(isFALSE(PIFspecies) & isFALSE(Figure)){
                                  return(list(
                                    "Table of count of unique species (species richness) per point for all species." = species.count.all.out))
                                }else{
                                  return(list(
                                    "Table of count of unique species (species richness) per point for all species." = species.count.all.out,
                                    "Table of count of unique species (species richness) per point for PIF species." = species.count.pif.out, 
                                      heat.map.all.list,
                                      heat.map.pif.list
                                    ))
                                }}}
                     )
                     
                     


          }
)
            