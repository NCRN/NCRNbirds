#' @include NCRNbirds_Class_def.R 
#' 
#' @title SpeciesRichnessByParkTable
#' 
#' @description Genearates tables and heatmaps showing species richness at park unit and network levels among years.
#' 
#' @importFrom reshape2 melt dcast
#' @importFrom ggplot2 ggplot ggsave geom_point element_rect ggtitle scale_fill_gradient geom_tile scale_x_discrete geom_text
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param Dir A filepath directory to where the data resides.
#' @param Allspecies Logical with default TRUE. Determines whether to present independent species richness table and heatmap for all unique species.
#' @param PIFspecies Logical with default TRUE. Determines whether to present independent species richness table and heatmap for Partners in Flight (PIF) watchlist species.
#' @param color A character string naming a valid color for figures. Default is grayscale.
#' @param ... Additonal arguments passed to \code{\link{SpeciesRichnessByParkTable}}.
#' 
#' @details This function generates tables (and heatmaps) containing network- and park-level species richness across years.
#' 
#' @export


########################


setGeneric(name="SpeciesRichnessByParkTable",function(object,Dir,PIFspecies=TRUE,color=NA,Figure=TRUE,...){standardGeneric("SpeciesRichnessByParkTable")}, signature="object")


############ need to add list method I

setMethod(f="SpeciesRichnessByParkTable", signature=c(object="list"),
          function(object,Dir,Allspecies,PIFspecies,color,Figure,...) {
            
            return(lapply(object=object,Dir=Dir,PIFspecies=PIFspecies,color=color,Figure=Figure, FUN=SpeciesRichnessByParkTable,...)
                   )
          }
  )


setMethod(f="SpeciesRichnessByParkTable", signature=c(object="NCRNbirds"),
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
            
            species.merge.df<-na.omit(unique(species.merge[,c("Admin_Unit_Code","AOU_Code","Year","Continental_Concern","IUCN_Red_List_2016","PIF")]))
            ######################################################################
            #First All Species
            
            #levels(data$Survey_Type)
            #subset data to inclued unique GRTS.Year rows
            data.all.sub<-unique(species.merge.df[,c("Admin_Unit_Code","Year","AOU_Code")])
            
            #make frequency table of num. visits per GRTS.Year.Survey_Type
            tableAll<-aggregate(data.all.sub$AOU_Code ~ data.all.sub$Admin_Unit_Code + data.all.sub$Year, FUN="length", drop=FALSE)
            colnames(tableAll)<-c("Admin_Unit_Code","Year","CountOfAOU_Code")
            #convert NAs to zero
            tableAll$CountOfAOU_Code[is.na(tableAll$CountOfAOU_Code)]<-0
            
            #melt and cast (pivot data)
            tableAll.melt<-melt(tableAll, id.vars=c("Admin_Unit_Code","Year"), measure.vars="CountOfAOU_Code")
            tableAll.cast<-dcast(tableAll.melt, Admin_Unit_Code ~ Year, FUN=max, fill=NA)
            
            #get total unique species for NCRN per year
            data.all.sub.2<-unique(species.merge[,c("Year","AOU_Code")])
            
            #make frequency table of num. visits per GRTS.Year.Survey_Type
            tableAll.2<-aggregate(data.all.sub.2$AOU_Code ~ data.all.sub.2$Year, FUN="length")
            colnames(tableAll.2)<-c("Year","CountOfAOU_Code")
            
            network.all.tot<-data.frame(Admin_Unit_Code= object@Network, t(tableAll.2$CountOfAOU_Code))
            colnames(network.all.tot)[2:length(names(network.all.tot))]<-as.character(seq(from=min(species.merge$Year, na.rm = TRUE), to=max(species.merge$Year, na.rm=TRUE), by=1))
            
            tableAll.comb<-rbind(network.all.tot, tableAll.cast)
            
            #add network and park unit totals
            
            #Network-wide species richness
            network.species.count.all<-data.frame(Admin_Unit_Code=object@Network, Total=length(na.omit(unique(species.merge.df$AOU_Code)))) #157

            #get species count by park
            park.species.count.all<-aggregate(species.merge$AOU_Code ~ species.merge$Admin_Unit_Code, FUN=function(x){length(unique(x))})
            colnames(park.species.count.all)<-c("Admin_Unit_Code","Total")
            
            #combine Network and park species count totals
            species.count.all<-rbind(network.species.count.all, park.species.count.all)
            
            #now merge with species richness table
            species.count.merge.all<-merge(tableAll.comb, species.count.all, by="Admin_Unit_Code",sort=FALSE)
            
            #add park names
            park.df<-data.frame(Admin_Unit_Code=unique(object@Points$Admin_Unit_Code), LongName=unique(object@Points$LongName))
            park.df<-rbind(park.df, data.frame(Admin_Unit_Code=object@ParkCode, LongName=object@LongName))
            
            #merge with species.count.merge.all
            species.count.all.out<-merge(species.count.merge.all, park.df, by="Admin_Unit_Code",all.x=TRUE,sort=FALSE)
            
            #reorganize table
            species.count.all.out<-cbind(LongName=species.count.all.out$LongName, species.count.all.out[, !(names(species.count.all.out) %in% "LongName")])
            
            ################################################################################
            #PIF species
            
            #subset PIF species
            data.pif<-subset(species.merge, PIF==1)
            
            pif.merge.df<-na.omit(unique(data.pif[,c("Admin_Unit_Code","AOU_Code","Year","Continental_Concern","IUCN_Red_List_2016","PIF")]))
            
            #subset data to inclued unique GRTS.Year rows
            data.pif.sub<-unique(pif.merge.df[,c("Admin_Unit_Code","Year","AOU_Code")])
            
            #make frequency table of num. visits per GRTS.Year.Survey_Type
            tablePIF<-aggregate(data.pif.sub$AOU_Code ~ data.pif.sub$Admin_Unit_Code + data.pif.sub$Year, FUN="length",drop=FALSE)
            colnames(tablePIF)<-c("Admin_Unit_Code","Year","CountOfAOU_Code")
            #convert NAs to zero
            tablePIF$CountOfAOU_Code[is.na(tablePIF$CountOfAOU_Code)]<-0
            
            
            #melt and cast (pivot data)
            tablePIF.melt<-melt(tablePIF, id.vars=c("Admin_Unit_Code","Year"), measure.vars="CountOfAOU_Code")
            tablePIF.cast<-dcast(tablePIF.melt, Admin_Unit_Code ~ Year, FUN=max, fill=0)
            
            #get total unique species for NCRN per year
            data.pif.sub.2<-unique(pif.merge.df[,c("Year","AOU_Code")])
            
            #make frequency table of num. visits per GRTS.Year.Survey_Type
            tablePIF.2<-aggregate(data.pif.sub.2$AOU_Code ~ data.pif.sub.2$Year, FUN="length")
            colnames(tablePIF.2)<-c("Year","CountOfAOU_Code")
            
            network.pif.tot<-data.frame(Admin_Unit_Code= object@Network, t(tablePIF.2$CountOfAOU_Code))
            colnames(network.pif.tot)[2:length(names(network.pif.tot))]<-as.character(seq(from=min(pif.merge.df$Year, na.rm = TRUE), to=max(pif.merge.df$Year, na.rm=TRUE), by=1))
            
            tablePIF.comb<-rbind(network.pif.tot, tablePIF.cast)
            
            #add network and park unit totals
            
            #Network-wide species richness
            network.species.count.pif<-data.frame(Admin_Unit_Code=object@Network, Total=length(na.omit(unique(pif.merge.df$AOU_Code)))) #157
            
            #get species count by park
            park.species.count.pif<-aggregate(pif.merge.df$AOU_Code ~ pif.merge.df$Admin_Unit_Code, FUN=function(x){length(unique(x))})
            colnames(park.species.count.pif)<-c("Admin_Unit_Code","Total")
            
            #combine Network and park species count totals
            species.count.pif<-rbind(network.species.count.pif, park.species.count.pif)
            
            #now merge with species richness table
            species.count.merge.pif<-merge(tablePIF.comb, species.count.pif, by="Admin_Unit_Code",sort=FALSE)
            
            #merge with species.count.merge.all
            species.count.pif.out<-merge(species.count.merge.pif, park.df, by="Admin_Unit_Code",all.x=TRUE, sort=FALSE)
            
            #reorganize table
            species.count.pif.out<-cbind(LongName=species.count.pif.out$LongName, species.count.pif.out[, !(names(species.count.pif.out) %in% "LongName")])
            
            ################################################################################
            #make Heatmaps of tables
            
            #get min and max of range of species richness
            maxSpecies<-max(tableAll$CountOfAOU_Code,na.rm=TRUE)
            
            #All specie richness
            #add park LongNames
            tableAllmerge<-merge(tableAll, park.df, by="Admin_Unit_Code",all.x=TRUE)
            
            #reorder factor leves for LongName
            parkOrder<-sort(unique(tableAllmerge$LongName))
            tableAllmerge$LongName<-factor(tableAllmerge$LongName, levels=rev(parkOrder))
            levels(tableAllmerge$LongName)

            color<-ifelse(is.na(color),I("gray30"), color)
            
            heat.map.all<-ggplot(data=tableAllmerge, aes(x=as.factor(Year), y=LongName, fill=CountOfAOU_Code)) + 
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
              labs(x="Year", y="Park Unit")+
              geom_text(aes(label = round(CountOfAOU_Code, 0)), size = 3,color="black")+
              ggtitle("Heat map of species richness for all species among park units and years.")
            
            heat.map.all
            
            #PIF species richness
            #add park LongNames
            tablePIFmerge<-merge(tablePIF, park.df, by="Admin_Unit_Code",all.x=TRUE)
            #reorder factor leves for LongName
            parkOrder<-sort(unique(tablePIFmerge$LongName))
            tablePIFmerge$LongName<-factor(tablePIFmerge$LongName, levels=rev(parkOrder))
            levels(tablePIFmerge$LongName)
            
            #get min and max of range of species richness
            maxSpeciesPIF<-max(tablePIFmerge$CountOfAOU_Code,na.rm=TRUE)
            
            
            heat.map.pif<-ggplot(data=tablePIFmerge, aes(x=as.factor(Year), y=LongName, fill=CountOfAOU_Code)) + 
              geom_tile(color="black")+
              theme(panel.background = element_rect(fill="transparent"),
                    panel.border= element_rect(color="white", fill="transparent"),
                    axis.text.x = element_text(size=10),
                    axis.text.y = element_text(size=10),
                    title = element_text(size=12),
                    legend.key.width=unit(2,"lines"),
                    legend.key.height=unit(3,"lines"))+
              scale_fill_gradient(low = "white", high = color,na.value="gray",
                                  limits=c(0, ceiling(maxSpeciesPIF)),
                                  breaks=c(round(seq(0,maxSpeciesPIF, length.out=5),0)))+
              scale_x_discrete(name = "Year", breaks = as.character(seq(min(tableAll$Year), max(tableAll$Year))))+
              labs(x="Year", y="Park Unit")+
              geom_text(aes(label = round(CountOfAOU_Code, 0)), size = 3,color="black")+
              ggtitle("Heat map of species richness for PIF watchlist species among park units and years.")
            
            heat.map.pif
            
            #get info for creating file names
            parkCodes<-paste(unique(object@ParkCode),collapse="_")

            ###############
            response<-readline(prompt="Would you like to save these results? (y/n)")
                     ifelse(response=="y",
                            {
                              suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))
                              
                              #save table of species richness for all species
                              write.csv(species.count.all.out, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_SpeciesRichnessByParkAndYear",parkCodes,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                              
                              #save table of species richness for PIF species
                              write.csv(species.count.pif.out, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_SpeciesRichnessByParkAndYear_PIF",parkCodes,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                              
                              
                              ggsave(heat.map.all, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(parkCodes, "HeatMapSpeciesRichnessParkAndYear",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=10, units="in", dpi=300)
                              
                              ggsave(heat.map.pif, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(parkCodes, "HeatMapSpeciesRichnessParkAndYear_PIF",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=10, units="in", dpi=300)
                              
                              
                              #return table and fig
                              #return table and fig
                              if(isTRUE(PIFspecies) & isFALSE(Figure)){
                                return(list(
                                       "Table of count of unique species (species richness) for all species." = species.count.all.out,
                                       "Table of count of unique species (species richness) for PIF species." = species.count.pif.out))
                              }else{
                                if(isFALSE(PIFspecies) & isTRUE(Figure)){
                                  return(list(
                                    "Table of count of unique species (species richness) for all species." = species.count.all.out, 
                                    heat.map.all))
                                }else{
                                  if(isFALSE(PIFspecies) & isFALSE(Figure)){
                                    return(list(
                                     "Table of count of unique species (species richness) for all species." = species.count.all.out))
                                  }else{
                                    return(list(
                                      "Table of count of unique species (species richness) for all species." = species.count.all.out,
                                      "Table of count of unique species (species richness) for PIF species."= species.count.pif.out, 
                                      heat.map.all, 
                                      heat.map.pif))
                                    
                                  }}}
                            },
                            #return table and fig
                            if(isTRUE(PIFspecies) & isFALSE(Figure)){
                              return(list(
                                "Table of count of unique species (species richness) for all species." = species.count.all.out,
                                "Table of count of unique species (species richness) for PIF species." = species.count.pif.out))
                            }else{
                              if(isFALSE(PIFspecies) & isTRUE(Figure)){
                                return(list(
                                  species.count.all.out, 
                                  heat.map.all))
                              }else{
                                if(isFALSE(PIFspecies) & isFALSE(Figure)){
                                  return(list(
                                    "Table of count of unique species (species richness) for all species." = species.count.all.out))
                                }else{
                                  return(list(
                                    "Table of count of unique species (species richness) for all species." = species.count.all.out,
                                    "Table of count of unique species (species richness) for PIF species." = species.count.pif.out, 
                                    heat.map.all,
                                    heat.map.pif))
                                }}}
                     )
                     
                     


          }
)
            