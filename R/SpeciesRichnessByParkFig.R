#' @include NCRNbirds_Class_def.R
#' 
#' @title SpeciesRichnessByParkFig
#' 
#' @description Generates a stacked bar chart of species richness for all unique species and species of conservation concern (i.e., Partners in Flight watchlist species) at the network and park unit levels.
#' 
#' @importFrom ggplot2 ggplot ggsave geom_point geom_errorbar element_rect ggtitle coord_flip scale_size_continuous geom_bar geom_text scale_fill_manual

#' @param object An NCRNbirds object or a list of such objects.
#' @param color1 User-defined color for stacked bar chart.
#' @param color2 User-defined color for stacked bar chart.
#' @param ... Additonal arguments.
#' 
#' @details This function generates the maximum number of unique species at network and park unit levels across years, and presents these species richness values as a stacked bar chart with user-defined colors.
#' 
#' @export

########################


setGeneric(name="SpeciesRichnessByParkFig",function(object,Dir=NA,color1=NA,color2=NA,...){standardGeneric("SpeciesRichnessByParkFig")}, signature="object")


############ need to add list method I

setMethod(f="SpeciesRichnessByParkFig", signature=c(object="list"),
          function(object,Dir,color1,color2,...) {
            
            return(lapply(object=object,Dir=Dir,color1=color1,color2=color2,FUN=SpeciesRichnessByParkFig,...)
                   )
          }
  )


setMethod(f="SpeciesRichnessByParkFig", signature=c(object="NCRNbirds"),
          function(object,Dir,color1,color2,...){
            
            #Figure 1. Species Richness per park
            
            data<- object@Birds
            
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
            
            unique.birds<-subset(unique.birds, ! AOU_Code %in% removeList)
            
            #now merge with PIF status
            species.merge<-merge(unique.birds, species.updated.sub, by=c("AOU_Code","Common_Name"),all.x=TRUE)
            species.merge$PIF<-ifelse(species.merge$Continental_Concern !="",1,0)
            
            pif.merge<-merge(data, species.merge, by=c("AOU_Code","Common_Name"),all=TRUE)
            pif.merge.df<-na.omit(unique(pif.merge[,c("Admin_Unit_Code","AOU_Code","Continental_Concern","IUCN_Red_List_2016","PIF")]))
            
            #NCRN-wide species richness
            network.species.count<-data.frame(Admin_Unit_Code=object@Network, Count=length(na.omit(unique(pif.merge.df$AOU_Code)))) #157
            network.species.count$Type<-"Total"
            network.cc.count<-data.frame(Admin_Unit_Code=object@Network, Count=sum(species.merge$PIF,na.rm=TRUE)) #27
            network.cc.count$Type<-"Continental Concern"
            network.species.df<-rbind(network.species.count, network.cc.count)
            
            
            #get species count by park
            park.species.count<-aggregate(pif.merge.df$AOU_Code~pif.merge.df$Admin_Unit_Code, FUN=function(x){length(unique(x))})
            park.species.count$Type<-"Total"
            colnames(park.species.count)<-c("Admin_Unit_Code","Count","Type")
            park.cc.count<-aggregate(pif.merge.df$PIF~pif.merge.df$Admin_Unit_Code, FUN=sum)
            park.cc.count$Type<-"Continental Concern"
            colnames(park.cc.count)<-c("Admin_Unit_Code","Count","Type")
            
            #combine
            park.species.df<-unique(rbind(park.species.count, park.cc.count))
            
            #combine park and network-level counts
            species.count.df<-rbind(network.species.df, park.species.df)
            
            #add park names
            park.df<-data.frame(Admin_Unit_Code=unique(object@Points$Admin_Unit_Code), LongName=unique(object@Points$LongName))
            park.df<-rbind(park.df, data.frame(Admin_Unit_Code=object@ParkCode, LongName=object@LongName))
            
            species.count.merge<-merge(species.count.df, park.df, by="Admin_Unit_Code")
            
            #reorder by count
            
            #get order of parks by total species (not cc)
            species.count.sub<-subset(species.count.merge, Type=="Total")
            species.count.sub.order<-species.count.sub[order(species.count.sub$Count, decreasing=FALSE),]
            
            species.count.merge$LongName<-factor(species.count.merge$LongName, levels=unique(species.count.sub.order$LongName))
            species.count.merge$Type<-as.factor(species.count.merge$Type)
            #create figure show counts on columns as labels
            
            color1<-ifelse(is.na(color1),"seagreen4",color1)
            color2<-ifelse(is.na(color2),"orange",color2)
            
            #create colors
            mycolors=c(color1,color2)
            
            species.count.merge$Type<-factor(species.count.merge$Type, levels=c("Total","Continental Concern"))
            
            spp.plot<-ggplot(data=species.count.merge)+
              # geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE),color="darkolivegreen", width=0)+
              geom_bar(data=species.count.merge, stat="identity",aes(x=LongName, y=Count, fill=Type, group=Type))+
              theme(panel.background = element_rect(fill="transparent"),
                    panel.border= element_rect(color="black", fill=NA),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8),
                    axis.title.x = element_text(size=12),
                    axis.title.y = element_text(size=12))+
              theme(legend.background = element_rect(fill="white",
                                                     size=0.5, linetype="solid", 
                                                     colour ="black"),legend.position=c(0.8,0.2))+
              scale_fill_manual(values=mycolors)+
              labs(x="Network/Park",y="Species Richness")+
              geom_text(aes(x=LongName, y=Count, label=Count, group=Type),hjust = 1.3, 
                        color="white", size = 3.5, fontface="bold",inherit.aes = TRUE)+
              coord_flip()
            spp.plot
            
            
            
            
            response<-readline(prompt="Would you like to save these results? (y/n)")
            ifelse(response=="y",
                   {
                     suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))

                     #save plot
                     ggsave(spp.plot, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Figure_Species_Richness",object@ParkCode,Sys.Date(), sep="_"),".png", sep=""),  sep="/"),width=8, height=7, dpi=600)
                     

                     #return table
                     return(spp.plot)
                   },
                   #return table
                   return(spp.plot)
            )
            
          }
)
            