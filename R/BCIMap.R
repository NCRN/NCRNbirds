#' @include NCRNbirds_Class_def.R BCI.R getVisits.R getBirds.R 
#' 
#' @title BCIMap
#' 
#' @description Generate a map of bird community index (BCI) scores at network and park unit levels.
#' 
#' @importFrom reshape2 melt dcast
#' @importFrom ggmap ggmap register_google get_googlemap get_map ggmap_credentials showing_key 
#' @importFrom ggplot2 ggplot ggsave geom_point element_rect ggtitle scale_color_gradient geom_tile scale_x_discrete geom_text unit
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param years  A vector of numbers that will return only data from the indicated years.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param color A character string naming a valid color for figures. Default is grayscale.
#' @param APIkey A character string of a Google API key used for generating maps with Google basemaps. If no APIkey is provided, basemaps default to stamen map.
#' @param ... Additonal arguments passed to \code{\link{BCIMap}}.
#'  
#' @details This function calculates point-level bird community index (BCI) scores and generates a map showing BCI scores plotted using a color ramp.
#'
#' @references 
#' O'Connell, T., R. Brooks, M. Lanzone, and J. Bishop. 2003. A bird community index for the mid-Atlantic piedmont and coastal plain, Final Report to the USGS-Patuxent Wildlife Research Center. Penn State Cooperative Wetlands Center, University Park, p 44. Report.
#' O'Connell, T. J., L. E. Jackson, and R. P. Brooks. 1998. The bird community index: A tool for assessing biotic integrity in the mid-Atlantic highlands. Penn State Cooperative Wetlands Center; EPA.
#' O'Connell, T. J., L. E. Jackson, and R. P. Brooks. 2000. Bird guilds as indicators of ecological condition in the central Appalachians. Ecological Applications 10:1706-1721.
#' Ladin, H., Z. S., and W. G. Shriver. 2016. Using regional bird community dynamics to evaluate ecological integrity within national parks. Ecosphere 7.
#' 
#' @export

########################


setGeneric(name="BCIMap",function(object,Dir,years=NA,points=NA,color=NA,APIkey=NA,...){standardGeneric("BCIMap")}, signature="object")


############ need to add list method I

setMethod(f="BCIMap", signature=c(object="list"),
          function(object,Dir,years,points,color,APIkey,...) {
            
            return(lapply(object=object,Dir=Dir,years=years,color=color,APIkey=APIkey, FUN=BCIMap,...)
                   )
          }
  )


setMethod(f="BCIMap", signature=c(object="NCRNbirds"),
          function(object,Dir,years,color,APIkey,...){
            

            bci.df<-BCI(object=object)
            
            #add coords
            point.data<-object@Points
            coords<-unique(point.data[,c("Point_Name","LongName","Admin_Unit_Code","Latitude","Longitude")])
            
            bci.coords<-merge(bci.df, coords, by=c("Point_Name","Admin_Unit_Code"),all.x=TRUE)
            
            #create map of abundance among points
            mean.x<-mean(bci.coords$Longitude, na.rm=TRUE)
            mean.y<-mean(bci.coords$Latitude, na.rm=TRUE)
            
            
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
            
            #make map with continuous BCI
            bci.map <- ggmap(park.map, darken = c(0.3, "white"))+
              #geom_polygon(data=park.map.df, aes(x=long, y=lat, group=group),fill="darkgreen",color="black",alpha=0.6)+
              #geom_point(data=Abun.Table, shape=1,aes(x=Longitude,y=Latitude, size=Predicted+SE),color="red",alpha=0.9)+
              geom_point(data=bci.coords, size=1.5, aes(x=Longitude,y=Latitude, color=BCI), alpha=0.6)+
              scale_color_gradient(low = "white", high = color,na.value="gray",
                                  limits=c(0, max(bci.coords$BCI, na.rm=TRUE)),
                                  breaks=c(seq(0,max(bci.coords$BCI,na.rm=TRUE), length.out=5),0))+
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
              ggtitle(paste(strwrap(paste("Map of species richness in ", object@LongName," from ", min(object@Visits$Year,na.rm=TRUE), " to ", max(object@Visits$Year, na.rm=TRUE)), width=70),collapse = "\n"))
            bci.map
            
            
            #make BCI_Category a factor
            bci.coords$BCI_Category<-factor(bci.coords$BCI_Category, levels=rev(c("Low Integrity","Medium Integrity","High Integrity","Highest Integrity")))
            
unique(bci.coords$BCI_Category)
            
            mycolors<-c("seagreen4","yellow","orange","red")
            
            #make map with categorical BCI
            bci.map.cat <- ggmap(park.map, darken = c(0.3, "white"))+
              geom_point(data=bci.coords, size=1.5, aes(x=Longitude,y=Latitude, color=BCI_Category), alpha=0.6)+
              scale_color_manual(values=mycolors)+
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
              ggtitle(paste(strwrap(paste("Map of bird community index categories ", object@LongName," from ", min(object@Visits$Year,na.rm=TRUE), " to ", max(object@Visits$Year, na.rm=TRUE)), width=70),collapse = "\n"))
            bci.map.cat
            
            
            ###############
            response<-readline(prompt="Would you like to your maps? (y/n)")
                     ifelse(response=="y",
                            {
                              dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/"))
                            
                              #save map
                              ggsave(bci.map, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(object@ParkCode, "MapBirdCommunityIndex",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=10, units="in", dpi=300)
                              
                              ggsave(bci.map.cat, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(object@ParkCode, "MapBirdCommunityIndex_Categories",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=10, units="in", dpi=300)
                              
                         #return map
                         return(list(bci.map,bci.map.cat))     

                            },
                         #return map
                         return(list(bci.map,bci.map.cat))     
                     )
          }
)
            