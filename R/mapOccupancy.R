#' @include NCRNbirds_Class_def.R nmixBirds_zl.R getParkData.R getBirdNames.R getParkNames.R
#' 
#' @title mapOccupancy
#' 
#' @description Generates map of probability of site occupancy estimates at the point-level that pool data among years and control for detection probability.
#' 
#' @importFrom unmarked predict 
#' @importFrom ggmap ggmap register_google get_googlemap get_map ggmap_credentials showing_key 
#' @importFrom ggplot2 ggplot ggsave geom_point geom_errorbar element_rect ggtitle coord_flip scale_size_continuous
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param times A numeric vector of length 1 passed on to \code{\link{CountXVisit}} and from there to \code{\link{getVisits}}. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param band A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits A length 1 numeric vector. Will only use data from points with that number of visits.
#' @param mixture  Defaults to "P". One of either "P", "NB", or "ZIP". Indicates which latent abundace distibution to use, either Poisson, Negative Binomial or Zero Inflated Poinsson.  Passed on to the \code{mixture} argumnet of \code{pcount} in the \code{unmarked} package. 
#' @param site.covs A list of desired site-level covariates that can be included within the abundance estimation process of the N-mixture model.
#' @param obs.covs A list of desired observation-level covariates that can be included within the detection probability estimation process of the N-mixture model.
#' @param Table Logical (default=TRUE). Generates table to display and save, if desired.
#' @param Figure Logical (default=TRUE). Generates a figure to display and save, if desired.
#' @param APIkey A character string of a Google API key used for generating maps with Google basemaps. If no APIkey is provided, basemaps default to stamen map.
#' @param color A character string naming a valid color for figures. Default is grayscale.
#' @param ... Additonal arguments passed to \code{\link{CountXVisit}} and from there to \code{\link{getBirds}}.
#' 
#' @details This function generates a map of point-level abundance estimates following estimating abundance by fitting a N-mixture model (Royal 2004, Kery et al. 2005) to data in an NCRNbirds object or a list of such objects. The data is first extracted from the object(s) using the \code{\link{CountXVisit}} and \code{\link{CovsXVisit}} funcitons. The data is then fed to the \code{\link[unmarked]{pcount}} function and the resutls of the analysis are returned. 
#' 
#' @references Kery, M. Royle, J.A. and Schmid. 2005. Modeling avian abundance from replicated counts using binomail mixutre models. Ecological Applications. 15: 1450-1461. 
#' @references Royle, J. A.  2004. N-Mixture models for estimating population size from spatially replicated counts. Biometrics 60: 108-115.
#' 
#' @export


########################


setGeneric(name="mapOccupancy",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=NA,mixture="P",site.covs="Point_Name",obs.covs=NA,Table=TRUE, Figure=TRUE,color=NA,APIkey=NA,...){standardGeneric("mapOccupancy")}, signature="object")


############ need to add list method I

setMethod(f="mapOccupancy", signature=c(object="list"),
          function(object,points,AOU,years,times,band,visits,mixture,site.covs,obs.covs,Table,Figure,APIkey,...) {
            
            return(lapply(X=object, FUN=mapOccupancy, points=points,AOU=AOU,years=years,times=times,band=band,
                           visits=visits,mixture=mixture,site.covs=site.covs, obs.covs=obs.covs,Table=Table,Figure=Figure,color=color,APIkey=APIkey,...)
                   )
          }
  )


setMethod(f="mapOccupancy", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,times,band,visits,mixture,site.covs,obs.covs,Table,Figure,color,APIkey,...){
            
            require(ggmap)
            
            #load park unit data
            parkData<-object
            
            #check of object is a network or park
            dataType<-ifelse(parkData@Network==parkData@ParkCode,"Network","Park")
            
            #get parkCode
            ParkCode<- unique(parkData@ParkCode)
            
            #run occupancy model
            mod<-occuBirds(object=parkData,AOU=AOU,years=years,times=times,band=band,visits=visits,mixture=mixture,site.covs=site.covs, obs.covs=obs.covs)
            
            #get predicted estimates for Occupancy and Detection Probability
            mod.occu.predict<-predict(mod, type="state",appendData=TRUE)
            
            #take desired columns
            ifelse(dataType=="Network",
                   {
                     mod.occu.predict.out.1<-unique(mod.occu.predict[,c("Point_Name","Predicted","SE","lower","upper")])
                     mod.occu.predict.out.1$Admin_Unit_Code<-parkData@Network
                   },
                   mod.occu.predict.out.1<-unique(mod.occu.predict[,c("Admin_Unit_Code","Point_Name","Predicted","SE","lower","upper")])
            )
            
            #add species AOU_Code
            mod.occu.predict.out.1$AOU_Code<-AOU
            
            #add common name
            myBirdName<-getBirdNames(object=parkData, names=AOU)
            
            mod.occu.predict.out.1$Common_Name<- myBirdName
            
            #add Long Park Unit name and Common Name of bird species
            myParkName<-getParkNames(parkData, name.class="long")
            
            mod.occu.predict.out.1$Park_Name<-myParkName
            
            #reorder columns
            mod.occu.predict.out<-mod.occu.predict.out.1[,c("Common_Name","AOU_Code","Park_Name","Admin_Unit_Code","Point_Name","Predicted","SE","lower","upper")]
            
            #add coords
            point.data<-parkData@Points
            coords<-unique(point.data[,c("Point_Name","Latitude","Longitude")])
            mod.occu.predict.out<-merge(mod.occu.predict.out, coords, by="Point_Name",all.x=TRUE)
            
            Occu.Table<- mod.occu.predict.out
            
            #get predicted estimates for Occupancy and Detection Probability
            mod.det.predict<-predict(mod, type="det",appendData=TRUE)
            
            #get mean detection probability
            mean.det<-mean(mod.det.predict$Predicted)
            mean.det.SE<-mean(mod.det.predict$SE)
            
            #Add mean detection probability to Occu.Table
            Occu.Table$DetectionProb<-mean.det
            Occu.Table$DetectionProb.SE<-mean.det.SE
            
            #sort Occu.Table by highest to lowest occupancy
            Occu.Table<-Occu.Table[order(Occu.Table$Predicted,decreasing=TRUE),]
            
            #reorder Point_Name factor levels
            Occu.Table$Point_Name<-factor(Occu.Table$Point_Name, levels=rev(unique(as.character(Occu.Table$Point_Name))))
            
            color=ifelse(is.na(color),"black",color)
            
            #add years if NA from function
            ifelse(is.na(years), years<-sort(unique(parkData@Visits$Year)), years<-years)
            
            #create plot of occupancy over years
            occuPlot<-ggplot(data=Occu.Table, aes(x=Point_Name, y=Predicted))+
              #geom_smooth(method="lm", color="white", fill=color, alpha=0.4)+
              #geom_path(color=color, group=1, alpha=0.3)+
              geom_errorbar(aes(ymin=Predicted-SE, ymax=Predicted+SE),color=color,width=0)+
              geom_point(color=color)+
              theme(panel.background=element_rect(fill="white"),
                    panel.border=element_rect(color="black",fill="transparent"),
                    title=element_text(size=14))+
              #scale_y_continuous(breaks=seq(0,round(max(Occu.Table$Predicted,na.rm=TRUE),0),length.out=5), expand=c(0.1,0.1), limits=c(-1,300))+
              #coord_cartesian(ylim=c(0,max(Occu.Table$Predicted,na.rm=TRUE)))+
              labs(x="Sampling Point Name", y="Estimated Probability of Site Occupancy")+
              coord_flip()+
              ggtitle(paste(strwrap(paste("Estimated Occupancy for the ", myBirdName, " in ", myParkName, " from ", min(years), " to ", max(years)), width=70),collapse = "\n"))
            #occuPlot
            
            #create map of occupancy among points
            mean.x<-mean(Occu.Table$Longitude, na.rm=TRUE)
            mean.y<-mean(Occu.Table$Latitude, na.rm=TRUE)
            
            
            parkZoom<-switch(ParkCode,
                             "NCRN"=7,
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
                                        style = 'feature:administrative.country|element:labels|visibility:off')
            }
            
            park.map.out <- ggmap(park.map, darken = c(0.2, "white"))+
              #geom_polygon(data=park.map.df, aes(x=long, y=lat, group=group),fill="darkgreen",color="black",alpha=0.6)+
              #geom_point(data=Occu.Table, shape=1,aes(x=Longitude,y=Latitude, size=Predicted+SE),color="red",alpha=0.9)+
              geom_point(data=Occu.Table, shape=21, aes(x=Longitude,y=Latitude, size=Predicted), fill=alpha(color,0.5), color=color)+
              scale_size_continuous(range = c(1, 4))+
              theme(panel.border=element_rect(fill="transparent",color="black"))+
              theme(panel.background=element_rect(fill='white',color="black"))+
              theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
              theme(axis.line=element_line(color="black"))+
              theme(axis.text.x = element_text(size=10, color="black"),
                    axis.text.y = element_text(size=10, color="black"),
                    axis.title.x = element_text(size=11, hjust=0.5, vjust=1.9),
                    axis.title.y = element_text(angle = 90, vjust=1.2, size=11),
                    title=element_text(size=14))+
              labs(x="Longitude",y="Latitude", size=paste("Occupancy"))+
              ggtitle(paste(strwrap(paste("Map of Point-Level Mean Occupancy for the ", myBirdName, " in ", myParkName," from ", min(years), " to ", max(years)), width=70),collapse = "\n"))
            #park.map.out
            
            response<-readline(prompt="Would you like to save these results? (y/n)")
            ifelse(response=="y",
                   {
                     suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))

                     write.csv(Occu.Table, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(myParkName, myBirdName, "OccupancyTableByPoint",Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)

                     ggsave(occuPlot, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(myParkName, myBirdName, "OccupancyPlotByPoint",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=10, units="in", dpi=300)

                     ggsave(park.map.out, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(myParkName, myBirdName, "mapOccupancy",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=10, height=10, units="in", dpi=300)


                     #return table and fig
                     #return table and fig
                     if(isTRUE(Table) & isFALSE(Figure)){
                       return(list(mod, Occu.Table, park.map.out))
                     }else{
                       if(isFALSE(Table) & isTRUE(Figure)){
                         return(list(mod, occuPlot, park.map.out))
                       }else{
                         if(isFALSE(Table) & isFALSE(Figure)){
                           return(mod)
                         }else{
                           return(list(mod, Occu.Table, occuPlot, park.map.out))
                         }}}
                   },
                   #return table and fig
                   if(isTRUE(Table) & isFALSE(Figure)){
                     return(list(mod, Occu.Table, park.map.out))
                   }else{
                     if(isFALSE(Table) & isTRUE(Figure)){
                       return(list(mod, occuPlot, park.map.out))
                     }else{
                       if(isFALSE(Table) & isFALSE(Figure)){
                         return(mod)
                       }else{
                         return(list(mod, Occu.Table, occuPlot, park.map.out))
                       }}}
            )
          
            


          }
)