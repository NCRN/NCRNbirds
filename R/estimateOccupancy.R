#' @include NCRNbirds_Class_def.R occuBirds.R getBirdNames.R getParkNames.R
#' 
#' @title estimateOccupancy
#' 
#' @description Fit hierarchical occupancy models to count data (MacKenzie et al. 2002) with the \code{occu()} function (Fiske et al. 2011).
#' 
#' @importFrom unmarked occu unmarkedFrameOccu predict 
#' @importFrom ggplot2 ggplot ggsave geom_smooth geom_path geom_point geom_errorbar element_rect coord_cartesian ggtitle
#'
#' @param object An NCRNbirds object or a list of such objects.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been visited is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param band A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param visits A length 1 numeric vector. Will only use data from points with that number of visits.
#' @param site.covs A list of desired site-level covariates that can be included within the estimation of probability of site occupancy.
#' @param obs.covs A list of desired observation-level covariates that can be included within the detection probability estimation process of the occupancy model.
#' @param Table Logical (default = TRUE). Print table of results to the console.
#' @param Figure Logical (default = TRUE). Show figure of trend in occupancy over time.
#' @param color A character input of for determining color in figure (e.g., "red").
#' @param ... Additonal arguments passed to \code{estimateOccupancy}.
#' 
#' @details This function allows users to fit occupancy models and select site-level and observation-level covariates (MacKenzie et al. 2002) to data in an NCRNbirds object or a list of such objects. The data is first extracted from the object(s) and is then fed to the \code{\link[unmarked]{occu}} function and the resutls of the analysis are returned. 
#' 
#' @references MacKenzie, D. I., J. D. Nichols, G. B. Lachman, S. Droege, J. Andrew Royle, and C. A. Langtimm. 2002. Estimating site occupancy rates when detection probabilities are less than one. Ecology 83:2248-2255.
#'  @references Fiske, I., R. Chandler, and others. 2011. Unmarked: An R package for fitting hierarchical models of wildlife occurrence and abundance. Journal of Statistical Software 43:1-23. 
#'  
#' @export


########################


setGeneric(name="estimateOccupancy",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=NA,site.covs=NA,obs.covs=NA,Table=TRUE, Figure=TRUE, color=NA,...){standardGeneric("estimateOccupancy")}, signature="object")


############ need to add list method I

setMethod(f="estimateOccupancy", signature=c(object="list"),
          function(object,points,AOU,years,times,band,visits,site.covs,obs.covs,Table,Figure,color,...) {
            
            return(lapply(object=object, FUN=estimateOccupancy, points=points,AOU=AOU,years=years,times=times,band=band,
                           visits=visits,site.covs=site.covs, obs.covs=obs.covs,Table=Table,Figure=Figure,color=color,...)
                   )
          }
  )


setMethod(f="estimateOccupancy", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,times,band,visits,site.covs,obs.covs,Table,Figure,color,...){
            
            #load park unit data
            parkData<-object
            
            #check of object is a network or park
            dataType<-ifelse(parkData@Network==parkData@ParkCode,"Network","Park")
            
            #run occupancy model
            mod<-occuBirds(object=parkData, AOU=AOU, years=years,site.covs=site.covs, obs.covs=obs.covs, band=band)
            
            #get predicted estimates for Occupancy and Detection Probability
            mod.occu.predict<-predict(mod, type="state",appendData=TRUE)
            
            #take desired columns
            ifelse(dataType=="Network",
              {
              mod.occu.predict.out.1<-unique(mod.occu.predict[,c("Year","Predicted","SE","lower","upper")])
              mod.occu.predict.out.1$Admin_Unit_Code<-parkData@Network
              },
              mod.occu.predict.out.1<-unique(mod.occu.predict[,c("Admin_Unit_Code","Year","Predicted","SE","lower","upper")])
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
            mod.occu.predict.out<-mod.occu.predict.out.1[,c("Common_Name","AOU_Code","Park_Name","Admin_Unit_Code","Year","Predicted","SE","lower","upper")]
            
            #sort by Year
            mod.occu.predict.out<-mod.occu.predict.out[order(mod.occu.predict.out$Year, decreasing=FALSE),]
            
            Occu.Table<- mod.occu.predict.out
            
            #get predicted estimates of Detection Probability
            mod.det.predict<-predict(mod, type="det")
            
            #get mean detection probability
            mean.det<-mean(mod.det.predict$Predicted, na.rm=TRUE)
            mean.det.SE<-mean(mod.det.predict$SE, na.rm=TRUE)
            
            #Add mean detection probability to Occu.Table
            Occu.Table$DetectionProb<-mean.det
            Occu.Table$DetectionProb.SE<-mean.det.SE
            
            #add years if NA from function
            ifelse(is.na(years), years<-sort(unique(parkData@Visits$Year)), years<-years)
            
            color=ifelse(is.na(color),"black",color)
            
            #create plot of occupancy probability over years
            occuPlot<-ggplot(data=Occu.Table, aes(x=floor(as.numeric(as.character(Year))), y=Predicted))+
              geom_smooth(method="lm", color="white", fill=color, alpha=0.4)+
              geom_path(color=color, group=1, alpha=0.3)+
              geom_errorbar(aes(ymin=Predicted-SE, ymax=Predicted+SE),color=color,width=0)+
              geom_point(color=color)+
              theme(panel.background=element_rect(fill="white"),
                    panel.border=element_rect(color="black",fill="transparent"))+
              scale_x_continuous(breaks=c(as.numeric(as.character(Occu.Table$Year))),labels = unique(as.character(Occu.Table$Year)))+
              scale_y_continuous(breaks=seq(0,1,length.out=5), expand=c(0.1,0.1), limits=c(-10,10))+
              coord_cartesian(xlim=c(min(years),max(years)), ylim=c(0,1))+
              labs(x="Year", y="Estimated Probability of Site Occupancy")+
              ggtitle(paste(strwrap(paste("Estimated Prob. of Occupancy for the ", myBirdName, " in ", myParkName, " from ", min(years), " to ", max(years)), width=50),collapse = "\n"))
            occuPlot
            
            response<-readline(prompt="Would you care to save these results? (y/n)" )
            ifelse(response=="y",
                   {
                     dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/"))
                     
                     write.csv(Occu.Table, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(myParkName, myBirdName, "OccupancyTableByYear",Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                     
                     ggsave(occuPlot, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(myParkName, myBirdName, "OccupancyPlotByYear",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=5, height=5, units="in", dpi=300)
                     
                     #return table and fig
                     if(isTRUE(Table) & isFALSE(Figure)){
                       return(list(mod, Occu.Table))
                     }else{
                       if(isFALSE(Table) & isTRUE(Figure)){
                         return(list(mod, occuPlot))
                       }else{
                         if(isFALSE(Table) & isFALSE(Figure)){
                           return(mod)
                         }else{
                           return(list(mod, Occu.Table, occuPlot))
                         }}}
                   },
                   #return table and fig
                   if(isTRUE(Table) & isFALSE(Figure)){
                     return(list(mod, Occu.Table))
                   }else{
                     if(isFALSE(Table) & isTRUE(Figure)){
                       return(list(mod, occuPlot))
                     }else{
                       if(isFALSE(Table) & isFALSE(Figure)){
                         return(mod)
                       }else{
                         return(list(mod, Occu.Table, occuPlot))
                       }}}
            )
            
          
          }
)