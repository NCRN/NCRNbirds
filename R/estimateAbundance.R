#' @include NCRNbirds_Class_def.R nmixBirds_zl.R getParkData.R getBirdNames.R getParkNames.R
#' 
#' @title estimateAbundance
#' 
#' @description Peforms trend analysis on bird data using N-mixture models (Royal 2004) form the unmarked package, and exports tables and figures.
#' 
#' @importFrom unmarked predict 
#' @importFrom ggplot2 ggplot ggsave geom_smooth geom_path geom_point geom_errorbar element_rect coord_cartesian ggtitle
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
#' @param Table Logical (default = TRUE). Print table of results to the console.
#' @param Figure Logical (default = TRUE). Show figure of trend in abundance over time.
#' @param color A character input of a valid color name for determining color in figure (e.g., "red").
#' @param ... Additonal arguments passed to \code{\link{CountXVisit}} and from there to \code{\link{getBirds}}.
#' 
#' @details This function is a wrapper for the \code{\link[NCRNbirds]{nmixBirds_zl}} for fitting a N-mixture model (Royal 2004, Kery et al. 2005) to data in an NCRNbirds object or a list of such objects. The data is first extracted from the object(s) and then fed to the \code{\link[unmarked]{pcount}} function. Tables and figures of resutls of the analysis are returned, and may be saved. 
#' 
#' @references Kery, M. Royle, J.A. and Schmid. 2005. Modeling avian abundance from replicated counts using binomail mixutre models. Ecological Applications. 15: 1450-1461. 
#' @references Royle, J. A.  2004. N-Mixture models for estimating population size from spatially replicated counts. Biometrics 60: 108-115.
#' 
#' @export



########################


setGeneric(name="estimateAbundance",function(object,points=NA,AOU=NA,years=NA,times=NA,band=1,visits=NA,site.covs=NA,obs.covs=NA,mixture="P",Table=TRUE, Figure=TRUE, color=NA,...){standardGeneric("estimateAbundance")}, signature="object")


############ need to add list method I

setMethod(f="estimateAbundance", signature=c(object="list"),
          function(object,points,AOU,years,times,band,visits,site.covs,obs.covs,mixture,Table,Figure,color,...) {
            
            return(lapply(object=object, FUN=estimateAbundance, points=points,AOU=AOU,years=years,times=times,band=band,
                           visits=visits,site.covs=site.covs, obs.covs=obs.covs,mixture=mixture,Table=Table,Figure=Figure,color=color,...)
                   )
          }
  )


setMethod(f="estimateAbundance", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,times,band,visits,site.covs,obs.covs,mixture,Table,Figure,color,...){
            
            #load park unit data
            parkData<-object
            
            #check of object is a network or park
            dataType<-ifelse(parkData@Network==parkData@ParkCode,"Network","Park")

            #run abundance model
            mod<-nmixBirds_zl(object=parkData, AOU=AOU,years=years,times=times, band=band, visits=visits, site.covs=site.covs, obs.covs=obs.covs, mixture=mixture)
            
            #get predicted estimates for Abundance and Detection Probability
            mod.abun.predict<-predict(mod, type="state",appendData=TRUE)
            
            #take desired columns
            ifelse(dataType=="Network",
                   {
                     mod.abun.predict.out.1<-unique(mod.abun.predict[,c("Year","Predicted","SE","lower","upper")])
                     mod.abun.predict.out.1$Admin_Unit_Code<-parkData@Network
                   },
                   mod.abun.predict.out.1<-unique(mod.abun.predict[,c("Admin_Unit_Code","Year","Predicted","SE","lower","upper")])
            )
            
            #add species AOU_Code
            mod.abun.predict.out.1$AOU_Code<-AOU
            
            #add common name
            myBirdName<-getBirdNames(object=parkData, names=AOU)
            
            mod.abun.predict.out.1$Common_Name<- myBirdName
            
            #add Long Park Unit name and Common Name of bird species
            myParkName<-getParkNames(parkData, name.class="long")
            
            mod.abun.predict.out.1$Park_Name<-myParkName
            
            #reorder columns
            mod.abun.predict.out<-mod.abun.predict.out.1[,c("Common_Name","AOU_Code","Park_Name","Admin_Unit_Code","Year","Predicted","SE","lower","upper")]
            
            #sort by Year
            mod.abun.predict.out<-mod.abun.predict.out[order(mod.abun.predict.out$Year, decreasing=FALSE),]
            
            Abun.Table<- mod.abun.predict.out
            
            #get predicted estimates for Abundance and Detection Probability
            mod.det.predict<-predict(mod, type="det",appendData=TRUE)
            
            #get mean detection probability
            mean.det<-mean(mod.det.predict$Predicted)
            mean.det.SE<-mean(mod.det.predict$SE)
            
            #Add mean detection probability to Abun.Table
            Abun.Table$DetectionProb<-mean.det
            Abun.Table$DetectionProb.SE<-mean.det.SE
            
            #add years if NA from function
            ifelse(is.na(years), years<-sort(unique(parkData@Visits$Year)), years<-years)
            
            color=ifelse(is.na(color),"black",color)
            
            #create plot of abundance over years
            abunPlot<-ggplot(data=Abun.Table, aes(x=floor(as.numeric(as.character(Year))), y=Predicted))+
              geom_smooth(method="lm", color="white", fill=color, alpha=0.4)+
              geom_path(color=color, group=1, alpha=0.3)+
              geom_errorbar(aes(ymin=Predicted-SE, ymax=Predicted+SE),color=color,width=0)+
              geom_point(color=color)+
              theme(panel.background=element_rect(fill="white"),
                    panel.border=element_rect(color="black",fill="transparent"))+
              scale_x_continuous(breaks=c(as.numeric(as.character(Abun.Table$Year))),labels = unique(as.character(Abun.Table$Year)))+
              scale_y_continuous(breaks=seq(0,round(max(Abun.Table$Predicted,na.rm=TRUE),0),length.out=5), expand=c(0.1,0.1), limits=c(-300,300))+
              coord_cartesian(xlim=c(min(years),max(years)), ylim=c(0,max(Abun.Table$Predicted,na.rm=TRUE)))+
              labs(x="Year", y="Estimated Abundance (birds per point)")+
              ggtitle(paste(strwrap(paste("Estimated Abundance for the ", myBirdName, " in ", myParkName, " from ", min(years), " to ", max(years)), width=50),collapse = "\n"))
            abunPlot
           
           parkCodes<-paste(unique(object@ParkCode),collapse="_")
           
            response<-readline(prompt="Would you like to save these results? (y/n)" )
            ifelse(response=="y",
                   {
                     dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/"))
                     
                     write.csv(Abun.Table, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(parkCodes, myBirdName, "AbundanceTableByYear",Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                     
                     ggsave(abunPlot, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste(parkCodes, myBirdName, "AbundancePlotByYear",Sys.Date(), sep="_"),".png", sep=""),sep="/"), width=5, height=5, units="in", dpi=300)
                     
                     #return table and fig
                     if(isTRUE(Table) & isFALSE(Figure)){
                       return(list(mod, Abun.Table))
                     }else{
                       if(isFALSE(Table) & isTRUE(Figure)){
                         return(list(mod, abunPlot))
                       }else{
                         if(isFALSE(Table) & isFALSE(Figure)){
                           return(mod)
                         }else{
                           return(list(mod, Abun.Table, abunPlot))
                         }}}
                   },
                   #return table and fig
                   if(isTRUE(Table) & isFALSE(Figure)){
                     return(list(mod, Abun.Table))
                   }else{
                     if(isFALSE(Table) & isTRUE(Figure)){
                       return(list(mod, abunPlot))
                     }else{
                       if(isFALSE(Table) & isFALSE(Figure)){
                         return(mod)
                       }else{
                         return(list(mod, Abun.Table, abunPlot))
                       }}})
          
            # #return table and fig
            # if(isTRUE(Table) & isFALSE(Figure)){
            #   return(list(mod, Abun.Table))
            # }else{
            #   if(isFALSE(Table) & isTRUE(Figure)){
            #     return(list(mod, abunPlot))
            #   }else{
            #     if(isFALSE(Table) & isFALSE(Figure)){
            #       return(mod)
            #     }else{
            #       return(list(mod, Abun.Table, abunPlot))
            #     }}}


          }
)