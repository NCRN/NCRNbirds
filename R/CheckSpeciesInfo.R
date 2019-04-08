#' @include NCRNbirds_Class_def.R
#' 
#' @title CheckSpeciesInfo
#' 
#' @description Query for comparing species information within raw data (e.g., Common Name, Scientific Name, and 4-letter Alpha coes), with up-to-date taxonomic information to standardize data prior to analysis.
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param Dir A filepath directory to where the data resides.
#' @param ... Additonal arguments passed to \code{\link{CheckSpeciesInfo}}.
#' 
#' @details This function uses resulting data gathered after running the \code{\link{getUpdatedSpeciesInfo}} function to query existing taxonomic information within raw data, and identifies any species that have discrepancies .
#' 
#' @references Chesser, R. T., K. J. Burns, C. Cicero, J. L. Dunn, A. W. Kratter, I. J. Lovette, P. C. Rasmussen, J. V. Remsen, Jr., D. F. Stotz, B. M. Winger, and K. Winker. 2018. Check-list of North American Birds (online). American Ornithological Society. http://checklist.aou.org/taxa
#' @references Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R. Hudson. 2018. North American Breeding Bird Survey Dataset 1966 - 2017, version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research Center. https://doi.org/10.5066/F76972V8. 
#' @references Panjabi, A. O., P. J. Blancher, W. E. Easton, J. C. Stanton, D. W. Demarest, R. Dettmers, K. V. Rosenberg, P. in Flight Science Committee, and others. 2017. The partners in flight handbook on species assessment version 2017. Partners in Flight.

#' 
#' @export

########################


setGeneric(name="CheckSpeciesInfo",function(object,Dir,...){standardGeneric("CheckSpeciesInfo")}, signature="object")


############ need to add list method I

setMethod(f="CheckSpeciesInfo", signature=c(object="list"),
          function(object,Dir,...) {
            
            return(lapply(object=object,Dir=Dir, FUN=CheckSpeciesInfo...)
                   )
          }
  )


setMethod(f="CheckSpeciesInfo", signature=c(object="NCRNbirds"),
          function(object,Dir,...){
            
            data=object@Birds
            
            #read in updated species info
            species.updated<-read.csv(paste(Dir,"AOS_Codes","SpeciesList_out.csv",sep="/"),as.is=TRUE)  
            
            #simplify data (get list of unique species)
            unique.birds<-unique(data[,c("AOU_Code","Common_Name")])
            
            #remove blanks and any "unidentified" codes
            unique.birds<-subset(unique.birds, AOU_Code !="")
            
            #get list of Unidentified spp.
            UnidentifiedList<-unique.birds[grep("Unidentified", unique.birds$Common_Name),]
            
            #species to remove
            removeList<-UnidentifiedList$AOU_Code
            
            unique.birds<-subset(unique.birds, ! AOU_Code %in% removeList)
            
            #merge unique.birds with species.updated
            unique.birds.merge<-merge(unique.birds, species.updated, by=c("Common_Name","AOU_Code"),all.x=TRUE)
            
            #find out which species need updating information
            NA.species.birds<-subset(unique.birds.merge, is.na(Scientific_Name))
            NA.species.birds<-data.frame(Common_Name=NA.species.birds$Common_Name, AOU_Code=NA.species.birds$AOU_Code)
            
            #get updated names and codes
            NA.speciesList<-as.character(NA.species.birds$Common_Name)
            
            #save species list delimited by commas for facilitating entering for update
            NA.speciesList.comma<-paste(NA.speciesList, collapse=", ")

            #notify user about species that have issues
            NA.speciesListCarriageReturn<-paste(NA.speciesList, collapse="\n")
            
            ifelse(identical(NA.speciesList, character(0)), 
                   {
                     return(
                     message("Hooray! We found no issues with species names in the data.")
                     )
                   },
                   
                   {
                    message(cat("We have found issues with the following species:\n", NA.speciesListCarriageReturn,"\nPlease refer to the current American Ornithologists' Union Check-list for North American Birds: https://doi.org/10.1642/AUK-18-62.1",sep=""))
            
                    response<-readline(prompt="Would you like to save list of problematic species? (y/n)")
                    ifelse(response=="y",
                   {
                     suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))
                     
                     write.csv(NA.species.birds, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_Species_Problems",object@ParkCode,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                     
                     #return table
                     return({
                       message("The file has been saved.")
                       NA.speciesList.comma
                     })
                   },
                   #return table
                   return()
            )
        }
      )
            
            
    }
)
            