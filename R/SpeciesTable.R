#' @include NCRNbirds_Class_def.R
#' 
#' @title SpeciesTable
#' 
#' @description Generate a table of species with corresponding taxonomic and conservation status information.
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param ... Additonal arguments passed to \code{\link{SpeciesTable}}.
#' 
#' @details This function generates a table (and .csv file to save, if desired) of all unique species within the data provided by the object. The table includes up-to-date information on common names, scientific names, and 4-letter alpha codes from \code{\link{getUpdatedSpeciesInfo}} function, as well as conservation status information for each species.
#' 
#' @export


########################


setGeneric(name="SpeciesTable",function(object,Dir,...){standardGeneric("SpeciesTable")}, signature="object")


############ need to add list method I

setMethod(f="SpeciesTable", signature=c(object="list"),
          function(object,Dir,...) {
            
            return(lapply(object=object,Dir=Dir, FUN=SpeciesTable...)
                   )
          }
  )


setMethod(f="SpeciesTable", signature=c(object="NCRNbirds"),
          function(object,Dir,...){
            
            data=object@Birds
            
            #read in updated species info
            species.updated<-read.csv(paste(Dir,"AOS_Codes","SpeciesList_out.csv",sep="/"),as.is=TRUE)  
            
            species.updated.sub<-species.updated[,c("Common_Name","Scientific_Name","AOU_Code","Continental_Concern","IUCN_Red_List_2016")]
            
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
            birds.merge<-merge(unique.birds, species.updated.sub, by=c("Common_Name","AOU_Code"),all.x=TRUE)
           #reorder columns
            birds.merge<-birds.merge[,c("Common_Name","Scientific_Name","AOU_Code","Continental_Concern","IUCN_Red_List_2016")]
            
            #get info for creating file names
            parkCodes<-paste(unique(object@ParkCode),collapse="_")
            
            response<-readline(prompt="Would you like to save these results? (y/n)")
            ifelse(response=="y",
                   {
                     suppressWarnings(dir.create(path=paste(Dir, paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))
                     
                     write.csv(birds.merge, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_Species_List",parkCodes,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                     
                     #return table
                     return({
                       print(birds.merge)
                       birds.merge
                       })
                   },
                   #return table
                   return({
                     print(birds.merge)
                     birds.merge
                   })
            )
            
            


          }
)
            