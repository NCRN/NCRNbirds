#' @include NCRNbirds_Class_def.R
#' 
#' @title UpdateSpeciesInfo
#' 
#' @description Update count data by finding and replacing inaccurate taxonomic names and species codes. 
#' 
#' @importFrom utils combn
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param Dir A filepath directory to where the data resides.
#' @param ... Additonal arguments passed to \code{\link{UpdateSpeciesInfo}}.
#' 
#' @details This function queries the user to input desired list of species common names (as currently exisiting in the data) for updating and reconciliation with updated species taxonomic information and conservation status gathered from \code{\link{getUpdatedSpeciesInfo}}. It saves new data, and archives original data.
#' 
#' @export


########################


setGeneric(name="UpdateSpeciesInfo",function(object,Dir,...){standardGeneric("UpdateSpeciesInfo")}, signature="object")


############ need to add list method I

setMethod(f="UpdateSpeciesInfo", signature=c(object="list"),
          function(object,Dir,...) {
            
            return(lapply(object=object,Dir=Dir, FUN=UpdateSpeciesInfo...)
                   )
          }
  )


setMethod(f="UpdateSpeciesInfo", signature=c(object="NCRNbirds"),
          function(object,Dir,...){
            
            data=object@Birds
            
            #condense unique birds within data
            unique.birds<-unique(data[,c("Common_Name","Scientific_Name","AOU_Code")])
            
            #remove blanks and any "unidentified" codes
            unique.birds<-subset(unique.birds, AOU_Code !="")
            unique.birds$Common_Name<-trimws(unique.birds$Common_Name)
            
            #prompt user to enter species common names that are desired to update
            response<-readline(prompt=cat("Please enter common name(s) for species you want to\nupdate (e.g., Eastern Tufted Titmouse, Solitary Vireo,...):  "))
            #set up names for subsetting
            splitNames<-trimws(unlist(strsplit(response, split=",")))
            splitNamesList<-c(combn(splitNames,m=length(splitNames),simplify=TRUE))
            
            #get old AOU_Codes
            speciesFix.df<-subset(unique.birds, Common_Name %in% splitNamesList)
            
            #make sure order of species is correct (as entered by user), use match() to sort data.frame by list
            speciesFix.df<-speciesFix.df[match(splitNamesList, speciesFix.df$Common_Name),]
            
            colnames(speciesFix.df)<-c("Common_Name_old","Scientific_Name_old","AOU_Code_old")

            #prompt user to input UPDATED common names to replace the old versions.
            response2<-readline(prompt = cat("Now enter up-to-date common name(s) for species (check here: https://www.birdpop.org)\nIMPORTANT: enter names in the same order as previous response:  "))
            
            #set up names for subsetting
            splitNames2<-trimws(unlist(strsplit(response2, split=",")))
            splitNames2List<-c(combn(splitNames2,m=length(splitNames2),simplify=TRUE))
            
            #add to speciesFix.df
            speciesFix.df$Common_Name<-splitNames2List
            
            #read in updated species info
            species.updated<-read.csv(paste(Dir,"AOS_Codes","SpeciesList_out.csv",sep="/"),as.is=TRUE)  
            
            #condense unique birds within data
            species.updated<-unique(species.updated[,c("Common_Name","Scientific_Name","AOU_Code")])
            
            #merge to get new Scientific_Name and AOU_Code
            speciesFix.df<-merge(speciesFix.df, species.updated, by="Common_Name",all.x=TRUE)
            
            temp.object=object
            
            #loop through data and update spcies Common names, Scientific names and AOU_Code
            for(i in 1:length(splitNames)){
              old.common.name<-speciesFix.df$Common_Name_old[i]
              old.sci.name<-speciesFix.df$Scientific_Name_old[i]
              old.aou.code<-speciesFix.df$AOU_Code_old[i]
              
              new.common.name<-speciesFix.df$Common_Name[i]
              new.sci.name<-speciesFix.df$Scientific_Name[i]
              new.aou.code<-speciesFix.df$AOU_Code[i]
              
              #find and replace names within object Data
              temp.object@Birds$Common_Name<-gsub(old.common.name, new.common.name, temp.object@Birds$Common_Name)
              temp.object@Birds$Scientific_Name<-gsub(old.sci.name, new.sci.name, temp.object@Birds$Scientific_Name)
              temp.object@Birds$AOU_Code<-gsub(old.aou.code, new.aou.code, temp.object@Birds$AOU_Code)
            }
            
            #check output
            #species.table.out<-SpeciesTable(object=temp.object,Dir=Dir)
            
            #generate updated data files (.csv)
            FieldData<-temp.object@Birds
            Points<-temp.object@Points
            Visits<-temp.object@Visits
            
            response<-readline(prompt="Would you like to save the updated data? (y/n)")
            ifelse(response=="y",
                   {
                     #rename original FieldData.csv as FieldData_original.csv
                   
                     file.rename(from=paste(Dir,"FieldData.csv",sep="/"), paste(Dir, paste("FieldData_original_archived_on_",Sys.Date(),".csv",sep=""),sep="/"))
                    
                     write.csv(FieldData, file=paste(Dir, "FieldData.csv",sep="/"), row.names=FALSE)
                     #write.csv(Points, file=paste(Dir, "Points.csv",sep="/"), row.names=FALSE)
                     #write.csv(Visits, file=paste(Dir,"Visits.csv",sep="/"), row.names=FALSE)
                     
                  
                     #return message
                     return(message("The updated data has been saved."))
                   },
                   #return nothing
                   return()
            )
          }
)
            