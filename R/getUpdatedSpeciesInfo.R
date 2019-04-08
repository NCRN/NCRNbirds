#' @include NCRNbirds_Class_def.R
#' 
#' @title getUpdatedSpeciesInfo
#' 
#' @description Gathers and saves up-to-date taxonomic and conservation status information on species to be used in updating species names and 4-letter codes within data, and generating tables.
#' 
#' @importFrom utils download.file unzip read.fwf
#' @importFrom foreign read.dbf
#' 
#' @param Dir A filepath directory to where the data resides.
#' 
#' @details This function navigates to existing URLs on the internet to download up-to-date taxonomic and conservation status information on species from Breeding Bird Survey data \link{ftp://ftpext.usgs.gov}, The Institute of Bird Populations \link{https://www.birdpop.org}, and Partners in Flight Bird Conservancy \link{http://pif.birdconservancy.org}. These data are then compiled and saved as a .csv file for future use in data anlayses and presentation.
#' 
#' @export


########################

getUpdatedSpeciesInfo<-function(Dir,...){
            
            #create directory for saving updated species info
            message("Creating new folder to save updated species info.")
            suppressWarnings(dir.create(paste(Dir,"AOS_Codes",sep="/")))
            
            #download .zipped DBF file of up-to-date bird codes
            message("Downloading latest AOS codes from The Institute of Bird Populations (https://www.birdpop.org)")
            URL <- "https://www.birdpop.org/docs/misc/List18.zip"
            download.file(url=URL, destfile=paste(Dir,"AOS_Codes","List18.zip",sep="/"))
            
            #unzip AOS codes
            message("Unzipping files.")
            zipF<-paste(Dir,"AOS_Codes","List18.zip",sep="/")
            outDir<-paste(Dir,"AOS_Codes",sep="/")
            unzip(zipF,exdir=outDir)  # unzip your file 
            
            #read in up-to-date AOU_Codes
            message("Reading in AOS codes.")
            species.codes<-read.dbf(file=paste(Dir,"AOS_Codes","LIST18.DBF",sep="/"))
            species.codes<-species.codes[,c("COMMONNAME","SCINAME","SPEC")]
            colnames(species.codes)<-c("English_Common_Name","Scientific_Name","AOU_Code")
            species.codes$Scientific_Name<-trimws(as.character(species.codes$Scientific_Name))
            species.codes$English_Common_Name<-trimws(as.character(species.codes$English_Common_Name))
            
            #Now add Order, Family, and Genus names
            message("Downloading latest list of species detected in Breeding Bird Survey data\nfrom USGS (ftp://ftpext.usgs.gov)")
            
            BBSurl="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt"
            download.file(url=BBSurl, destfile=paste(Dir,"AOS_Codes","SpeciesList.txt",sep="/"))
            
            #get official list of all BBS species info from url
            raw_species = readLines(paste(Dir,"AOS_Codes","SpeciesList.txt",sep="/"))
            pasted_species = paste0(raw_species, collapse = "\n")
            
            # Convert to UTF-8 and then save.
            # The encoding wasn't actually latin1, it was something R couldn't handle.  But
            # on my machine at least, iconv seemed to accept it.
            write(
              iconv(gsub("\n([:alnum:])", "\1", pasted_species), from = "latin1", to = "UTF-8"), 
              file =paste(Dir,"AOS_Codes","SpeciesList_1.txt",sep="/")
            )
            
            dashes = readLines(paste(Dir,"AOS_Codes","SpeciesList_1.txt",sep="/"))[9]
            species_colnames = strsplit(readLines(paste(Dir,"AOS_Codes","SpeciesList_1.txt",sep="/"))[8]," +" )[[1]]
            
            species_list = read.fwf(
              paste(Dir,"AOS_Codes","SpeciesList_1.txt",sep="/"), 
              widths = nchar(strsplit(dashes, " ")[[1]]) + 1, 
              header = FALSE, 
              skip = 9,
              encoding = "UTF-8",
              stringsAsFactors = FALSE
            )
            colnames(species_list) = species_colnames
            
            #remove whitespace
            species_list$English_Common_Name<-trimws(as.character(species_list$English_Common_Name))
            
            #add Scientific_Name column
            species_list$Scientific_Name<-trimws(paste(trimws(species_list$Genus), trimws(species_list$Species),sep=" "))
            
            #merge species_list with read.dbf
            species.merge<-merge(species_list, species.codes, by="English_Common_Name",all.x=TRUE)
            
            #clean up species.merge
            species.merge$Scientific_Name.y<-NULL
            names(species.merge)[names(species.merge)=="Scientific_Name.x"]<-"Scientific_Name"
            names(species.merge)[names(species.merge)=="English_Common_Name"]<-"Common_Name"
            names(species.merge)[names(species.merge)=="ORDER"]<-"Order"
            
            
            #download PIF watchlist data
            message("Downloading latest Partners in Flight watchlist information from (http://pif.birdconservancy.org)")
            PIFurl ="http://pif.birdconservancy.org/ACAD/ajax/download.aspx?list=Glo&regions=US"
            download.file(url=PIFurl, destfile=paste(Dir,"AOS_Codes","PIF_watchlist.csv",sep="/"))
            
            #read in pif.data
            pif.data<-read.csv(paste(Dir,"AOS_Codes", "PIF_watchlist.csv",sep="/"),quote = "", 
                               row.names = NULL,stringsAsFactors = FALSE)
            
            pif.data.2<-as.data.frame(sapply(pif.data, function(x) gsub("\"", "", x)))
            pif.data.2$row.names<-NULL
            
            #fix column names
            pif.names<-names(pif.data.2)[-1]
            names(pif.data.2)<-pif.names
            
            #get desired columns
            pif.data.3<-pif.data.2[,c("common_name","continental_importance","iucn_red_list_2016")]
            colnames(pif.data.3)<-c("Common_Name","Continental_Concern","IUCN_Red_List_2016")
            pif.data.3$Common_Name<-trimws(as.character(pif.data.3$Common_Name))
            
            #merge pif.data with species.merge
            species.out<-merge(species.merge, pif.data.3, by="Common_Name",all.x=TRUE)
            species.out$Common_Name<-trimws(species.out$Common_Name)
            
            #Add common names for some edge cases:  (Yellow-shafted Flicker) Norther Flicker, (Myrtle Warbler) Yellow-rumped Warbler

            NorthernFlicker<-subset(species.out, Common_Name=="(Yellow-shafted Flicker) Northern Flicker")
            NorthernFlicker$Common_Name<-"Northern Flicker"
            NorthernFlicker$AOU_Code<-"NOFL"
            
            YellowRumpedWarbler<-subset(species.out, Common_Name=="(Myrtle Warbler) Yellow-rumped Warbler")
            YellowRumpedWarbler$Common_Name<-"Yellow-rumped Warbler"
            YellowRumpedWarbler$AOU_Code<-"YRWA"
            
            species.out<-rbind(species.out, NorthernFlicker)
            species.out<-rbind(species.out, YellowRumpedWarbler)
            
            #save species info to .csv file
            message("Saving updated species info as SpeciesList_out.csv")
            write.csv(species.out, paste(Dir,"AOS_Codes","SpeciesList_out.csv",sep="/"),row.names=FALSE)  
            
          }
