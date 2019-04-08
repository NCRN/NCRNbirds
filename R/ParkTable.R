#' @include NCRNbirds_Class_def.R
#' 
#' @title ParkTable
#' 
#' @description Create a table of park units within a given network.
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param ... Additonal arguments passed to \code{\link{ParkTable}}.
#'
#' @export

########################


setGeneric(name="ParkTable",function(object,...){standardGeneric("ParkTable")}, signature="object")


############ need to add list method I

setMethod(f="ParkTable", signature=c(object="list"),
          function(object,...) {
            
            return(lapply(object=object, FUN=ParkTable...)
                   )
          }
  )


setMethod(f="ParkTable", signature=c(object="NCRNbirds"),
          function(object,...){
            
            data=object@Points
            
            #create UnitNames.df
            parkNames <- unique(data[,c("LongName","Admin_Unit_Code")])
            
            networkName<-data.frame(LongName=object@LongName, Admin_Unit_Code=object@ParkCode)
            
            parkNames<-unique(rbind(networkName, parkNames))
          
            
            response<-readline(prompt="Would you like to save these results? (y/n)")
            ifelse(response=="y",
                   {
                     suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))
                     
                     write.csv(parkNames, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_Park_Names",object@ParkCode,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)
                     
                     #return table
                     return({
                       print(parkNames)
                       parkNames
                       })
                   },
                   #return table
                   {
                     return({
                       print(parkNames)
                       parkNames
                     })
                   }
            )
            
            


          }
)