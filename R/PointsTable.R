#' @include NCRNbirds_Class_def.R getParkData.R 
#' 
#' @title PointsTable
#' 
#' @description Create a table of the number of sampling points at network- and park unit-levels among years.
#' 
#' @importFrom reshape2 melt dcast 
#' @importFrom stats aggregate
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param visits A length 1 numeric vector. Will only use data from points with that number of visits.
#' @param ... Additonal arguments passed to \code{\link{PointsTable}}.
#' 
#' @details This function generates a table tallying the number of sampling locations for an entire network and park units within each year.
#' 
#' @export


########################


setGeneric(name="PointsTable",function(object,years=NA,visits=NA,...){standardGeneric("PointsTable")}, signature="object")


############ need to add list method I

setMethod(f="PointsTable", signature=c(object="list"),
          function(object,years,visits,...) {
            
            return(lapply(object=object,FUN=PointsTable, years=years,visits=visits,...)
                   )
          }
  )


setMethod(f="PointsTable", signature=c(object="NCRNbirds"),
          function(object,years,visits,...){
            
            data=object@Visits

            parkNames <-object@Points
            parkNames <- unique(parkNames[,c("Admin_Unit_Code", "LongName")])
            
            #make Unit.Year column
            data$Unit.Year<-paste(data$Admin_Unit_Code, data$Year,sep=".")
            
            #subset data to inclued unique GRTS.Year rows
            data.sub<-unique(data[,c("Admin_Unit_Code","Year","Unit.Year","Point_Name")])
            
            #merge LongNames to data.sub
            data.merge<-merge(data.sub, parkNames, by="Admin_Unit_Code",all.x=TRUE)
            
            #create UnitNames.df and add network
            UnitNames.df<-rbind(parkNames, data.frame(Admin_Unit_Code=object@ParkCode,LongName=object@LongName))
            
            #add years if NA from function
            ifelse(is.na(years), years<-sort(unique(object@Visits$Year)), years<-years)
            
            
            #make frequency table of num. visits per GRTS.Year.Survey_Type
            table1<-aggregate(data.merge$Point_Name ~ data.merge$Admin_Unit_Code + data.merge$LongName + data.merge$Year, FUN="length")
            colnames(table1)<-c("Admin_Unit_Code","LongName","Year","Count_of_Points")
            
            #melt and cast (pivot data)
            table1.melt<-melt(table1, id.vars=c("Admin_Unit_Code","LongName","Year"), measure.vars="Count_of_Points")
            table1.cast<-suppressWarnings(dcast(table1.melt, Admin_Unit_Code + LongName ~ Year, value.var="value",fun=max,drop=TRUE))
            
            network.sum<-data.frame(Admin_Unit_Code=object@ParkCode, LongName=object@LongName, t(colSums(table1.cast[3:13])))
            colnames(network.sum)[3:13]<-as.character(seq(from=min(years,na.rm=TRUE), to=max(years,na.rm=TRUE), by=1))
            
            ifelse(object@ParkCode!="NCRN", table1.comb<-table1.cast,table1.comb<-rbind(network.sum, table1.cast))
            
            
            #organize
            table1.out<-cbind(LongName=table1.comb$LongName, Admin_Unit_Code=table1.comb$Admin_Unit_Code, table1.comb[,-1:-2])
            colnames(table1.out)[1:2]<-c("LongName","Admin_Unit_Code")
            
            response<-readline(prompt="Would you like to save these results? (y/n)")
            ifelse(response=="y",
                   {
                     suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))

                     write.csv(table1.out, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_Count_of_Sampling_Points",object@ParkCode,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)

                   #return table
                     return({
                       print(table1.out)
                       table1.out
                     })
                   },
                   #return table
                   return({
                     print(table1.out)
                     table1.out
                   })
            )
            
          }
)