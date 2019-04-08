#' @include NCRNbirds_Class_def.R 
#' 
#' @title PointsXVisitsTable
#' 
#' @description Create a table of the max number of visits to each sampling point in each year. NAs indicate the point was not sampled.
#' 
#' @importFrom reshape2 melt dcast 
#' @importFrom stats aggregate
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param visits A length 1 numeric vector. Will only use data from points with that number of visits.
#' @param ... Additonal arguments passed to \code{\link{PointsXVisitsTable}}.
#' 
#' @details This function generates a table with the maximum number of times each point was visited in each year. NAs indicate points that were not sampled in given years.
#' 
#' @export


########################


setGeneric(name="PointsXVisitsTable",function(object,years=NA,visits=NA,...){standardGeneric("PointsXVisitsTable")}, signature="object")


############ need to add list method I

setMethod(f="PointsXVisitsTable", signature=c(object="list"),
          function(object,years,visits,...) {
            
            return(lapply(object=object,years=years,visits=visits,FUN=PointsXVisitsTable,...)
                   )
          }
  )


setMethod(f="PointsXVisitsTable", signature=c(object="NCRNbirds"),
          function(object,years,visits,...){
            
            dataVisits=object@Visits
            
            parkNames <-object@Points
            parkNames <- unique(parkNames[,c("Admin_Unit_Code", "LongName")])
            
            #subset data to inclued unique GRTS.Year rows
            data.sub<-unique(dataVisits[,c("Admin_Unit_Code","Year","Point_Name","Visit")])
            
            #merge LongNames to data.sub
            data.merge<-merge(data.sub, parkNames, by="Admin_Unit_Code",all.x=TRUE)
            
            #add years if NA from function
            ifelse(is.na(years), years<-sort(unique(object@Visits$Year)), years<-years)
            
            #make frequency table of num. visits per GRTS.Year.Survey_Type
            table1<-aggregate(data.merge$Visit ~ data.merge$Admin_Unit_Code + data.merge$LongName + data.merge$Year + data.merge$Point_Name, FUN="length")
            colnames(table1)<-c("Admin_Unit_Code","LongName","Year","Point_Name","Count_of_Visits")
            
            #melt and cast (pivot data)
            table1.melt<-melt(table1, id.vars=c("Admin_Unit_Code","LongName","Year","Point_Name"), measure.vars="Count_of_Visits")
            table1.cast<-suppressWarnings(dcast(table1.melt, Admin_Unit_Code + LongName + Point_Name ~ Year, value.var="value",fun=max,drop=TRUE))
            table1.cast[table1.cast==-Inf]<-NA

            table1.out<-table1.cast
            
            response<-readline(prompt="Would you like to save these results? (y/n)")
            ifelse(response=="y",
                   {
                     suppressWarnings(dir.create(path=paste(getwd(), paste("NCRNbirds_Output", Sys.Date(), sep="_"), sep="/")))

                     write.csv(table1.out, file=paste(paste("NCRNbirds_Output", Sys.Date(), sep="_"), paste(paste("Table_Visit_Count",object@ParkCode,Sys.Date(), sep="_"),".csv", sep=""),  sep="/"), row.names=FALSE)

                   #return table
                   return(table1.out)
                   },
                   #return table
                   return(table1.out)
            )
          
          }
)