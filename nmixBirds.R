#' @title nmixBirds
#' 
#' @description Peforms trend analysis on bird data using N-mixture models (Royal 2004) form the unmarked package.
#' 
#'  @import unmarked
#'  @import tidyr 
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' 
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param AOU  A character vector. One or more AOU (American Onothological Union) codes of bird species.
#' @param years  A vector of number. will return only data from the indicated years.
#' @param band. A numeirc vector. Only observations whose \code{Distance_id} field matches a value in \code{band} will be returned.
#' @param interval A numeric vector. Only observations whose \code{Interval} field matches a value in \code{interval} will be returned.
#' @param visits, A length 1 numeric vector. Will only use data from points with that number of visits.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#' @details ADD ME
#' 
#' @include NCRNbirds_Class_def.R
#' @export


########################


setGeneric(name="nmixBirds",function(object,points=NA,AOU=NA,years=NA,band=1,interval=NA,visits=2,output="dataframe"){standardGeneric("nmixBirds")}, signature="object")


############ need to add list method I think


setMethod(f="nmixBirds", signature=c(object="list"),
          function(object,points,AOU,years,band,interval,visits, output) {
            OutBirds<-lapply(X=object, FUN=getBirds, points=points,AOU=AOU,years=years,band=band,interval=interval,visits=visits, output=output)
            switch(output
                   ,
                   list= return(OutBirds),
                   dataframe=return(do.call("rbind",OutBirds))
            )
          })



# Z<-getBirds(CATO,AOU="ACFL")
#  Y$Visit<-paste0("Visit",Y$Visit)
# 
# 
# GoodPoints<-Y %>%
#   group_by(Admin_Unit_Code,Plot_Name,Year) %>%
#   summarize(Vists=n()) %>%
# filter(Visits=visits)
# 
# 
# X<-left_join(GoodPoints,X %>% spread(key=Visit,value=Bird_Count,fill=0) %>% dplyr::select(Plot_Name,Year, Visit1, Visit2) )



setMethod(f="nmixBirds", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,band,interval,visits,output){
            
            MixVisits<-getVisits(object) %>%
              mutate(Visit=paste0("Visit",Visit)) %>%
              dplyr::select(Admin_Unit_Code,Plot_Name,Visit,Year) %>%
              left_join(getBirds(object=object, points=points, AOU=AOU, years=years, band=band, interval=interval))%>% 
              group_by(Admin_Unit_Code,Plot_Name, Visit, AOU_Code,Year) %>%
              summarize(Counts=sum(Bird_Count)) %>%
              spread(key=Visit,value=Counts,fill=0)# %>%
              
            
            ### fix so that missing visits = NA
            #filter(Plot_Name %in% 
            (getVisits(object=object) %>% dplyr::select(Plot_Name) %>% unlist %>% as.character) )
                                       
                      #group_by(Admin_Unit_Code, Plot_Name,Year) %>% 
                      # summarize(Visits=n()) %>% filter(Visits==visits) %>% ungroup %>% dplyr::select(Plot_Name) %>%
              #unique))
            return(MixVisits)
            
            GoodPoints<-MixVisits %>%
              group_by(Admint_Unit_Code, Plot_name,Year) %>%
              summarize(Visits=n()) %>%
              filter(Visits==visits)
              
            
            MixBirds<-getBirds(object, points, AOU, years, band, interval)
            MixBirds<-MixBirds %>%
              left_join(getVisits(object=object,years=years,points=points) %>%
                          mutate(Visit=paste0("Visit",Visit))%>% 
                          dplyr::select(Plot_Name,Date,Visit)) %>%
              group_by(Admin_Unit_Code,Plot_Name,Visit,AOU_Code,Year) %>%
              summarize(Counts=sum(Bird_Count))
            MixData<-spread(MixBirds,key=Visit,value=Counts,fill=0) 
            unmarkedData<-unmarkedFramePCount(y=dplyr::select(MixData,Visit1,Visit2), 
                                              siteCovs=as.data.frame(dplyr::select(MixData,Year,Admin_Unit_Code,Plot_Name)))
           return(unmarkedData)
            ifelse(length(years)==1,
              return(pcount(~1~1, data=unmarkedData, mixture="P",K=100)),
              return(pcount(~1~Year, data=unmarkedData,mixture="P",K=100))
           )
            
            
          }
)