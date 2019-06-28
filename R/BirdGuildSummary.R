#' @include NCRNbirds_Class_def.R BCI.R
#'   
#' @title BirdGuildSummary
#'
#' @importFrom dplyr group_by mutate left_join right_join summarise_if select
#' @importFrom magrittr %>% 
#' @importFrom tidyr gather
#' @importFrom data.table rbindlist
#' 
#' @description Calculates a summary of guild membership across point counts.
#' 
#' @param object An \code{NCRNbirds} object a \code{list} of such objects, or a \code{data.frame} like that produced by \code{birdRichness()}.
#' @param years  A numeric vector. Indicates which years should be summarized.
#' @param points A character vector of point names. Only these points will be used.
#' @param visits A length 1 numeric vector, defaults to NA. Returns data only from the indicated visits. 
#' @param times A numeric vector of length 1. Returns only data from points where the number of years that a point has been vistied is greater or equal to the value of \code{times}. This is determined based on the data found in the \code{Visits} slot.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{\link{BCI}}
#' 
#' @return Returns a \code{data.frame} of the mean and SD of percent guild membership.
#' 
#' @details This calculates a summary of the percent guild membership across point counts.
#' 
#' @references O'Connell, TJ, LE Jackson and RP Brooks. 1998. The Bird Community Index: A Tool for Assessing Biotic Integrity in the 
#' Mid-Atlantic Highlands. Final Report prepared for U. S. Environmental Protection Agency, Region III.
#' 
#' O'Connell TJ, LE Jackson and RP Brooks. 2000. Bird guilds as indicators of ecological condition in the central Appalachians. 
#' Ecological Applications 10:1706-1721.

#' @export

setGeneric(name="BirdGuildSummary",function(object,years=NA,points=NA, visits=NA, times= NA,output="dataframe",...){standardGeneric("BirdGuildSummary")}, signature="object")

setMethod(f="BirdGuildSummary", signature=c(object="list"),
          function(object, years, points,visits, times, output ,...) {
            OutMat<-lapply(X=object, FUN=BirdGuildSummary, years=years, points=points, visits= visits,times= times, ...)
            switch(output,
                   list= return(OutMat),
                   dataframe= return(rbindlist(OutMat, use.names=TRUE, fill=TRUE))
            )
          })


setMethod(f="BirdGuildSummary", signature=c(object="NCRNbirds"),
          function(object,years, points,visits, times,output,...){
    
        #### Summarize Guild info----          
        
     Guildlist<-BCI(object=object, years=years, points=points, visits=visits, times=times, output="dataframe",...)
    
        MeanProp<- Guildlist %>% # calc mean percentage guild membership across points
          dplyr::select(Admin_Unit_Code,starts_with("Pro_")) %>% # just grab the proportions from BCI
          dplyr::group_by(Admin_Unit_Code) %>% 
          dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
          tidyr::gather(Response_Guild, mean, -Admin_Unit_Code) %>% 
          dplyr::mutate(mean = round(mean*100,1))
        
        SDProp<- Guildlist %>% # calc SD in the percentage of guild membership across points
          dplyr::select(Admin_Unit_Code,starts_with("Pro_")) %>% # just grab the proportions from BCI
          dplyr::group_by(Admin_Unit_Code) %>% 
          dplyr::summarise_if(is.numeric, sd, na.rm = TRUE) %>% 
          tidyr::gather(Response_Guild, sd, -Admin_Unit_Code) %>% 
          dplyr::mutate(sd = round(sd*100,1))
        
        
        GuildProp<-left_join(MeanProp,SDProp,by=c("Admin_Unit_Code","Response_Guild")) %>% # cobine mean and SD
          dplyr::mutate(Response_Guild= str_remove(Response_Guild,"Pro_")) %>% # remove header for joining
          dplyr::right_join(unique(object@Guilds[c("Int_Element","Guild_Category", "Response_Guild")]),., by= "Response_Guild") %>% 
          dplyr::arrange(Int_Element,Guild_Category,Response_Guild) %>% 
          dplyr::select(Admin_Unit_Code, "Integrity Element" = Int_Element, "Guild Category" = Guild_Category, "Response Guild" = Response_Guild, mean,sd)
        
        return(GuildProp)
        
})
