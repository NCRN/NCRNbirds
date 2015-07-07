#' @include NCRNbirds_Class_def.R
#' 
#' @title getGuilds
#' 
#' @description Returns the guild data from the \code{Species} slot of an NCRNbirds object. The returned data can be filtered to meet various criteria.
#' 
#' @importFrom dplyr select 
#' 
#' @param object An \code{NCRNbirds} object or a list of such objects.
#' @param AOU, an optional character vector of AOU codes. Each code should be in quotes. Only data from those birds will be returned.
#' @param categories A character vector of guild categories. Only data for those categories will be returned. Options are:
#' \describe{
#' \item{"Migratory"}{Migratory status of the species}
#' \item{"Brood.Numbers"}{Is the speices single or double brooded}
#' \item{"Primary.Habitat"}{Is the species a forest generalist or an interior forest obligate?}
#' \item{"Exotic"}{Is the species exotic or native?}
#' \item{"Nest.Placement"}{Where is the species' nest found? Possibilities include canopy, shrub, open ground nester etc.}
#' \item{"Foraging.Behavior"}{What tyoe of foraging behavior does the species show? Possibilites include wate plunger, bark prober,upper canopy forager, etc.}
#' \item{"Tropic.Level"}{What does the species feed on? Can be omnivore, carnivore, insectivore etc.}
#' \item{"Nest.Height"}{How high does the species nest? High, low or highly variable.}
#' \item{"Pred_Para_Desc"}{Is the species a nest predator or brood parasite.Yes or no.}
#' }
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function. When a list of \code{NCRNbirds} object is used, and the output is "dataframe" then a each unique conbination of species and guidls will only be listed once in the resulting dataframe. If some species have ifferent guild assingments in the different \code{NCRNbirds} objects in the list, then those species will be listed twice which could have unintended consiquences
#' 
#'@details This function returns data on bird guilds from an \code{NPSForVeg} object or a list of such objects. This data is stored in the \code{Species} slot.
#'
#'  @export


setGeneric(name="getGuilds",function(object,AOU=NA,categories=NA,output="dataframe"){standardGeneric("getGuilds")}, signature="object")


 setMethod(f="getGuilds", signature=c(object="list"),
           function(object,AOU,categories,output) {
             OutGuilds<-lapply(X=object, FUN=getGuilds, AOU=AOU, categories=categories)
             switch(output,
                    list=return(OutGuilds),
                    dataframe=return(unique(do.call("rbind",OutGuilds)))
              )
})


setMethod(f="getGuilds", signature=c(object="NCRNbirds"),
          function(object,AOU,categories){
            
            XGuilds<-object@Species
            
            if(!anyNA(AOU)) XGuilds<-XGuilds[XGuilds$AOU %in% AOU,]
            
            if(!anyNA(categories)) GNames<-c("AOU_Code",categories) else GNames<-c("AOU_Code", "Migratory", "Brood.Numbers", "Primary.Habitat",
                                                                        "Exotic", "Nest.Placement", "Foraging.Behavior","Trophic.Level",
                                                                        "Nest.Height","Pred_Para_Desc")
            
            XGuilds<-XGuilds %>% dplyr::select(which(colnames(XGuilds) %in% GNames))
            
            return(XGuilds)
})
