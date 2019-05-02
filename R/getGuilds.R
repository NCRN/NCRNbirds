#' @include NCRNbirds_Class_def.R
#' 
#' @title getGuilds
#' 
#' @description Returns the guild data from the \code{Guilds} slot of an NCRNbirds object. The returned data can be filtered to meet 
#' various criteria.
#' 
#' @importFrom data.table rbindlist
#' @importFrom dplyr distinct
#' 
#' @param object An \code{NCRNbirds} object or a list of such objects.
#' @param AOU An optional character vector of AOU codes. Defaults to \code{NA} Each code should be in quotes. Only data from those birds will be returned.
#' @param type An optional character vector that indicates which BCI you want guilds for. Typically you would pick only 1, but you can pick more. Options are:
#' \describe{
#' \item{\code{NA}}{The default, returns data from all BCIs}
#' \item{"Cent_Appal"}{Central Appalachians BCI}
#' \item{"NETN_Forest_BCI"}{The Northeast Temperate Network's forest BCI}
#' }
#' @param elements An optional character vector that indicates which will filter the Guild dat aby integrity elements from the BCI. 
#' Defaults to \code{NA} which returns all integrity elements. Other options are "Compositional", "Functional", "Structural"
#' @param categories A character vector of guild categories. Defaults to \code{NA}. Only data for those categories will be returned. Options are:
#' \describe{
#' \item{"InsectavoreForagingBehavior"}{What tyoe of foraging behavior does the species show? Possibilites include wate plunger, 
#' bark prober,upper canopy forager, etc.}
#' \item{"Migratory"}{Migratory status of the species}
#' \item{"NestPlacement"}{Where is the species' nest found? Possibilities include canopy, shrub, open ground nester etc.}
#' \item{"NumberofBroods"}{Is the speices single or double brooded}
#' \item{"Origin"}{Is the species exotic or native?}
#' \item{"PopulationLimitingCode"}{Is the species a nest predator or brood parasite.Yes or no.}
#' \item{"PrimaryHabitat"}{Is the species a forest generalist or an interior forest obligate?}
#' \item{"Trophic"}{What does the species feed on? Can be omnivore, carnivore, insectivore etc.}
#' }
#' @param guilds A character vector that defualts to \code{NA}. When not \code{NA} it will filter the guild infomraiton by this field. Options include
#' "BarkProbler", "CanopyNester", "Exotic", "ForestGeneralist", "ForestGroundNester", "GroundGleaner", "InteriorForestObligate", "LowerCanopyForger",
#' "NetPredator_BroodParasite", "Omnivore", "OpenGroundNester","Resident,"ShrubNester","SingleBrooded","TemperateMigrant" and "UpperCanopyForager".
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function. 
#' When a list of \code{NCRNbirds} object is used, and the output is "dataframe" then a each unique conbination of species and guidls 
#' will only be listed once in the resulting dataframe. If some species have different guild assingments in the different \code{NCRNbirds} objects
#' in the list, then those species will be listed twice which could have unintended consiquences
#' 
#' @details This function returns data on bird guilds from an \code{NPSForVeg} object or a list of such objects. This data is stored in the 
#' \code{Guilds} slot.
#'
#' @export


setGeneric(name="getGuilds",function(object,AOU=NA,type=NA,elements=NA, categories=NA, guilds=NA, 
                                     output="dataframe"){standardGeneric("getGuilds")}, signature="object")


 setMethod(f="getGuilds", signature=c(object="list"),
           function(object,AOU,type,elements, categories, guilds, output) {
             OutGuilds<-lapply(X=object, FUN=getGuilds, AOU=AOU, type=type,elements=elements, categories=categories, guilds=guilds)
             switch(output,
                    list=return(OutGuilds),
                    dataframe=return(distinct(rbindlist(OutGuilds)))
              )
})


setMethod(f="getGuilds", signature=c(object="NCRNbirds"),
          function(object,AOU, type, elements, categories, guilds){
            
            XGuilds<-object@Guilds
            
            if(!anyNA(AOU)) XGuilds<-XGuilds[XGuilds$AOU_Code %in% AOU,]
            if(!anyNA(type)) XGuilds<-XGuilds[XGuilds$BCI_Type %in% type,]
            if(!anyNA(elements)) XGuilds<-XGuilds[XGuilds$Int_Element %in% elements,]
            if(!anyNA(categories)) XGuilds<-XGuilds[XGuilds$Guild_Category %in% categories,]
            if(!anyNA(guilds)) XGuilds<-XGuilds[XGuilds$Response_Guild %in% guilds,]
            
            return(XGuilds)
})
