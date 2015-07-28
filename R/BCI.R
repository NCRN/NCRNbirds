#' @include NCRNbirds_Class_def.R getVisits.R getBirds.R 
#' 
#' @title BCI
#' 
#' @description Calcualtes the Bird Conmmunity Index (BCI) of O'Connell et al 1998, 2000.
#' 
#' @importFrom dplyr mutate rowwise
#' @importFrom magrittr %>%
#' 
#' 
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param years  A vector of numbers. Will only return data from the indicated years. It is highly recommeded to only calculate the BCI over a single year's data.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{\link{getChecklist}} and from there to \code{\link{getBirds}}
#' 
#' @return Returns a \code{data.frame} with the BCI, the BCI category (Low Integrity, High Integrity etc.), the number and percent of species present in each guild and guild type.
#' 
#' @details This calculates the Bird Community Index (BCI - O'Connell et al 1998, 2000) for a park or parks, and can be restricted by point as well. Typically a single years' data is used for the BCI, so it is recommend (but not strictly requried) that a single year be indicated in the \code{years} argument. The fuction calls \code{\link{getChecklist}} to get a species list of birds for each point.  That function, in turn, calls \code{\link{getBirds}}. Any valid argument for getBirds can be inluded as part of \code{...}. By default birds from all distance bands and time intervals will be included in the calculation.
#' 
#' @references O'Connell, TJ, LE Jackson and RP Brooks. 1998. The Bird Community Index: A Tool for Assessing Biotic Integrity in the Mid-Atlantic Highlands. Final Report prepared for U. S. Environmental Protection Agency, Region III.
#' 
#' O'Connell TJ, LE Jackson and RP Brooks. 2000. Bird guilds as indicators of ecological condition in the central Appalachians. Ecological Applications 10:1706-1721.
#'  
#' @export


########################


setGeneric(name="BCI",function(object,years=NA,points=NA,output="dataframe",...){standardGeneric("BCI")}, signature="object")



setMethod(f="BCI", signature=c(object="list"),
          function(object, years, points, output,...) {
            OutMat<-lapply(X=object, FUN=BCI, years=years, points=points,...)
            switch(output,
                   list= return(OutMat),
                   dataframe=return(do.call("rbind",OutMat))
            )
          })


setMethod(f="BCI", signature=c(object="NCRNbirds"),
  function(object,years, points,...){
      
    XPoints<-getPoints(object=object,years=years)$Point_Name
    XList<-lapply(X=XPoints, FUN=getChecklist, object=object, years=years, ...)
    XGuilds<-getGuilds(object=object)
            
    XBCI<-data.frame(Point_Name=getPoints(object=object,years=years)[c("Admin_Unit_Code","Point_Name")])
    XBCI<-XBCI %>% rowwise %>% 
    mutate(ForestGeneralist= sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Primary.Habitat=="Forest Generalist",]$AOU_Code),
          InteriorForest=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Primary.Habitat=="Interior Forest Obligate",]$AOU_Code),
          ForestGroundNester=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Nest.Placement=="Forest Ground Nester",]$AOU_Code),
          OpenGroundNester=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Nest.Placement=="Open Ground Nester",]$AOU_Code),
          ShrubNester=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Nest.Placement=="Shrub",]$AOU_Code),
          CanopyNester=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Nest.Placement=="Canopy",]$AOU_Code),
          BarkProber=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Foraging.Behavior=="Bark Prober" & 
                                                                  XGuilds$Trophic.Level=="Insectivore",]$AOU_Code),
          GroundGleaner=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Foraging.Behavior=="Ground Gleaner" & 
                                                                  XGuilds$Trophic.Level=="Insectivore",]$AOU_Code),
          CanopyInsectivore=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Foraging.Behavior=="Upper Canopy Forager" & 
                                                                  XGuilds$Trophic.Level=="Insectivore",]$AOU_Code),
          ShrubInsectivore=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Foraging.Behavior=="Lower Canopy Forager" & 
                                                                  XGuilds$Trophic.Level=="Insectivore",]$AOU_Code),
          Omnivore=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Trophic.Level=="Omnivore",]$AOU_Code),
          NestPredator=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Pred_Para_Desc=="Yes",]$AOU_Code),
          Exotic=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Exotic=="Exotic",]$AOU_Code),
          Resident=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Migratory=="Resident",]$AOU_Code),
          TemperateMigrant=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Migratory=="Temperate Migrant",]$AOU_Code),
          SingleBrooded=sum(XList[[Point_Name]]%in% XGuilds[XGuilds$Brood.Numbers=="Single Brooded",]$AOU_Code),
          Total=length(XList[[Point_Name]])
    ) 
    BCI<-XBCI %>% 
    mutate(Pro_ForestGeneralist=round(ForestGeneralist/Total,digits=3),
          Pro_InteriorForest=round(InteriorForest/Total,digits=3),
          Pro_ForestGroundNester=round(ForestGroundNester/Total,digits=3),
          Pro_OpenGroundNester=round(OpenGroundNester/Total,digits=3),
          Pro_ShrubNester=round(ShrubNester/Total,digits=3),
          Pro_CanopyNester=round(CanopyNester/Total,digits=3),
          Pro_BarkProber=round(BarkProber/Total,digits=3),
          Pro_GroundGleaner=round(GroundGleaner/Total,digits=3),
          Pro_CanopyInsectivore=round(CanopyInsectivore/Total,digits=3),
          Pro_ShrubInsectivore=round(ShrubInsectivore/Total,digits=3),
          Pro_Omnivore=round(Omnivore/Total,digits=3),
          Pro_NestPredator=round(NestPredator/Total,digits=3),
          Pro_Exotic=round(Exotic/Total, digits=3),
          Pro_Resident=round(Resident/Total,digits=3),
          Pro_TemperateMigrant=round(TemperateMigrant/Total,digits=3),
          Pro_SingleBrooded=round(SingleBrooded/Total,digits=3)
    )
     
   XBCI<-XBCI %>% 
   mutate(BCI_ForestGeneralist=c(4.5, 2.5)[findInterval(Pro_ForestGeneralist,vec=c(0, 0.281, 1.001))],
        BCI_InteriorForest=c(1, 1.5, 3, 4, 5)[findInterval(Pro_InteriorForest, vec=c(0,0.011,0.081,0.261,0.431,1.001))],
        BCI_ForestGroundNester=c(1, 1.5, 3, 4.5, 5)[findInterval(Pro_ForestGroundNester, vec=c(0,0.001,0.021,0.161,0.241,1.001))],
        BCI_OpenGroundNester=c(1, 2.5, 5)[findInterval(Pro_OpenGroundNester, vec=c(0,0.021,0.111,1.001))],
        BCI_ShrubNester=c(4, 1.5, 1)[findInterval(Pro_ShrubNester, vec=c(0,0.211,0.331,1.001))],
        BCI_CanopyNester=c(1.5, 2, 4.5)[findInterval(Pro_CanopyNester, vec=c(0,0.281,0.321,1.001))],
        BCI_BarkProber=c(1.5, 3, 4, 5)[findInterval(Pro_BarkProber, vec=c(0,0.061,0.111,0.171,1.001))],
        BCI_GroundGleaner=c(1.5, 2, 4.5, 5)[findInterval(Pro_GroundGleaner, vec=c(0,0.051,0.071,0.141,1.001))],
        BCI_CanopyInsectivore=c(1.5, 2, 3, 4.5, 5)[findInterval(Pro_CanopyInsectivore, vec=c(0,0.031,0.051,0.121,0.201,1.001))],
        BCI_ShrubInsectivore=c(1.5, 2.5, 5)[findInterval(Pro_ShrubInsectivore, vec=c(0,0.141,0.231,1.001))],
        BCI_Omnivore=c(5, 4, 3, 1, 2)[findInterval(Pro_Omnivore, vec=c(0,0.291,0.411,0.481,0.581,1.001))],
        BCI_NestPredator=c(5, 3.5, 2, 1)[findInterval(Pro_NestPredator, vec=c(0,0.101,0.151,0.181,1.001))],
        BCI_Exotic=c(5, 4.5, 3, 2, 1)[findInterval(Pro_Exotic, vec=c(0,0.001,0.021,0.051,0.111,1.001))],
        BCI_Resident=c(5, 3.5, 2, 1)[findInterval(Pro_Resident, vec=c(0,0.261,0.391,0.571,1.001))],
        BCI_TemperateMigrant=c(4, 2, 1)[findInterval(Pro_TemperateMigrant, vec=c(0,0.211,0.301,1.001))],
        BCI_SingleBrooded=c(1.5, 2, 3, 4, 5)[findInterval(Pro_SingleBrooded, vec=c(0,0.411,0.451,0.611,0.731,1.001))]
    )
   XBCI<-XBCI %>% 
    mutate(BCI_Structural=BCI_ForestGeneralist + BCI_InteriorForest + BCI_ForestGroundNester + BCI_OpenGroundNester + 
             BCI_ShrubNester +BCI_CanopyNester,
          BCI_Funcitonal=BCI_BarkProber + BCI_GroundGleaner + BCI_CanopyInsectivore + BCI_ShrubInsectivore + BCI_Omnivore,
          BCI_Compostional=BCI_NestPredator + BCI_Exotic + BCI_Resident + BCI_TemperateMigrant + BCI_SingleBrooded,
          BCI = BCI_Structural + BCI_Funcitonal + BCI_Compostional,
          BCI_Category=c("Low Integrity", "Medium Integrity","High Integrity","Highest Integrity")[findInterval(BCI,
              vec=c(0,40.1,52.1,60.1,77.1))]
    )
    return(as.data.frame(XBCI))
  })