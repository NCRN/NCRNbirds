#' @include NCRNbirds_Class_def.R getVisits.R getBirds.R 
#' 
#' @title BCI
#' 
#' @description Calculates the Bird Conmmunity Index (BCI) of O'Connell et al 1998, 2000.
#' 
#' @importFrom dplyr mutate rename select
#' @importFrom magrittr %>%
#' @importFrom purrr map map_int
#' @importFrom tibble as_tibble
#' 
#' 
#' @param object An \code{NCRNbirds} object or a \code{list} of such objects.
#' @param years  A vector of numbers. Will only return data from the indicated years. It is highly recommeded to only 
#' calculate the BCI over a single year's data.
#' @param points A character vector. The names of one or more points where the data was collected.
#' @param type A mandatory length 1 character \code{vector} that indicates the type of BCI to calculate. Can be "Cent_Appal" , the default, or "NETN_Forest_BCI"
#' @param checklist Locgical. Indicates if a list column with a checklist of birds for each point should be included in the output. Defaults to \code{FALSE}.
#' @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' @param ... Additional arguments passed to \code{\link{getChecklist}} and from there to \code{\link{getBirds}}
#' 
#' @return Returns a \code{data.frame} with the BCI, the BCI category (Low Integrity, High Integrity etc.), the number and percent of species 
#' present in each guild and guild type.
#' 
#' @details This calculates the Bird Community Index (BCI - O'Connell et al 1998, 2000) for a park or parks, and can be restricted by point as well.
#' Typically a single years' data is used for the BCI, so it is recommended (but not strictly requried) that a single year be indicated in 
#' the \code{years} argument. The function calls \code{\link{getChecklist}} to get a species list of birds for each point.  
#' That function, in turn, calls \code{\link{getBirds}}. Any valid argument for getBirds can be inluded as part of \code{...}. By default birds 
#' from all distance bands and time intervals will be included in the calculation.
#' 
#' @references O'Connell, TJ, LE Jackson and RP Brooks. 1998. The Bird Community Index: A Tool for Assessing Biotic Integrity in the 
#' Mid-Atlantic Highlands. Final Report prepared for U. S. Environmental Protection Agency, Region III.
#' 
#' O'Connell TJ, LE Jackson and RP Brooks. 2000. Bird guilds as indicators of ecological condition in the central Appalachians. 
#' Ecological Applications 10:1706-1721.
#'  
#' @export


########################


setGeneric(name="BCI",function(object,years=NA,points=NA,type="Cent_Appal", checklist=F, output="dataframe",...){standardGeneric("BCI")}, signature="object")



setMethod(f="BCI", signature=c(object="list"),
          function(object, years, points, type,checklist, output,...) {
            OutMat<-lapply(X=object, FUN=BCI, years=years, points=points,type=type, checklist=checklist, ...)
            switch(output,
                   list= return(OutMat),
                   dataframe=return(do.call("rbind",OutMat))
            )
          })


setMethod(f="BCI", signature=c(object="NCRNbirds"),
  function(object,years, points,type,checklist,...){
    
    #Get all guild info first so it does not have to be retrieved for every point
    
    XGuilds<-getGuilds(object=object, type=type)
    GuildSpecies<-unique(XGuilds$AOU_Code)
    
    ForGen<-getGuilds(object=object, type=type, guilds = "ForestGeneralist")$AOU_Code
    InForOb<-getGuilds(object=object,type=type, guilds = "InteriorForestObligate")$AOU_Code
    ForGroNest<-getGuilds(object=object, type=type,guilds = "ForestGroundNester")$AOU_Code
    OpenGroNest<-getGuilds(object=object,type=type, guilds = "OpenGroundNester")$AOU_Code
    ShrubNest<-getGuilds(object=object,type=type, guilds = "ShrubNester")$AOU_Code
    CanNest<-getGuilds(object=object, type=type,guilds = "CanopyNester")$AOU_Code
    BarkPro<-getGuilds(object=object, type=type,guilds = "BarkProber")$AOU_Code
    GroundGl<-getGuilds(object=object, type=type,guilds = "GroundGleaner")$AOU_Code
    UpperCanFor<-getGuilds(object=object, type=type,guilds = "UpperCanopyForager")$AOU_Code
    LowCanFor<-getGuilds(object=object, type=type,guilds = "LowerCanopyForager")$AOU_Code
    Omni<-getGuilds(object=object,type=type, guilds = "Omnivore")$AOU_Code
    NPBP<-getGuilds(object=object,type=type, guilds = "NestPredator_BroodParasite")$AOU_Code
    Exo<-getGuilds(object=object,type=type, guilds = "Exotic")$AOU_Code
    Resid<-getGuilds(object=object, type=type,guilds = "Resident")$AOU_Code
    TempMi<-getGuilds(object=object, type=type,guilds = "TemperateMigrant")$AOU_Code
    SingBrood<-getGuilds(object=object,type=type, guilds = "SingleBrooded")$AOU_Code
    
    # Count birds in each guild
    XBCI<-as_tibble(getPoints(object=object,years=years, points=points)[c("Admin_Unit_Code","Point_Name")]) %>% rename(points=Point_Name)
    
    if(nrow(XBCI)==0) return()

    XBCI<-XBCI %>% mutate(CheckList=map(points,.f=getChecklist, object=object, years=years, AOU=GuildSpecies, ...))
    
    XBCI<-XBCI %>% mutate(
            ForestGeneralist = map_int(CheckList,~.[. %in% ForGen] %>% length),
            InteriorForestObligate=map_int(CheckList,~.[. %in% InForOb] %>% length),
            ForestGroundNester=map_int(CheckList,~.[. %in% ForGroNest] %>% length),
            OpenGroundNester=map_int(CheckList,~.[. %in% OpenGroNest] %>% length),
            ShrubNester=map_int(CheckList,~.[. %in% ShrubNest] %>% length),
            CanopyNester=map_int(CheckList,~.[. %in% CanNest] %>% length),
            BarkProber=map_int(CheckList,~.[. %in% BarkPro] %>% length),
            GroundGleaner=map_int(CheckList,~.[. %in% GroundGl] %>% length),
            UpperCanopyForager=map_int(CheckList,~.[. %in% UpperCanFor] %>% length),
            LowerCanopyForager=map_int(CheckList,~.[. %in% LowCanFor] %>% length),
            Omnivore=map_int(CheckList,~.[. %in% Omni] %>% length),
            NestPredator_BroodParasite=map_int(CheckList,~.[. %in% NPBP] %>% length),
            Exotic=map_int(CheckList,~.[. %in% Exo] %>% length),
            Resident=map_int(CheckList,~.[. %in% Resid] %>% length),
            TemperateMigrant=map_int(CheckList,~.[. %in% TempMi] %>% length),
            SingleBrooded=map_int(CheckList,~.[. %in% SingBrood] %>% length),
            
            Total=map_int(CheckList,length),
            
          # Proportions for BCI  
            Pro_ForestGeneralist=round(ForestGeneralist/Total,digits=3),
            Pro_InteriorForestObligate=round(InteriorForestObligate/Total,digits=3),
            Pro_ForestGroundNester=round(ForestGroundNester/Total,digits=3),
            Pro_OpenGroundNester=round(OpenGroundNester/Total,digits=3),
            Pro_ShrubNester=round(ShrubNester/Total,digits=3),
            Pro_CanopyNester=round(CanopyNester/Total,digits=3),
            Pro_BarkProber=round(BarkProber/Total,digits=3),
            Pro_GroundGleaner=round(GroundGleaner/Total,digits=3),
            Pro_UpperCanopyForager=round(UpperCanopyForager/Total,digits=3),
            Pro_LowerCanopyForager=round(LowerCanopyForager/Total,digits=3),
            Pro_Omnivore=round(Omnivore/Total, digits=3),
            Pro_NestPredator_BroodParasite=round(NestPredator_BroodParasite/Total,digits=3),
            Pro_Exotic=round(Exotic/Total, digits=3),
            Pro_Resident=round(Resident/Total,digits=3),
            Pro_TemperateMigrant=round(TemperateMigrant/Total,digits=3),
            Pro_SingleBrooded=round(SingleBrooded/Total,digits=3)
    )
     
    XBCI[is.na(XBCI)]<-0   #This is for the situation where one of the totals is zero
   
    XBCI<-XBCI %>% 
      mutate(BCI_ForestGeneralist=c(4.5, 2.5)[findInterval(Pro_ForestGeneralist,vec=c(0, 0.281, 1.001))],
        BCI_InteriorForest=c(1, 1.5, 3, 4, 5)[findInterval(Pro_InteriorForestObligate, vec=c(0,0.011,0.081,0.261,0.431,1.001))],
        BCI_ForestGroundNester=c(1, 1.5, 3, 4.5, 5)[findInterval(Pro_ForestGroundNester, vec=c(0,0.001,0.021,0.161,0.241,1.001))],
        BCI_OpenGroundNester=c(1, 2.5, 5)[findInterval(Pro_OpenGroundNester, vec=c(0,0.021,0.111,1.001))],
        BCI_ShrubNester=c(4, 1.5, 1)[findInterval(Pro_ShrubNester, vec=c(0,0.211,0.331,1.001))],
        BCI_CanopyNester=c(1.5, 2, 4.5)[findInterval(Pro_CanopyNester, vec=c(0,0.281,0.321,1.001))],
        BCI_BarkProber=c(1.5, 3, 4, 5)[findInterval(Pro_BarkProber, vec=c(0,0.061,0.111,0.171,1.001))],
        BCI_GroundGleaner=c(1.5, 2, 4.5, 5)[findInterval(Pro_GroundGleaner, vec=c(0,0.051,0.071,0.141,1.001))],
        BCI_UpperCanopyForager=c(1.5, 2, 3, 4.5, 5)[findInterval(Pro_UpperCanopyForager, vec=c(0,0.031,0.051,0.121,0.201,1.001))],
        BCI_LowerCanopyForager=c(1.5, 2.5, 5)[findInterval(Pro_LowerCanopyForager, vec=c(0,0.141,0.231,1.001))],
        BCI_Omnivore=c(5, 4, 3, 2, 1)[findInterval(Pro_Omnivore, vec=c(0,0.291,0.411,0.481,0.581,1.001))],
        BCI_NestPredator_BroodParasite=c(5, 3.5, 2, 1)[findInterval(Pro_NestPredator_BroodParasite, vec=c(0,0.101,0.151,0.181,1.001))],
        BCI_Exotic=c(5, 4.5, 3, 2, 1)[findInterval(Pro_Exotic, vec=c(0,0.001,0.021,0.051,0.111,1.001))],
        BCI_Resident=c(5, 3.5, 2, 1)[findInterval(Pro_Resident, vec=c(0,0.261,0.391,0.571,1.001))],
        BCI_TemperateMigrant=c(4, 2, 1)[findInterval(Pro_TemperateMigrant, vec=c(0,0.211,0.301,1.001))],
        BCI_SingleBrooded=c(1.5, 2, 3, 4, 5)[findInterval(Pro_SingleBrooded, vec=c(0,0.411,0.451,0.611,0.731,1.001))],
    
        BCI_Structural=BCI_ForestGeneralist + BCI_InteriorForest + BCI_ForestGroundNester + BCI_OpenGroundNester + 
             BCI_ShrubNester +BCI_CanopyNester,
        BCI_Funcitonal=BCI_BarkProber + BCI_GroundGleaner + BCI_UpperCanopyForager + BCI_LowerCanopyForager + BCI_Omnivore,
        BCI_Compostional=BCI_NestPredator_BroodParasite + BCI_Exotic + BCI_Resident + BCI_TemperateMigrant + BCI_SingleBrooded,
        BCI = BCI_Structural + BCI_Funcitonal + BCI_Compostional,
        BCI_Category=c("Low Integrity", "Medium Integrity","High Integrity","Highest Integrity")[findInterval(BCI,
              vec=c(0,40.1,52.1,60.1,77.1))]
    )
if(!checklist) XBCI<-XBCI %>% select(-CheckList)  
    
    XBCI<-XBCI %>% rename(Point_Name=points)
    
    return(XBCI)

})