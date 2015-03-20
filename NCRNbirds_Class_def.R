setClass(Class="NCRNbirds",
         slots=c(
           ParkCode="character",
           ShortName="character",
           LongName="character",
           
           Network="character",
           
           Points="data.frame",
           Visits="data.frame",
           Birds="data.frame"
           ))