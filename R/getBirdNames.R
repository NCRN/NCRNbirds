#' @include NCRNbirds_Class_def.R
#'  
#' @title getBirdNames
#' 
#' @importFrom  dplyr group_by filter mutate select tbl_df
#' 
#' 
#' @description Translates between Latin name, common name and AOU code based on data from the \code{Species} slot. 
#' 
#' @param object A \code{NCRNbirds} object or a \code{list} of such objects.
#' @param names  A character vector containing the names to be translated.
#' @param in.style A length 1 character vector. Either "AOU", "Latin" or "common".  Indicates the type of name found in the "names" agrument. 
#' @param out.style A length 1 character vector. Either "AOU", "Latin", "common". Indicates the type of names to be returned. Defaults to "common"
#' # @param output Either "dataframe" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function.
#' 
#' @details This function converts a vector of names between the American Ornitholgoical Union Code (AOU Code), Latin name and scientific name. This is used by various other functions to provide the correct type of name in the output. The data to make these conversion is stored in the \code{Species} solot of a \code{\link{NCRNbirds}} object. 
#'  
#'  Currently the \code{list} method simply uses the data from the \code{Species} slot of the first object in the list.  
#'  
#' @export

 
setGeneric(name="getBirdNames",function(object,names=NA, in.style="AOU", out.style="common"){standardGeneric("getBirdNames")}, signature="object")


 setMethod(f="getBirdNames", signature=c(object="list"),
           function(object,names,in.style, out.style) {
             return(getBirdNames(object=object[[1]], names=names, in.style=in.style, out.style=out.style))
  
           })


setMethod(f="getBirdNames", signature=c(object="NCRNbirds"),
      function(object,names,in.style, out.style){
        
        XName<-object@Species
        
        
      #Scietific_Name, AOU_Code, Common_Name
      
        ColIn<-switch(in.style,
                      AOU=object@Species$AOU_Code,
                      Latin=object@Species$Scientific_Name,
                      common=object@Species$Common_Name
                      )
        
        ColOut<-switch(out.style,
                      AOU=object@Species$AOU_Code,
                      Latin=object@Species$Scientific_Name,
                      common=object@Species$Common_Name
        )
        
        return(ColOut[match(names,ColIn)])
      }
)

