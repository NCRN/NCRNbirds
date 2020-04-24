#' @include NCRNbirds_Class_def.R getParkNames.R
#' @title matchParkCodes
#' @importFrom purrr map_chr
#' 
#' @description Matches a stirng of park codes with the corresponding park codes.
#' 
#' @param object Either an NPSbirds object or a list of such objects.
#' @param parkNames A vector of Park codes
#' @param name.class Type of name to return. One of three options, in quotes.
#' \describe{
#' \item{"short"}{ The default. Returns the short name of the park}
#' \item{"long"}{Returns the long name of the park}
#' \item{"code"}{Returns the park code}
#' } 
#' @return A character vector with one or more park names. 
#' 
#' @export

setGeneric(name="matchParkCodes",function(object,parkNames, name.class="short"){standardGeneric("matchParkCodes")},signature=c("object") )


setMethod(f="matchParkCodes", signature=c(object="list"),
          function(object,parkNames, name.class){

            GoodNames<-getParkNames(object, name.class=name.class)
            ParkCodes<-getParkNames(object, name.class="code")
            map_chr(parkNames, ~GoodNames[which(.x==ParkCodes)])
})

setMethod(f="matchParkCodes", signature=c(object="NCRNbirds"),
          function(object,parkNames, name.class){
            
            GoodNames<-getParkNames(object, name.class=name.class)
            ParkCodes<-getParkNames(object, name.class="code")
            map_chr(parkNames, ~GoodNames[which(.x==ParkCodes)])
          })