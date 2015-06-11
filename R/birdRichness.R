#' @include NCRNbirds_Class_def.R getBirds.R
#' 
#' @title birdRichness
#' 
#' @importFrom dplyr n_distinct
#' 
#' @description Returns the number of bird species found in a park, at a point or a collection of points. 
#' 
#' @param object An NCRNbirds object or a list of such objects.
#' @param points A character vector passed to \code{\link{getBirds}}. The names of one or more points where the data was collected. \code{NA}(the default) meausures richenss from all ponits.
#' @param AOU  A character vector passed to \code{\link{getBirds}}. One or more AOU (American Ornothological Union) codes of bird species. \code{NA} (the default) measures richness based on all species, otherwise only species listed in \code{AOU} will count.
#' @param years  A vector of numbers passed to \code{\link{getBirds}}. \code{NA} (the default) will count species regardless of the year they were monitored, otherwise only data from years listed in \code{years} will be used.
#' @param output Either "total" (the default) or "list". Note that this must be in quotes. Determines the type of output from the function, when \code{object} is a \code{list}. "Total" will give the number of distinct species found across all parks. "List" will return a list, with each entry to the list corresponding to the species richness of one of the \code{NCRNbirds} objects in the input list.  
#' @param ... addtional arguments passed to \code{\link{getBirds}}. Any argument which is a valid argument for \code{\link{getBirds}} can be used here.
#' 
#' @details This funciton calculates the species richness for a park, group of parks, group of monitoring points etc. Species richness is the number of different species found, regardless of their abundance. The function works by first getting the monitorng data by using the \code{\link{getBirds}} funciton and then conting the number of unique birds found. All of the options for combinign or subsetting data in \code{\link{getBirds}} will work for \code{birdRichness} including subsetting by year and monitoring point. 
#' 
#' @export
#'

setGeneric(name="birdRichness",function(object,points=NA,AOU=NA,years=NA,output="total",...){standardGeneric("birdRichness")}, signature="object")

setMethod(f="birdRichness", signature=c(object="list"),
          function(object,points,AOU,years,output,...) {
            switch(output,
                   list= return(
                     lapply(X=object, FUN=birdRichness, points=points,AOU=AOU,years=years,output=output,...)
                     ),
                   total=return(n_distinct(getBirds(object=object,points=points,AOU=AOU,years=years,output="dataframe",...)$AOU_Code))
            )
          })


setMethod(f="birdRichness", signature=c(object="NCRNbirds"),
          function(object,points,AOU,years,output,...){
            n_distinct(getBirds(object=object,points=points,AOU=AOU,years=years,output=output,...)$AOU_Code)
          }
)