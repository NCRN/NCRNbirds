#' @title S4 Class Definition for NCRNbirds
#' 
#' @description
#' An S4 class that contains the data from bird monitoring at a set of monitoring points. This will typcially be a single park, but it could be any group of plots, including part of a park or a group of parks. 
#' @slot ParkCode A short code to designate the park (or group of plots), typically an NPS 4 letter code. Stored as a length 1 character vector. 
#' @slot ShortName A short name for the park. Stored as a length 1 character vector. 
#' @slot LongName  A long, formal name for the park. Stored as a length 1 character vector. 
#' @slot Network The code for the Inventory & Montoirng network (or other network) the park belongs to. Stored as a length 1 character vector. 
#' @slot VisitNumber A \code{numeric} vector of length 1. The number of visits that is typically made to each point. This serves as the default number of visits for other functions.
#' @slot Bands A \code{data.frame} with metadata for the distance bands used during monitoring. Includes a name, min distance and max distance for each band.
#' @slot Points A \code{data.frame} with metadata for the monitoring points, such as names and lat/long.
#' @slot Visits A \code{data.frame} with metadata for each sampling visit.
#' @slot Birds  A \code{data.frame} with the bird monitoring data. Each row is an observation of a number of individuals of a particular species at a particular point, visit, time interval and distance band.
#' @slot Species A \code{data.frame} with metadata about the bird species, including AOU (American Ornithological Union) Code, Latin and common names, as well as guild assignments for the BCI calculations.
#' 
#' @exportClass NCRNbirds

setClass(Class="NCRNbirds",
         slots=c(
           ParkCode="character",
           ShortName="character",
           LongName="character",
           
           Network="character",
           
           VisitNumber="numeric",
           Bands="data.frame",
           
           Points="data.frame",
           Visits="data.frame",
           Birds="data.frame",
           Species='data.frame'
           )
)
