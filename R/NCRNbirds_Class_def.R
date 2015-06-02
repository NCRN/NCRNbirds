#' @title S4 Class Definition for NCRNbirds
#' 
#' @description
#' An S4 class that contains the data from bird monitoring from a set of montoirng points. This will typcially be a single park, but it could be any group of plots, including part of a park or a group of parks. 
#' @slot ParkCode A short code to designate the park (or group of plots), typically an NPS 4 letter code. Stored as a length 1 character vector. 
#' @slot ShortName A short name for the park. Stored as a length 1 character vector. 
#' @slot LongName  A long, formal name for the park. Stored as a length 1 character vector. 
#' @slot Network The code for the Inventory & Montoirng network (or other networ) the park belongs to. Stored as a length 1 character vector. 
#' @slot Points A \code{data.frame} with information on the mointoring points, such as names and lat/long.
#' @slot Visits A \code{data.frame} with information on each sampling visit.
#' @slot Birds  A \code{data.frame} with the bird monitoring data. 
#' 
#' @exportClass NCRNbirds

setClass(Class="NCRNbirds",
         slots=c(
           ParkCode="character",
           ShortName="character",
           LongName="character",
           
           Network="character",
           
           Points="data.frame",
           Visits="data.frame",
           Birds="data.frame"
           )
)
