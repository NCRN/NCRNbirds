% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapRichness.R
\name{mapRichness}
\alias{mapRichness}
\title{mapRichness}
\usage{
mapRichness(
  object,
  years = NA,
  points = NA,
  palette = "BuGn",
  maptype = "basic",
  title = "Species Richness",
  ...
)
}
\arguments{
\item{object}{Either an object of class \code{NCRNbirds} or a list of such objects.}

\item{years}{Optional. A vector of numeric value(s) indicating the year of the data you wish to map. \code{NA}, the default, combines data across all
years.}

\item{points}{A character vector of point names. \code{NA}, the default, will map all points.}

\item{palette}{Color pallete for the colors of the points. Defaults to "BuGn" (blue green) but will accept any RColorBrewer, viridis or custom palette.}

\item{maptype}{The type of base map to be used. See \code{\link{mapBirds}} for options.}

\item{title}{A character vector to be used as the legend title.}

\item{...}{Additional arguments passed to \code{\link{birdRichness}}.}
}
\description{
Produces an html map of bird species richness.
}
\details{
This function prodcues a map showing the bird species richness of each point for one or more years. Bird species richness is calculated using the 
\code{\link{birdRichness}} function, and this data is then passed on to the \code{\link{mapBirds}} function.
}
\seealso{
\code{\link{birdRichness}} \code{\link{mapBirds}}
}
