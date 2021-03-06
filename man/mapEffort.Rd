% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapEffort.R
\name{mapEffort}
\alias{mapEffort}
\title{mapEffort}
\usage{
mapEffort(
  object,
  years = NA,
  points = NA,
  palette = "BuGn",
  maptype = "basic",
  title = "Visits per Point",
  ...
)
}
\arguments{
\item{object}{Either an object of class \code{NCRNbirds} or a list of such objects.}

\item{years}{Optional. A vector of numeric value(s) indicating the year of the data you wish to map. \code{NA}, the default, combines data across all
years.}

\item{palette}{Color pallete for the colors of the points. Defaults to "BuGn" (blue green) but will accept any RColorBrewer, viridis or custom palette.}

\item{maptype}{The type of base map to be used. See \code{\link{mapBirds}} for options.}

\item{title}{A character vector to be used as the legend title.}

\item{...}{Additional arguments passed to \code{\link{summarizeEffort}}.}
}
\description{
Produces an html map of the number of visits made to each point.
}
\details{
This function prodcues a map showing number of visits to each point for one or more years. The number of visits 
is calculated using the \code{\link{summarizeEffort}} function, and this data is then passed on to the \code{\link{mapBirds}} 
function.
}
\seealso{
\code{\link{summarizeEffort}} \code{\link{mapBirds}}
}
