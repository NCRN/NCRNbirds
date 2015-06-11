#' @include NCRNbirds_Class_def.R makeNCRNbirds.R
#' 
#' @title mapBirds
#' 
#' @importFrom leaflet addCircles addLegend addTiles leaflet colorBin colorFactor colorNumeric colorQuantile leaflet
#' @importFrom magrittr %>%
#' 
#' @description Produces an html map from a vector of point names and a vector of corresponding values.
#' 
#' @param object Either an object of class \code{NCRNbirds} or a list of such objects
#' @param points A character vector of plot names. 
#' @param values  A vector of values to map. Can be either numerical data or categories.
#' @param maptype The type of base map to be used. Options are:
#'  \describe{
#'  \item{"basic"}{The default, uses the basic Park Tiles map, similar to the maps found in park brochures.}
#'  \item{"imagery"}{The Park Tiles satellite imagery map.}
#'  \item{"slate"}{The Park Tiles slate map. A very muted and grey base map.}
#' }
#' @param colorgroups The number of different colors to display for maps with a \code{colortype} of "bin" or "quantile", or a series of cut points to be used with \code{colortype="bin"} See discussion below.
#' @param radius The radius in meters, of the circle drawn on the map. When the map is zoomed out the circles will be visible regardless of the value chosen.
#' @param opacity Opacity of the circles on the map. A single number from 0 (completely transparent) to 1 (completely opaque). Defaults to 1.
#' @param colortype Indicates the method of assiging colors to each plot. Make use of the leaflet package's color funcitons:
#'  \describe{
#'    \item{"quantile"}{Divides the data into the number of groups indicated in \code{colorgroups} based on quantiles of the data. An approximately equal number of plots will be in each group.  Each group will be given a different color.}
#'    \item{"bin"}{Divides the data into groups based on the \code{cut} function. Like the "quantile" option in divides the plots into groups, but in this case each group should cover an approximately equal range in values of the data. Alternatively if \code{colorgroups} is a vector rather than a single number, the elements of the vector will be the cut points separating the groups to be mapped.}
#'    \item{"numeric"}{Colors the plots based on amp smooth color ramp, rather than dividing into groups.}
#'    \item{"factor}{Used when \code{values} are categorical data.}
#'  }
#' @param colors  A character vector of one or more colors for the plots on the map. See discussion below.
#' @param title  A character vector to be used as the lengend title. 
#' 
#' @details  This function is serves as a wrapper for the leaflet package. It quickly creates a map by plotting the locations of the points on the ParkTiles base map from the NPS. 
#' 
#' Several option exist for coloring the plots based on the \code{colorNumeric}, \code{colorBin}, \code{colorQuantile}, and \code{colorFactor} functions in leaflet.
#' 
#' quantile - This option will divide the plots into the equal quantiles based on the data in \code{values}. The \code{colorgroups} agrument indicates the number of gourps the plots will be divided into. For exmple if \code{colorgroups=4} then the plots with the bottom quarter of \code{values} will get one color, the second quarter of \code{values} will get a second color etc. An erorr may occur if it is not possible to divide the \code{value} vector in to the indicate number of groups. For example, if half of \code{values} is the same number, than it cannot be divided into thirds.
#' 
#' bin - This option also divides the plots into groups, but in this case the groups either cover an equal range in \code{values} or cover ranges specificed in \code{colorgroups}. If \code{colorgroups} is a single value, than that will indicate the number of groups to divide \code{values} into. For example, if \code{values} range from 1 to 100, and \code{colorgroups} is 4, the plots with values 1-25 will be one color, 26-50 a second and so on. If instead \code{colorgroups} is \code{c(-1,10,50,75,101)} then plots from 1-10 will be one color, 11-50 a second and so on. 
#' 
#' numeric - This option does not divide the plots into groups. Rather plots will have a smooth ramp of colors from the lowest to the highest value.
#' 
#' factor - This well color the plots when \code{values} is categorical data (e.g. soil or vegetation type) rather than numeric. Each category gets a different color. 
#' 
#'The \code{colors} agrument indicates which colors will be chosen for the plots. This is a charcter vector and can either be standard R color names ("blue", "green" etc.) or hexadecimal colors ("#0000FF","008000",etc). If the number of colors is equal to the number of groups that each group will get the corresponding color. If there are fewer colors than groups, than a colorramp will be created using the indicate colors and used for the plots. Typically maps made with the \code{"numeric"} option will make use of the color ramp.
#'
#'Currently there is no legend displayed with the map. However, if you click on a circle it will give you the Plot Name and the value associated with the plot.
#' 
#' @export

setGeneric(name="mapBirds",function(object,points,values, maptype="basic", colorgroups=8,radius=30,opacity=1,colortype="quantile",
                                     colors=c("cyan","magenta4","orangered3"),title=deparse(substitute(values)),...){standardGeneric("mapBirds")}, signature="object")

setMethod(f="mapBirds", signature=c(object="list"),
          function(object,points,values,maptype,colorgroups,radius,opacity,colortype,colors,title,...){
            TempPark<-makeNCRNbirds(object,ParkCode="TEMPOBJ", ShortName="TempObj",LongName="Temp park Map", Network="TempMap")
            return(mapBirds(object=TempPark, points=points,values=values,maptype=maptype, colorgroups=colorgroups,radius=radius,
                             opacity=opacity,colortype=colortype,colors=colors,title=title,...))
          })


setMethod(f="mapBirds", signature=c(object="NCRNbirds"),
          function(object,points,values,...){
            
            BaseMap<-switch(maptype,
                            basic="//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
                            imagery="//{s}.tiles.mapbox.com/v4/nps.map-n9nxe12m,nps.gdipreks,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",
                            slate="//{s}.tiles.mapbox.com/v4/nps.68926899,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q"
            )
            
            MapCol<-switch(colortype,
                           quantile=colorQuantile(palette=colors, n=colorgroups,domain=values),
                           bin=colorBin(palette=colors,bins=colorgroups,domain=values),
                           numeric=colorNumeric(palette=colors,domain=values),
                           factor=colorFactor(palette=colors,domain=values)
            )
            
            BirdMap<-leaflet() %>%
              addTiles(urlTemplate=BaseMap) 
            BirdMap%>%
              addCircles(data=getPoints(object=object,points=points), color=MapCol(values), fillColor=MapCol(values), radius=radius,
                        opacity=opacity, fillOpacity=opacity, popup=paste(points,":",as.character(values))) %>%
              addLegend(pal=MapCol, values=values,title=title)
          })
