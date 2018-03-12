## defines a function called gClip, which trims spatial data by whether or not it
## falls within the bounding box (bbox) of another spatial object

## by Robin Lovelace
## http://robinlovelace.net/r/2014/07/29/clipping-with-r.html

require(raster)
library(rgeos)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}