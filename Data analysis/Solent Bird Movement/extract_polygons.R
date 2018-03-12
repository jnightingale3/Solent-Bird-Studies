## do a network analysis of the network

require(raster) # for extent()
require(magrittr)
require(rgdal)
require(rgeos)
require(plyr)


# read data
m <- move_network

# subset of coordinates
coords <- subset(m, select=c(ox, oy, dx, dy)) %>% na.omit

# spatial file of origins and destinations
o <- SpatialPoints(coords=coords[,1:2], proj4string = netwk@proj4string)
d <- SpatialPoints(coords=coords[,3:4], proj4string = netwk@proj4string)
od <- gUnion(o, d)
# od <- od[which(od@coords[,1] >= 450000), ] # remove extreme records from near Tichfield


# plot(out)
# points(o, col='green')
# points(d, col='red')



# make boundary
netsub <- subset(netwk, (Site_code %in% net$Origin) | (Site_code %in% net$Destination))
bound <- as(extent(netsub), 'SpatialPolygons')
proj4string(bound) <- od@proj4string
bound <- gBuffer(bound, width=1000) # units are metres?

# check it out on full map
plot(netwk)
plot(bound, add=T, col='green') # looks good


# clip netwk to only be in this area
out <- netwk[bound, ]



# check it out with lines
plot(out)
lines(shape, col='red')




# where do they overlap?
olap <- over(o, out) # why are there NAs?
dlap <- over(d, out)
## There are NAs where the line start or endpoint is in a gap in the underlying
## geometry - either onland where there is no strat field, or offshore where 
## there are holes in the WeBS data set
# we should probably fix this at some point


#### DATASET TO BE USED FOR NETWORK ANALYSIS
# add origin and destination polygons to a dataset
net <- data.frame(
  Origin = olap$Site_code,
  Destination = dlap$Site_code
) %>% droplevels
