# Assign each polygon to its nearest WeBS Sector

require(rgdal)
require(rgeos)
require(magrittr)

#### The code below is commented out as it is analytically intensive,
#### and unnecessary to repeat. The results of the spatial analysis
#### are saved as a CSV, which is loaded below.

webs <- readOGR(dsn='../../Solent Bird Studies GIS Data', layer='Solent_sections')

# Check both shapefiles exist and are on the same projection
# plot(webs, col='green')
# plot(polys, col='brown', add=T)


# use midpoints of polygons to speed calculation
# midpts <- gCentroid(polys, byid = T)
midweb <- gCentroid(webs, byid = T)

# Calculate distance between objects
webdist <- gDistance(solent_space, midweb, byid=T)

webdist <- as.data.frame(t(webdist))
colnames(webdist) <- webs$LOC_LABEL
# rownames(webdist) <- (as.character(polys$Site_code))
# save spadist as csv due to very lengthy calculation time!
write.csv(webdist, file='Data/Distance_to_WeBS_Sector.csv', na='', row.names=T)


webdist <- read.csv('Data/Distance_to_WeBS_Sector.csv', row.names = 1)
# 
# which is the closest webs sector
closest_webs <- rep(NA, nrow(webdist))
ii <- 1
for (ii in 1:nrow(webdist)) {
  closest_webs[ii] <- names(webdist)[which.min(webdist[ii,])]
}
# change naming conventions to match polygon names
closest_webs <- gsub('^X', '', closest_webs) # remove leading X
closest_webs <- gsub('.1', '', closest_webs, fixed=T) # remove .1 in duplicate entries

solent_space@data %<>% cbind(closest_webs)
