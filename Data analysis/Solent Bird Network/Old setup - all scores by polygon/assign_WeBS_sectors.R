# Assign each polygon to its nearest WeBS Sector

require(rgdal)
require(rgeos)
require(magrittr)

#### The code below is commented out as it is analytically intensive,
#### and unnecessary to repeat. The results of the spatial analysis
#### are saved as a CSV, which is loaded below.

# webs <- readOGR(dsn='../../Solent Bird Studies GIS Data', layer='Solent_sections')
# 
# # Check both shapefiles exist and are on the same projection
# plot(webs, col='green')
# plot(polys, col='brown', add=T)
# 
# 
# # Calculate distance between objects
# webdist <- gDistance(polys, webs, byid=T)
# 
# webdist <- as.data.frame(t(webdist))
# colnames(webdist) <- webs$LOC_LABEL
# rownames(webdist) <- make.unique(as.character(polys$Site_ID))
# # save spadist as csv due to very lengthy calculation time!
# write.csv(webdist, file='Data/Distance_to_WeBS_Sector.csv', na='', row.names=T)


# this is nearly identical, except that a three WeBS sectors occur twice
# (presumably because their polygons are broken in two)
# no problem for our analysis and we can fix the naming convention later :)
webdist <- read.csv('Data/Distance_to_WeBS_Sector.csv', row.names = 1)

# which is the closest webs sector
closest_webs <- rep(NA, nrow(webdist))
ii <- 1
for (ii in 1:nrow(webdist)) {
  closest_webs[ii] <- names(webdist)[which.min(webdist[ii,])]
}
# change naming conventions to match polygon names
closest_webs <- gsub('^X', '', closest_webs) # remove leading X
closest_webs <- gsub('.1', '', closest_webs, fixed=T) # remove .1 in duplicate entries

polys@data %<>% cbind(closest_webs)
# if this doesn't work, there are more rows in polys@data than in closest
# as some polygons have duplicated Site_IDs
# in which case use the code below


# # which are duplicated?
# which(duplicated(polys$Site_ID))
# # are the duplicated rows just the same as the previous row?
# all.equal(polys@data[which(duplicated(polys$Site_ID)),]$Site_ID,
#           polys@data[(which(duplicated(polys$Site_ID))-1),]$Site_ID) # yes
# 
# # so, we can simply add the same data from the previous row
# polys$closest_webs <- NA
# polys$closest_webs[which(!duplicated(polys$Site_ID))] <- closest_webs
# polys$closest_webs[which(duplicated(polys$Site_ID))] <- closest_webs[(which(duplicated(polys$Site_ID))-1)]
