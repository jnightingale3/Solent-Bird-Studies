# Assign each site to its nearest SPA

require(rgdal)
require(rgeos)
require(magrittr)

#### The code below is commented out as it is analytically intensive,
#### and unnecessary to repeat. The results of the spatial analysis
#### are saved as a CSV, which is loaded below.

# spa <- readOGR(dsn='../../Solent Bird Studies GIS Data/UK SPAs with Marine Components',
#                layer='UKSPAswithMarineComponents_WGS84_b')
# spa <- spa[spa@data$SiteName %in% c('Solent and Southampton Water', 'Portsmouth Harbour'
#                                     , 'Chichester and Langstone Harbours'), ]
# 
# # change projecttion to from long-lat to UTM
# spa2 <- spTransform(spa, polys@proj4string)
# # overwrite original
# spa <- spa2; rm(spa2)
# spa@data %<>% droplevels
# 
# 
# spadist <- gDistance(polys, spa, byid=T)
# 
# spadist <- as.data.frame(t(spadist))
# colnames(spadist) <- spa$SiteName
# # save spadist as csv due to very lengthy calculation time!
# write.csv(spadist, file='Data/Distance_to_SPA.csv', na='', row.names=F)

# load previously-calculated values from file
spadist <- read.csv('Data/Distance_to_SPA.csv')

closest <- rep(NA, nrow(spadist))
ii <- 1
for (ii in 1:nrow(spadist)) {
  closest[ii] <- names(spadist)[which.min(spadist[ii,])]
}


polys@data %<>% cbind(closest) # add to main data set
# if this doesn't work, it's because there are more rows in polys@data 
# than in closest, as some polygons have duplicated Site_IDs
# in which case use the code below


# # which are duplicated?
# which(duplicated(polys$Site_ID))
# # are the duplicated rows just the same as the previous row?
# all.equal(polys@data[which(duplicated(polys$Site_ID)),]$Site_ID,
# polys@data[(which(duplicated(polys$Site_ID))-1),]$Site_ID) # yes
# 
# # so, we can simply add the same data from the previous row
# polys$closest <- NA
# polys$closest[which(!duplicated(polys$Site_ID))] <- closest
# polys$closest[which(duplicated(polys$Site_ID))] <- closest[(which(duplicated(polys$Site_ID))-1)]

