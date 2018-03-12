## Find the closest tide gauge to each polygon

require(rgdal)
require(rgeos)
require(magrittr)
require(plyr)


gauge <- read.csv('Data/gauge_locations.csv') 

gauges <- SpatialPointsDataFrame(coords=gauge[,2:3], data=gauge,
                                 proj4string = polys@proj4string)

# plot(polys)
# points(gauges, col='red')

gaugedist <- gDistance(polys, gauges, byid=T)
gaugedist %<>% t %>% as.data.frame
names(gaugedist) <- gauges$Name

gaugedist$Closest <- NA
ii <- 1
for (ii in 1:nrow(gaugedist)) {
  gaugedist$Closest[ii] <- gauges$Name[which.min(gaugedist[ii,])] %>% as.character
}
# gaugedist$Closest %<>% factor

polys@data %<>% cbind(gaugedist)



# if it doesn't work, see below, as per SPA and WeBS sector

# polys$Closest <- NA
# polys$Closest[which(!duplicated(polys$Site_ID))] <- gaugedist$Closest
# polys$Closest[which(duplicated(polys$Site_ID))] <- gaugedist$Closest[(which(duplicated(polys$Site_ID))-1)]
# 

polygauge <- subset(polys@data, select=c(Site_code, Closest))
solent_jdata %<>% join(polygauge)

# check it looks plausible on a map
# require(tmap)
# qtm(polys, fill='Closest')

rm(gaugedist, gauges, polygauge)
