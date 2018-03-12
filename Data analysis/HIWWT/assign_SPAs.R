# Assign each site to its nearest SPA

require(rgdal)
require(rgeos)
require(magrittr)

#### The code below is commented out as it is analytically intensive,
#### and unnecessary to repeat. The results of the spatial analysis
#### are saved as a CSV, which is loaded below.

spa <- readOGR(dsn='../../Solent Bird Studies GIS Data/UK SPAs with Marine Components',
               layer='UKSPAswithMarineComponents_WGS84_b')
spa <- spa[spa@data$SiteName %in% c('Solent and Southampton Water', 'Portsmouth Harbour'
                                    , 'Chichester and Langstone Harbours'), ]

# change projecttion to from long-lat to UTM
spa2 <- spTransform(spa, polys@proj4string)
# overwrite original
spa <- spa2; rm(spa2)
spa@data %<>% droplevels

# use midpoints to speed analysis
# midpts <- gCentroid(polys, byid = T)
midspa <- gCentroid(spa, byid = T)

spadist <- gDistance(solent_space, midspa, byid=T)

spadist <- as.data.frame(t(spadist))
colnames(spadist) <- spa$SiteName
# save spadist as csv due to very lengthy calculation time!
write.csv(spadist, file='Data/Distance_to_SPA2.csv', na='', row.names=F)

# # load previously-calculated values from file
spadist <- read.csv('Data/Distance_to_SPA2.csv')
# 
closest <- rep(NA, nrow(spadist))
ii <- 1
for (ii in 1:nrow(spadist)) {
  closest[ii] <- names(spadist)[which.min(spadist[ii,])]
}


solent_space@data %<>% cbind(closest) # add to main data set

