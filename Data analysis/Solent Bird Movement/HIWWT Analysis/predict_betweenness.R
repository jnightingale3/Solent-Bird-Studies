# calculate betweenness scores and then predict them from site characteristics

## basically, the data is too messy, so we can only use the scores that we
# actually have!

require(rgeos)
require(magrittr)
require(rgdal)

# polygons
polys <- stratcode

# pnly sites with betweenness scores
bsites <- subset(polys, SITE_ID %in% between$name)
bsites <- bsites[!duplicated(bsites$SITE_ID),]

# will subset polygons to include only those that are nearby
bound <- as(raster::extent(bsites), 'SpatialPolygons')
proj4string(bound) <- bsites@proj4string
bound %<>% gBuffer(width=3000) # units are metres?



## subset of polygons that are close to HIWWT survey results
hsites <- # not to be confused with shites
  polys[bound, ]


# have a look at this magic on a map
plot(hsites)
plot(bsites, add=T, col='green')




#### distance to (any) other site ####
bdist <- gDistance(bsites, hsites, byid = TRUE)

# 
for (ii in colnames(bdist)) {
  bdist[ii,ii] <- NA
}


bdist_min <- rep(NA, ncol(bdist))
for (ii in 1:ncol(bdist)) {
  bdist_min[ii] <- min(bdist[,ii], na.rm=T)
}



###### size of site #####
bsize<- gArea(bsites, byid=T)




##### buffer - containing other sites #####

# median distance of movements
# avelength <- median ( gLength(shape, byid=T) )
avelength <- 1200 # for now. 

bbuff <- gBuffer(bsites, byid = TRUE, width=avelength) %>% gSimplify(tol = 0.00001)


bdiff <- raster::erase(bbuff, gSimplify(bsites, tol = 0.00001))
nonhabarea <- gArea(bdiff, byid=T)


buffarea <- gArea(bbuff, byid=T)
outarea <- buffarea - bsize


prop.habitat <- 1 - (nonhabarea / outarea)


##### predict if site is in top 10% of sites ####

is.top.10 <- rank(between$between) >= max(rank(between$between))*0.9
lm(rank(between$between) ~ prop.habitat + bdist_min + bsize) %>% summary


# make data frame
resdf <- data.frame (
  betw = between$between,
  size = (bsize),
  dist = (bdist_min),
  phab = prop.habitat,
  tten = is.top.10)
)

pairs(resdf)


glm(tten ~ size + dist + phab, data=resdf, family=binomial(link = "logit")) %>% summary
