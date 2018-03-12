# predict which sites have high betweenness scores
# in order to extrapolate and identify other important sites in other areas

require(magrittr)
require(rgdal)
require(rgeos)
require(plyr); require(tmap) # for some sanity-checking plots at end only


# polygons in C&L SPA
clspa <- polys[bound, ]


# sites with betweenness scores only
bsites <- subset(polys, Site_ID %in% site_between$Site_ID)

plot(clspa)
plot(bsites, col='green', add=T)




##### distances to other sites #####
bdist <- gDistance(bsites, clspa, byid = TRUE)

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
avelength <- median ( gLength(shape, byid=T) )

bbuff <- gBuffer(bsites, byid = TRUE, width=avelength)


bdiff <- raster::erase(bbuff, bsites)
nonhabarea <- gArea(bdiff, byid=T)


buffarea <- gArea(bbuff, byid=T)
outarea <- buffarea - bsize


prop.habitat <- 1 - (nonhabarea / outarea)


# plot to sanity-check
phdf <- data.frame(rowcode=names(prop.habitat), prop=prop.habitat)
propmap <- clspa
propmap$rowcode <- rownames(propmap@data)
propmap@data %<>% join(phdf)
qtm(propmap, fill='prop')
propmap@data %<>% join(site_between)
with(propmap@data, plot(between, prop))
with(na.omit(subset(propmap@data, select=c(prop, between))), cor.test(prop, between))

