## Depends on maps_for_bird_meeting.R

## Plots thematic maps indicating the importance of sites and confidence of these 
## assessments.
## Not currently very visually satisfying - needs further work.

require(ggplot2)
require(ggmap)
require(magrittr)
require(rgeos)
require(maptools)

#### download maps ####
maptoner <- get_map(location=c(lon=-1.2, lat=50.85), zoom=10, maptype = 'toner',
                    source='stamen')
# ggmap(maptoner)


### convert East/Northings to lat-long for ggmap
library(rgdal)
# prepare UTM coordinates matrix
utmcoor<-SpatialPoints(cbind(solent_jdata$Easting,solent_jdata$Northing), proj4string=polys@proj4string)
# converting to latlon
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
# add to original data frame
solent_jdata$coords.x1 <- longlatcoor@coords[,1]
solent_jdata$coords.x2 <- longlatcoor@coords[,2]

#### waders ####

# convert spatial object to data frame
poly.f <- polys %>% fortify(region='Site_ID')
# merge with polygon attributes
wad_poly <- merge(poly.f, polys@data, by.x = 'id', by.y = 'Site_ID')

# create Site_ID column
wad_poly$Site_ID <- wad_poly$id

# convert eastings/northings to decimal coordinates
# for use with ggmap / google maps etc
utmcoor<-SpatialPoints(cbind(wad_poly$long,wad_poly$lat), proj4string=polys@proj4string)
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
wad_poly[,2:3] <- longlatcoor@coords

## join to wader site data
wad_poly <- join(wad_poly, wader_sites, by="Site_ID")


## plot on a map
pwads <- ggmap(maptoner) +
  geom_polygon(data=wad_poly,
               aes(x=long, y=lat, group=group,fill=Importance, alpha=Confidence)) +
  scale_alpha(range=c(0.2,1)) + scale_y_continuous(limits=c(50.7, 50.9))
print(pwads)

#### brents ####

# repeat as above - join polygons with attributes in data frame
bg_poly <- merge(poly.f, polys@data, by.x = 'id', by.y = 'Site_ID')

# add Site_ID
bg_poly$Site_ID <- bg_poly$id
# convert coordinates
utmcoor<-SpatialPoints(cbind(bg_poly$long,bg_poly$lat), proj4string=polys@proj4string)
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
bg_poly[,2:3] <- longlatcoor@coords

## join with brent site data
bg_poly <- join(bg_poly, brent_sites, by="Site_ID")


## plot on a map
pbg <- ggmap(maptoner) +
  geom_polygon(data=bg_poly,
               aes(x=long, y=lat, group=group,fill=Importance, alpha=Confidence)) +
  scale_alpha(range=c(0.2,1)) + scale_y_continuous(limits=c(50.7, 50.9))
print(pbg)
