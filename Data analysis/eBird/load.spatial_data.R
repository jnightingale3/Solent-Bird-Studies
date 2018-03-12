#### Script to load shapefiles and filter ebird records by locations

require(rgdal) # to load shapefiles
require(magrittr) # |
require(rgeos) # gDistance()

## load Solent Birds and WeBS polygons

sbs <- readOGR(dsn='../../Solent Bird Studies GIS Data/Final SBS GIS layer',
               layer='removeduplicates3', stringsAsFactors = FALSE)
bto <- readOGR(dsn='../../Solent Bird Studies GIS Data/', 
               layer='Solent_sections', stringsAsFactors = FALSE)
# remove Pagham sections
bto %<>% subset(NAME %in% c('Pagham - Harbour', 'Pagham Lagoon', 'Pagham Sea',
                            'North Fields','Severals', 'Sidlesham Ferry') == F)


## make ebird data spatial

spacebird <- SpatialPointsDataFrame(coords=cbind(ebird$LONGITUDE, ebird$LATITUDE), 
                                    data=ebird, 
                                    proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  # convert to same coordinate system as BTO shapefile
  spTransform(CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894'))


# spacewebs <- spacebird[bto, ]

max_dist <- 2500 # sites must be within 2.5km of the coast

ebird$bto_dist <- apply( gDistance(spacebird, bto, byid=T), 2, min )

# points close enough to BTO WeBS sector (2.5km) and west of Selsey Bill
ebird$buffer <- (ebird$bto_dist <= max_dist) & (ebird$LONGITUDE <= -0.787992)
sum(ebird$buffer)

### define spacebird again, only including points within 2.5km of the coast
spacebird <- SpatialPointsDataFrame(coords=with(ebird, cbind(LONGITUDE[buffer], 
                                                             LATITUDE[buffer])), 
                                    data=subset(ebird, buffer),
                                    proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  # convert to same coordinate system as BTO shapefile
  spTransform(CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894'))

# plot(bto); points(spacebird, col='blue') # check
