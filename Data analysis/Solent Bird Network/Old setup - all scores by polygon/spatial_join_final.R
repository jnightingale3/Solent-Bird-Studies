#### script to join bird survey results to survey polygons
#### uses final, clean, undissolved polygon layer

require(rgdal)
require(magrittr)
require(maptools) # for spatial rbind technique spRbind()

## read in data
polys <- readOGR(dsn='../../Solent Bird Studies GIS Data/Final Final SBS layer',
                 layer='solent_swbgs')
# plot(polys) # looks ok

# inspect
# summary(polys)




##### spatial join to solent survey data ####

head(solent_filter)

solent_filter$Species %<>% droplevels
# check all have co-ordinates
is.na(solent_filter$Easting)%>%sum # only 12 lack them
solent_filter <- solent_filter[!is.na(solent_filter$Easting),] # remove them

# convert to spatial
solent_space <- SpatialPointsDataFrame(coords=cbind(solent_filter$Easting, 
                                                    solent_filter$Northing),
                                       data=solent_filter,
                                       proj4string = polys@proj4string)
# plot(polys)
# points(solent_space, col='blue')

# clip records from outside solent polygons area
solent_over <- solent_space[polys, ]

# polygon attrbute data for each row in the solent_over dataset
solent_polys  <- over(solent_over, polys)

# join both attributes together
solent_join <- spCbind(solent_over, solent_polys)

# save data slot separately
solent_jdata <- solent_join@data


## clean up
rm(solent_space, solent_polys, solent_join)
