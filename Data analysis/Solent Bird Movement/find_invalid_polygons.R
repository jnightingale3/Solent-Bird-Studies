#### script for finding invalid polygons

require(rgeos)
require(rgdal)

# terrestrial sites - ESCP polygon layer
polys <- readOGR(dsn='../../Solent Bird Studies GIS Data/Final Final SBS layer',
                 layer='solent_merged')

polys@data[which(!gIsValid(polys, byid=T)),] # list all invalid geometries

# warnings() %>% head # what's wrong with first ones

