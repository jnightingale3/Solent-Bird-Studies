# join polygons to WeBS low tide sectors
# clip to Langstone harbour

require(rgdal)
require(rgeos)
require(raster) # for extent()
require(magrittr)



# terrestrial sites - HIWWT polygon layer
polys <- readOGR(dsn='../../Solent Bird Studies GIS Data/HIWWT Strategy Layer',
                 layer='SWBGS')
polys$Onshore <- 'Terrestrial'



# offshore sites - WeBS sectors for Chichester and Langstone Harbours
cl <- readOGR(dsn='../../Solent Bird Studies GIS Data',
                layer='Solent_sections')
cl <- cl[cl$LOC_LABEL %in% sectors,]
# cl %<>% gSimplify(tol=0.1)
# cl %<>% gBuffer(byid=TRUE, width=0)

# currently claiming a ring intersection in an area with no geometry! :(
# just have to crack on without the map I guess


## this was needed when low tide sectors were loaded separately; now deprecated
# # remove unneeded columns and merge offshore sites
# chic$LABEL <- NULL
# lang$AREA <- NULL
# cl <- rbind(chic, lang)
# cl$COUNT <- NULL
# names(cl) <- c('Site_ID', 'Easting', 'Northing')
# rm(chic, lang)
# cl$Onshore <- 'Offshore'
# 
# # add additional columns not found in cl but which are in polys
# cols <- which(!names(polys)%in%names(cl))
# newcols <- matrix(rep(NA, (length(cols)*nrow(cl))), nrow=nrow(cl))
# colnames(newcols) <- names(polys)[cols]
# cl@data %<>% cbind(newcols)
# cl@data <- cl@data[,match(names(polys),names(cl))]
# rm(cols, newcols)



# clip polys

# fix problem of invalid polygons
# all are valid now!
y <- gIsValid(polys, byid=T) # which are they?
# plot(polys) # plot all
# plot(polys[!y,], col='red', add=T) # highlight invalid ones in red
# 
# # 'simplifying' the polygons seems to fix this issue (a known hack)
# polys_s <- gSimplify(polys, tol = 0.00001)


# save list of invalid polygons so that they can be fixed
# write.csv(polys@data$Site_ID[!y], file='Invalid_polys_15Jun.csv', row.names = F)


# remove overlap between cl and polys
c.diff <- gDifference(cl, polys[y,], byid=c(T,F))
c.diff <- SpatialPolygonsDataFrame(c.diff, data=data.frame(
  Site_code = cl$LOC_LABEL,
  Easting = NA,
  Northing=NA,
  Area_ha = NA,
  Onshore = 'Offshore',
  row.names=getSpPPolygonsIDSlots(c.diff)
))


# merge
netwk <- rbind.SpatialPolygonsDataFrame(c.diff, polys[y,], makeUniqueIDs = TRUE)

# netwank <- gUnion(polys, cl, byid=T)

# clean up
rm(cl, c.diff)
