# solent_filter <- solent
# 
# head(solent_filter)
# # check all have co-ordinates
# is.na(solent_filter$Easting)%>%sum # only 12 lack them
# solent_filter <- solent_filter[!is.na(solent_filter$Easting),] # remove them
# 
# # convert to spatial
# solent_space <- SpatialPointsDataFrame(coords=cbind(solent_filter$Easting, 
#                                                     solent_filter$Northing),
#                                        data=solent_filter,
#                                        proj4string = polys@proj4string)

# writeOGR(solent_space, dsn=getwd(), layer='All_points_merged', driver='ESRI Shapefile')