### visualise network statistics (from network_vis.R) on maps

require(tmap)
require(plyr)
require(magrittr)
require(rgeos)


##### maps of statistics #####

# create separate file for mapping
out_map <- out[!out$Site_code %in% sectors,] # remove offshore polygons

### betweenness
out_map@data %<>%  join(site_between)
qtm(out_map, fill='between')

## testing thresholds for betweenness
head(out_map[order(out_map$between, decreasing = T),],
     ceiling(nrow(polys)*0.01)+10)
thresh <- 5
out_map@data$bthresh <- factor(out_map$between>=thresh&out_map$Onshore=='Terrestrial')
qtm(out_map, fill='bthresh', title=paste0('threshold=',thresh))
# subset(out_map@data, between>=thresh & Onshore == 'Terrestrial')[,c(4,5,6,8)]
(site_between[order(site_between$between, decreasing = T),])


### clustering - not super interesting
out_map@data %<>% join(site_clus)
qtm(out_map, fill='clus')


### degree
out_map@data %<>% join(site_deg)
qtm(out_map, fill='degree')


w###### maps of movements ######

# do this using the centroids of polygons
midpts2 <- gCentroid(out, byid=T) %>% as.data.frame
rownames(midpts2) <- as.character(out@data$Site_ID) %>% make.unique

# centoid of origin and destination polygons for mapping
co <- midpts2[match(net_sort$Origin, rownames(midpts2)),]
cd <- midpts2[match(net_sort$Destination, rownames(midpts2)),]

# convert to sp::SpatialLines format
clines <- vector("list", nrow(co))
for(i in 1:nrow(co)) {
  clines[[i]] <- Lines(list(Line(rbind(co[i,], cd[i,]))), as.character(i))
}
clines <- SpatialLines(clines)

# plot this map - all movements
plot(out, col='lightblue')
plot(out[which(out$Onshore=='Terrestrial'),], col='lightgreen', add=T)
lines(clines, col='red', lwd=2)


## map only showing movements from feeding areas
ffeed <- which(out$Onshore[match( gsub('[.][[:digit:]]', '', rownames(co)), 
                            out$Site_ID)] == 'Offshore'  & 
                 out$Onshore[match( gsub('[.][[:digit:]]', '', rownames(cd)), 
                                    out$Site_ID)] == 'Terrestrial')
plot(out, col='lightblue')
plot(out[which(out$Onshore=='Terrestrial'),], col='lightgreen', add=T)
lines(clines[ffeed,], col='red', lwd=2)



###### write out map as shapefile #####
# require(rgdal)
# out_map$bthresh <- NULL
# writeOGR(out_map, dsn='Results/', layer='Network_stats_new', driver='ESRI Shapefile')
# write.csv(out_map@data, file='Results/Network_stats_new.csv', row.names = F)

##### write out list of sites exceeding thresholds #####
# res <- subset(out_map@data, between >= 4 | degree >= 4, 
#               select=c(Site_code, between, degree))
# res <- res[order(res$Site_code),]
# write.csv(res, file='Results/Core_network_sites.csv', row.names=F)


#### write out CSV of ESCP movements ####
## add site codes
# subset of coordinates
# coords <- subset(mpartial, select=c(ox, oy, dx, dy)) %>% na.omit
# 
# 
# # spatial file of origins and destinations
# o <- SpatialPoints(coords=coords[,1:2], proj4string = CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894'))
# d <- SpatialPoints(coords=coords[,3:4], proj4string = CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894'))
# od <- gUnion(o, d)
# 
# # where do they overlap?
# olap <- over(o, out) # why are there NAs?
# dlap <- over(d, out)
# 
# # add this info to mpartial dataset
# mpartial$OriginSite <- NA
# mpartial$OriginSite[match(rownames(olap), rownames(mpartial))] <- olap$Site_code %>% as.character
# mpartial$DestinationSite <- NA
# mpartial$DestinationSite[match(rownames(dlap), rownames(mpartial))] <- dlap$Site_code %>% as.character
# 
# escp.out <- subset(mpartial, select=c(Unique.Code, Date, Time, Species, Count, OriginSite, Oknown, DestinationSite, Dknown, Observer, Comments))
# escp.out <- subset(escp.out, !(is.na(OriginSite) & is.na(DestinationSite)))
# write.csv(escp.out, file='Results/ESCP_movements.csv', row.names = F)


### write out neat movement map
writeOGR(linemap2, dsn='Results/', layer='Neat_lines_map', driver='ESRI Shapefile')
