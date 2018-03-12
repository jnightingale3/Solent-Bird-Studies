require(plyr)
require(rgdal)
require(maptools) # for spatial rbind technique spRbind()
require(lubridate)

##### how many records in total to each site #####

today <- Sys.Date()
limit <- today - years(5)
solent5y <- subset(solent, Date >= limit)


# check all records have co-ordinates
is.na(solent5y$Easting)%>%sum # 12 lack them
solent5y <- solent5y[!is.na(solent5y$Easting),] # remove them


# convert to spatial
solent5y_outerspace <- SpatialPointsDataFrame(coords=cbind(solent5y$Easting, 
                                                           solent5y$Northing),
                                              data=solent5y,
                                              proj4string = polys@proj4string)

# clip records from outside solent5y polygons area
solent5y_overlap <- solent5y_outerspace[polys, ]

# polygon attrbute data for each row in the solent5y_over dataset
solent5y_polyflap  <- over(solent5y_overlap, polys)

# join both attributes together
solent5y_allrex <- spCbind(solent5y_overlap, solent5y_polyflap)


## calculate all the visits
nnvisits <- ddply(solent5y_allrex@data, .(Site_code, visitid), nrow)
nsitevisits <- ddply(nnvisits, .(Site_code), nrow)
names(nsitevisits)[2] <- 'Days'


##### extract records of non-target species #####

stratspp <- c('Brent Goose (Dark-bellied)', waders)

justnegs <- ddply(solent, .(visitid), function(x) {
  any(x$Species %in% stratspp)
})

allnegs <- subset(solent, visitid %in% subset(justnegs, V1==F)$visitid) %>%
  droplevels

table(allnegs$Species)

write.csv(allnegs, 'SolentBirds_Negative-Records.csv', row.names = F)
