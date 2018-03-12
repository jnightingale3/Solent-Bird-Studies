## extract species records

# species of interest
spoi <- 'Brent Goose (Dark-bellied)'

x <- subset(solent_space, Species==spoi & Count > 1, select=c(Easting, Northing, Date, Time, Species, Count, Sitename, Activity, Polygon, Describe, Habitat, Protect, Disturb, Recorder))

write.csv(x, file=paste0(spoi, '_records.csv'), row.names=F)

writeOGR(x, dsn=getwd(), layer=paste0(spoi, '_records'), driver='ESRI Shapefile')


