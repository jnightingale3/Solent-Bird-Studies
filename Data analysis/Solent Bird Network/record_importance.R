# Assign importance scores to each record in the total Solent dataset

require(rgeos)
require(rgdal)
require(magrittr)



#### turn collated records into spatially-linked dataset

#head(solent_filter)

solent_filter$Species %<>% droplevels

# check all have co-ordinates
is.na(solent_filter$Easting)%>%sum # 0 lack them
solent_filter <- solent_filter[!is.na(solent_filter$Easting),] # remove them




# convert to spatial
solent_space <- SpatialPointsDataFrame(coords=cbind(solent_filter$Easting, 
                                                    solent_filter$Northing),
                                       data=solent_filter,
                                       proj4string = polys@proj4string)




## need to find all records from *inside* the Oysterbeds
# and assign them new co-ordinates as the strategy
# site only includes the bunds, and so misses many records

# load shapefile covering all oysterbeds area
oyc_overlap <- readOGR('../../Solent Bird Studies GIS Data', 'Oysterbeds_overla')
# identify all records from that point
solent_oyc <- gIntersects(solent_space, oyc_overlap, byid = T) %>% t
# and assign new co-ordinates
solent_filter$Easting[(solent_oyc[,1])] <- 471627.3 # H73A
solent_filter$Northing[(solent_oyc[,1])] <- 103746.2
solent_filter$Easting[(solent_oyc[,2])] <- 471343   # H73B
solent_filter$Northing[(solent_oyc[,2])] <- 103667
rm(solent_oyc)

# then recreate the spatial dataset with corrected coords
# convert to spatial
solent_space <- SpatialPointsDataFrame(coords=cbind(solent_filter$Easting, 
                                                    solent_filter$Northing),
                                       data=solent_filter,
                                       proj4string = polys@proj4string)

# plot(polys)
# points(solent_space, col='blue')



#### GB Population thresholds ####

thresh <- read.csv("../Target_species_list.csv", stringsAsFactors = F)

solent_space$GBthresh <- NA

ii <- 1
for (ii in 1:nrow(solent_space)) {
  sp_thresh <- match(solent_space$Species[ii], thresh$Species)
  if (is.na(sp_thresh)) {
    next } else { 
      solent_space$GBthresh[ii] <- solent_space$Count[ii] >= thresh$threshGB[sp_thresh]
    }
}



#### SPA features of interest ####

source('assign_SPAs.R') # file that links each record to its closest SPA

# load data giving designated SPA populations of waterbird species
spathresh <- thresh[, which(names(thresh) %in% c('Species', names(spadist)) )] %>% 
  subset( !(is.na(Chichester.and.Langstone.Harbours) & is.na(Portsmouth.Harbour) &
              is.na(Solent.and.Southampton.Water)))



# calculate for each count what proportion of 
# the relevant SPA's population it represents
solent_space$SPAprop <- NA
ii <- 1

for (ii in 1:nrow(solent_space)) {
  r <- match(solent_space$Species[ii], spathresh$Species)
  c <- match(solent_space$closest[ii], names(spathresh))
  cell <- spathresh[r,c]
  
  if (is.na(cell)) {
    next } else {
      solent_space$SPAprop[ii] <- ((solent_space$Count[ii] / cell) * 100)
    }
}




#### Comparison with nearest WeBS sector ####

# Evaluate the 'local importance' of sites

# load median counts per sector per species
secavepop <- read.csv('../WeBS/Sector_median_nonzero.csv', stringsAsFactors = F)

# Add webs sector info to dataset
source('assign_WeBS_sectors.R')
# sitemaxspp %<>% join(subset(polys@data, select=c(Site_code, closest_webs)))


# Add column of relevant species' total for neares WeBS sector
solent_space$webpop <- NA

ii <- 1
for (ii in 1:nrow(solent_space)) {
  cell <- 
    secavepop[which(secavepop$sector==solent_space[ii,]$closest_webs &
                      secavepop$Species==solent_space[ii,]$Species),]$Median
  if(length(cell) > 0) {
    solent_space$webpop[ii] <- cell }
}


solent_space$webprop <- (solent_space$Count / (solent_space$webpop)) * 100
# summary(sitemaxspp$webthresh)
