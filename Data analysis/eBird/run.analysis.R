### Script to calculate stats about eBird data
### Can't meaningfully comparer with SBS / WeBS data

### TODO : Get locations of counts across thresholds

require(sp)
require(plyr)
require(magrittr)

# data from spatially-clipped bird points
spacedate <- spacebird@data


#### total assemblage ####

eb_ass <- ddply(spacedate, .(visitid), function(x) {
  sum(x$OBSERVATION.COUNT)
})
  
names(eb_ass)[2] <- 'Assemblage'

spacedate %<>% join(eb_ass)
spacedate$Sig.ass <- (spacedate$Assemblage >= 2000) %>% factor(levels=c('TRUE', 'FALSE'))
rm(eb_ass)


#### thresholds ####

# load lookup tables of species thresholds
thresh <- read.csv("~/Dropbox/Solent Bird Studies/Data analysis/Target_species_list.csv",
                   stringsAsFactors = F)

spacedate$Int.Thresh <- rep(NA, nrow(spacedate))
ii <- 1
for (ii in 1:nrow(spacedate)) {
  sp_thresh <- match(spacedate$species[ii], thresh$Species)
  if (is.na(sp_thresh)) {
    next } else { 
      spacedate$Int.Thresh[ii] <- spacedate$OBSERVATION.COUNT[ii] >= thresh$threshInt[sp_thresh]
    }
}

spacedate$Nat.Thresh <- rep(NA, nrow(spacedate))
ii <- 1
for (ii in 1:nrow(spacedate)) {
  sp_thresh <- match(spacedate$species[ii], thresh$Species)
  if (is.na(sp_thresh)) {
    next } else { 
      spacedate$Nat.Thresh[ii] <- spacedate$OBSERVATION.COUNT[ii] >= thresh$threshGB[sp_thresh]
    }
}

rm(ii, sp_thresh)

# extract info about each unique visit
visitloc <- subset(spacedate, select=c(visitid, LATITUDE, LONGITUDE, LOCALITY),
                   duplicated(visitid)==F)

# add national threshold information
eb_nat <- ddply(spacedate, .(visitid), function(x) {any(x$Nat.Thresh, na.rm=T)})
names(eb_nat)[2] <- 'Nat.Thresh'
visitloc %<>% join(eb_nat)

# add international threshold information
eb_int <- ddply(spacedate, .(visitid), function(x) {any(x$Int.Thresh, na.rm=T)})
names(eb_int)[2] <- 'Int.Thresh'
visitloc %<>% join(eb_int)

# clean up
rm(eb_int, eb_nat)

visitloc$Nat.Thresh %<>% factor(levels=c(TRUE, FALSE))
visitloc$Int.Thresh %<>% factor

visitloc <- visitloc[order(visitloc$Nat.Thresh, decreasing=T),]




#### number of species ####

nospp <- ddply(subset(spacedate, species %in% nohybrids(unique(spacedate$species))),
               .(LOCALITY), function(x) {length(unique(x$species))})
nospp %<>% join(subset(spacedate, !duplicated(spacedate$LOCALITY), select=c(
  LOCALITY, LONGITUDE, LATITUDE)
))




#### visit information / recording bias ####
visinfo <- ddply(spacedate, .(LOCALITY), function(x) {
  res <- data.frame(
   N.vis = length(unique(x$visitid)),
   Max = max( by(x, x$visitid, function(y) {sum(y$OBSERVATION.COUNT)}, simplify=T) ),
   NSpp = length(unique(nohybrids(x$species)))
  )
})
