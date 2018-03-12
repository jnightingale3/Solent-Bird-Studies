### Produces various summaries of WeBS data by sector
### and plots them

require(plyr)
require(ggplot2)

## only include species of interest
htc_ourbirds <- subset(htc, Species %in% ourbirds)

### Total assemblage
# for each visit
htc_sector_vistot <- ddply(htc_ourbirds, .(id, Visit), function(x) {sum(x$Count)})
# mean visit totals
htc_sector_avetot <- ddply(htc_sector_vistot, .(id), function(x) mean(x$V1))
htc_sector_avetot$rank <- order(htc_sector_avetot$V1, decreasing = T)

map.avetot <- join(htc_sector_avetot, coremap, by='id')

p <- ggplot(map.avetot) + geom_polygon(aes(x=long, y=lat, group=group,
                                           fill=V1)) + coord_equal() +
  scale_fill_gradient(low='yellow', high='green') + theme_classic()
print(p)



### Number of species
htc_sector_visnsp <- ddply(htc_ourbirds, .(id, Visit), function(x) {
  sum(x$Count >= 1)
})
htc_sector_avensp <- ddply(htc_sector_visnsp, .(id), function(x) (mean(x$V1)))

map.avensp <- join(htc_sector_avensp, coremap, by='id')

q <- ggplot(map.avensp) + geom_polygon(aes(x=long, y=lat, group=group,
                                           fill=Site)) + coord_equal() +
  # scale_fill_gradient(low='yellow', high='green') + 
  theme_classic()
print(q)

plot(log(htc_sector_avetot$V1), log(htc_sector_avensp$V1))


### If any species meets national or international thresholds
# Any counts recorded in that sector that are across threshold?

# Population thresholds taken from BTO Online WeBS Report 2013-14
pops <- read.csv('pop_thresholds.csv')


## international

# per visit
htc_sector_visipt <- ddply(htc_ourbirds, .(id, Visit), function(x) {
  splist <- as.character(unique(x$Species))
  allspp <- as.character(unique(htc_ourbirds$Species))
  
  out <- rep(NA, length(allspp))
  
  # loop through all species on that visit to that sector
  for (ii in 1:length(splist)) {
    current_sp <- splist[ii]
    counts <- x$Count[x$Species == current_sp]
    thresh <- pops$International[pops$Species == current_sp]
    greater <- sum(counts >= thresh) # number of counts exceeding threshold
    
    out[which(allspp == current_sp)] <- greater
  }
  
  return(out)
})
# rename the columns with names of species
names(htc_sector_visipt)[3:dim(htc_sector_visipt)[2]] <- as.character(unique(htc_ourbirds$Species))
# show all non-zero totals - Brents and Blackwits
colSums(htc_sector_visipt[,-(1:2)], na.rm = T)[
  which(colSums(htc_sector_visipt[,-(1:2)], na.rm = T)>0)]


# per species total
htc_sector_avipt_persp <- ddply(htc_sector_visipt, .(id), function(x) {
  for (ir in 1:nrow(x)) {x[,is.na(x[ir,])] <- 0}
  out <- colSums(x[,-(1:2)])
  #out[is.na(out)] <- 0
  return(out)
})

# overall total
htc_sector_avipt_tot <- ddply(htc_sector_visipt, .(id), function(x) {
  sum( rowSums(x[,-(1:2)], na.rm = TRUE) )
})


map_ipt <- join(htc_sector_avipt_tot, coremap, by='id')

p.ipt <- ggplot(map_ipt) + geom_polygon(aes(x=long, y=lat, group=group,
                                           fill=V1)) + coord_equal() +
  scale_fill_gradient(low='yellow', high='green') + theme_classic()
print(p.ipt)



## national

# per visit
htc_sector_visnat <- ddply(htc_ourbirds, .(id, Visit), function(x) {
  splist <- as.character(unique(x$Species))
  allspp <- as.character(unique(htc_ourbirds$Species))
  
  out <- rep(NA, length(allspp))
  
  # loop through all species on that visit to that sector
  for (ii in 1:length(splist)) {
    current_sp <- splist[ii]
    counts <- x$Count[x$Species == current_sp]
    thresh <- pops$National[pops$Species == current_sp]
    greater <- sum(counts >= thresh)
    
    out[which(allspp == current_sp)] <- greater
  }
  
  return(out)
})
names(htc_sector_visnat)[3:dim(htc_sector_visnat)[2]] <- as.character(unique(htc_ourbirds$Species))
colSums(htc_sector_visnat[,-(1:2)], na.rm = T)[
  which(colSums(htc_sector_visnat[,-(1:2)], na.rm = T)>0)]


# per species total
htc_sector_avnat_persp <- ddply(htc_sector_visnat, .(id), function(x) {
  for (ir in 1:nrow(x)) {x[,is.na(x[ir,])] <- 0}
  out <- colSums(x[,-(1:2)])
  #out[is.na(out)] <- 0
  return(out)
})

# overall total
htc_sector_avnat_tot <- ddply(htc_sector_visnat, .(id), function(x) {
  sum( rowSums(x[,-(1:2)], na.rm = TRUE) )
})
# How many counts over threshold for each species?
colSums(htc_sector_avnat_persp[,-1])[which(colSums(htc_sector_avnat_persp[,-1])>0)]

map_nat <- join(htc_sector_avnat_tot, coremap, by='id')

p.nat <- ggplot(map_nat) + geom_polygon(aes(x=long, y=lat, group=group,
                                            fill=V1)) + coord_equal() +
  scale_fill_gradient(low='yellow', high='red') + theme_classic()
print(p.nat)
