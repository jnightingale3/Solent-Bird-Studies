  ### Script to refine each category to a single score per site

require(plyr)
require(lubridate)
require(magrittr)
require(reshape2)
require(rgdal)

#### Population thresholds ####

thresh <- read.csv("../Target_species_list.csv", stringsAsFactors = F)

sitemaxspp$thresh <- NA

ii <- 1
for (ii in 1:nrow(sitemaxspp)) {
  sp_thresh <- match(sitemaxspp$Species[ii], thresh$Species)
  if (is.na(sp_thresh)) {
    next } else { 
      sitemaxspp$thresh[ii] <- sitemaxspp$Max[ii] >= thresh$threshGB[sp_thresh]
    }
}
siteanyspp <- ddply(sitemaxspp, .(Site_ID), function(x) {
  data.frame(Thresh = any(x$thresh))
})

siteanyspp$sp_score <- 0
siteanyspp$sp_score[which(siteanyspp$Thresh)] <- 2

polys@data %<>% join(subset(siteanyspp, select=c(Site_ID, sp_score)))

#### SPA features of interest ####

source('assign_SPAs.R') # file that links each site to its closest SPA

# load data giving designated SPA populations of waterbird species
spathresh <- thresh[, which(names(thresh) %in% c('Species', names(spadist)) )] %>% 
  subset( !(is.na(Chichester.and.Langstone.Harbours) & is.na(Portsmouth.Harbour) &
           is.na(Solent.and.Southampton.Water)))




# clip sitemaxspp to only include SPA species
sitespaspp <- subset(sitemaxspp, Species %in% spathresh$Species)

# add the relevant polygon's closest SPA
sitespaspp$SPA <- polys$closest[match(sitespaspp$Site_ID, polys$Site_ID)]

# calculate for each count what proportion of 
# the relevant SPA's population it represents
sitespaspp$thresh <- NA
ii <- 1

for (ii in 1:nrow(sitespaspp)) {
  cell <- spathresh[which(spathresh$Species == sitespaspp$Species[ii]),
                    which(names(spathresh) == sitespaspp$SPA[ii])]
  sitespaspp$thresh <- ((sitespaspp$Max / cell) * 100)
}


## summarise per site
site_spa_max <- ddply(sitespaspp, .(Site_ID), function(x) {
  data.frame(PC = max(x$thresh))
})

site_spa_max$SPAscore <- 0 
site_spa_max$SPAscore[which(site_spa_max$PC >= 1)] <- 1
site_spa_max$SPAscore[which(site_spa_max$PC >= 5)] <- 2
# site_spa_max$SPAscore[which(site_spa_max$PC >= 10)] <- 3


polys@data %<>% join(subset(site_spa_max, select=c(Site_ID, SPAscore)))





#### Maximum assemblage recorded per site ####

sitetotal <- ddply(daytotal, .(Site_ID), function(x) {
  data.frame(Assemblage = max(x$Total, na.rm=T))
})

## compare with SPA Assemblage designation

# add column of closest SPA
sitetotal$SPA <- polys$closest[match(sitetotal$Site_ID, polys$Site_ID)]

sitetotal$assPC <- NA
ii <- 1
for ( ii in 1:nrow(sitetotal)) {
  cell <- spathresh[which(spathresh$Species == 'Assemblage'),
                    which(names(spathresh) == sitetotal$SPA[ii])]
  sitetotal$assPC <- ((sitetotal$Assemblage / cell) * 100)
}


sitetotal$AssScore <- 0
sitetotal$AssScore[which(sitetotal$assPC >= 1)] <- 1
sitetotal$AssScore[which(sitetotal$assPC >= 5)] <- 2
# sitetotal$AssScore[which(sitetotal$assPC >= 10)] <- 3

polys@data %<>% join(subset(sitetotal, select=c(Site_ID, Assemblage, AssScore)))


#### Comparison with nearest WeBS sector ####

# Evaluate the 'local importance' of sites

# load median counts per sector per species
secavepop <- read.csv('../WeBS/Sector_median_nonzero.csv', stringsAsFactors = F)

# Add webs sector info to dataset
source('assign_WeBS_sectors.R')
sitemaxspp %<>% join(subset(polys@data, select=c(Site_ID, closest_webs)))


# Add column of relevant species' total for neares WeBS sector
sitemaxspp$webpop <- NA
ii <- 1
for (ii in 1:nrow(sitemaxspp)) {
  cell <- 
    secavepop[which(secavepop$sector==sitemaxspp[ii,]$closest_webs &
                      secavepop$Species==sitemaxspp[ii,]$Species),]$Median
  if(length(cell) > 0) {
    sitemaxspp$webpop[ii] <- cell }
}


sitemaxspp$webthresh <- sitemaxspp$Max >= (sitemaxspp$webpop)
# summary(sitemaxspp$webthresh)

webs_score <- ddply(sitemaxspp, .(Site_ID), function(x) {
  data.frame( WebsScore = as.numeric(any(x$webthresh)) )
}) %>% na.omit
polys@data %<>% join(webs_score)





#### Number of visits ####

source('assign_gauge.R') # assign each site to its nearest tide gauge


# load data generated earlier 
# in tidal_visit_number.R
tidevisit <- read.csv('Data/Visit_during_tide_window.csv')
tidevisit$Date %<>% as.POSIXct()


### only use counts from within last 5 years
today <- Sys.Date()
limit <- today - years(5)

# split out those data
tvis5y <- subset(tidevisit, Date >= limit)


### number of visits in last 5 years in tidal window
sitevisits <- ddply(tvis5y, .(Site_ID), function(x) {sum(x$V1, na.rm=T)})
names(sitevisits)[2] <- 'Days'

# how many sites have 5+ visits?
nrow(subset(sitevisits, Days >= 5)) / nrow(polys@data) # 15%


### add Confidence score to original site info
polys@data %<>% join(sitevisits)
polys$Days[is.na(polys$Days)] <- 0

rm(today, limit)





##### Betweenness Centrality #####


bet <- readOGR(dsn='../Solent Bird Movement/GIS Files',
        layer='Network_stats')@data
bet %<>% subset(Site_ID %in% polys$Site_ID, select=c(TARGET_FID, Site_ID,
                                                     Site_code, between))

polys@data %<>% join(bet, by='TARGET_FID')
polys$BetweenScore <- 0
polys$BetweenScore[which(polys$between >= 1)] <- 2

################################
###### combine all scores ######
################################

# which columns are scores?
cols <- grep('score', names(polys), ig=T)

# initialise column 
polys@data$TopScore <- 0

# loop through sites - Top Score will be maximum of other scores
ii <- 1
for (ii in 1:nrow(polys@data)) {
  polys@data$TopScore[ii] <- max(as.vector(polys@data[ii,cols]), na.rm=T)
}


# and correct for lack of visits - set to NA to reflect lack of data
polys$TopScore[polys$Days < 5 & polys$TopScore <= 0] <- NA

# Set minimum score to 0
# - the max() function returns -Inf if all input values are NA
# this happens for records of species that don't have thresholds
polys$TopScore[which(polys$TopScore < 0)] <- 0


## Convert TopScore to character
polys@data <- within(polys@data, {
  TopScore[is.na(TopScore)] <- 'Data Deficient'
  TopScore[which(TopScore == 0)] <- 'Not in network'
  TopScore[which(TopScore == 1)] <- 'Secondary network'
  TopScore[which(TopScore == 2)] <- 'Primary network'
})


# check relationships between the variables
# source('../def_panelutils.R')
# pairs(polys@data[,cols], upper.panel=panel.cor)

