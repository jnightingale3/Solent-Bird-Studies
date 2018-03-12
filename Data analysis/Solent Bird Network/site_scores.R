### use information about each record to assign importance scores to sites

### Script to refine each category to a single score per site

require(plyr)
require(lubridate)
require(magrittr)
require(reshape2)
require(rgdal)



#### Number of visits ####

source('assign_gauge.R') # assign each site to its nearest tide gauge


# load data generated earlier 
# in tidal_visit_number.R
tidevisit <- read.csv('Data/Visit_during_tide_window.csv')
tidevisit$Date %<>% as.POSIXct()


### only use counts from within last 5 years
today <- Sys.Date()
limit <- today - lubridate::years(5)

# split out those data
tvis5y <- subset(solent_jdata, Date >= limit)


### number of visits in last 5 years in tidal window
sitevisits <- ddply(tvis5y, .(Site_code), nrow)
names(sitevisits)[2] <- 'Days'

# how many sites have 5+ visits?
nrow(subset(sitevisits, Days >= 5)) / length(unique(polys$Site_code)) # 56%


### add Confidence score to original site info
polys@data %<>% join(sitevisits) 
polys$Days[is.na(polys$Days)] <- 0

rm(today, limit)






#### Maximum assemblage recorded per site ####

sitetotal <- ddply(daytotal, .(Site_code), function(x) {
  data.frame(Assemblage = max(x$Total, na.rm=T))
})
# polys@data %<>% join(sitetotal)
# polys$Assemblage[is.na(polys$Assemblage)] <- 0

# ## compare with SPA Assemblage designation
# add column naming closest SPA
sitetotal$closestSPA <- rep(NA, nrow(sitetotal))
midpts <- gCentroid(polys, byid=TRUE)
mat <- match(sitetotal$Site_code, polys$Site_code)
dist.poly.spa <- gDistance(midpts, midspa, byid=T) %>% t

ii <- 1
for (ii in 1:nrow(sitetotal)) {
  sitetotal$closestSPA[ii] <- names(spadist)[which.min(dist.poly.spa[mat[ii],])]
}



sitetotal$assPC <- NA
ii <- 1
for ( ii in 1:nrow(sitetotal)) {
  cell <- spathresh[which(spathresh$Species == 'Assemblage'),
                    which(names(spathresh) == sitetotal$closestSPA[ii])]
  sitetotal$assPC[ii] <- ((sitetotal$Assemblage[ii] / cell) * 100)
}


sitetotal$AssScore <- 0
sitetotal$AssScore[which(sitetotal$assPC >= 1)] <- 1
sitetotal$AssScore[which(sitetotal$assPC >= 5)] <- 2

polys@data %<>% join(subset(sitetotal, select=c(Site_code, Assemblage, AssScore)))


##### Betweenness Centrality #####


bet <- readOGR(dsn='../Solent Bird Movement/Results',
        layer='Network_stats_new')@data
# bet %<>% subset(Site_code %in% polys$Site_code, select=c(Site_code, between, degree))
source('movement_to_polys.R') # maps movement data (based on Strat sites) onto whatever sites are in polys

betmax <- ddply(bet, .(Site_code), function(x) {
  data.frame(
    between = max(x$between, na.rm=T),
    degree = max(x$degree, na.rm=T)
  )
})
betmax$between[betmax$between <0] <- -1




polys@data %<>% join(betmax)
polys$BetweenScore <- 0
polys$BetweenScore[which(polys$between >= 1)] <- 2
polys$BetweenScore[which(polys$degree >= 4)] <- 2




#### Population thresholds ####
siteanyspp <- ddply(solent_jdata, .(Site_code), function(x) {
  data.frame(GBthresh = any(x$GBthresh))
})

siteanyspp$sp_score <- 0
siteanyspp$sp_score[which(siteanyspp$GBthresh)] <- 2

polys@data %<>% join(subset(siteanyspp, select=c(Site_code, sp_score)))


#### SPA features of interest ####

## summarise per site
site_spa_max <- ddply(solent_jdata, .(Site_code), function(x) {
  data.frame(PC = max(x$SPAprop, na.rm=T))
})

polys@data %<>% join(site_spa_max)


polys$SPAscore <- 0 
polys$SPAscore[which(polys$PC >= 1)] <- 1
polys$SPAscore[which(polys$PC >= 5)] <- 2





#### Comparison with nearest WeBS sector ####
## Any site with over 5% of closest WeBS median count 
webs_score <- ddply(solent_jdata, .(Site_code), function(x) {
  data.frame( WebsScore = as.numeric(any(x$webprop >= 5)) )
}) %>% na.omit
polys@data %<>% join(webs_score)

polys$WebsScore[is.na(polys$WebsScore)] <- 0



################################
###### combine all scores ######
################################

# which columns are scores?
polys$TopScore <- NULL
cols <- grep('score', names(polys), ig=T)

# initialise column 
polys@data$TopScore <- 0

# loop through sites - Top Score will be maximum of other scores
ii <- 1
for (ii in 1:nrow(polys@data)) {
  polys@data$TopScore[ii] <- max(as.vector(polys@data[ii,cols]), na.rm=T)
}


# and correct for lack of visits - set to NA to reflect lack of data
# polys$TopScore[polys$Visits <=3 & polys$TopScore <= 0] <- NA

# Set minimum score to 0
# - the max() function returns -Inf if all input values are NA
# this happens for records of species that don't have thresholds
polys$TopScore[which(polys$TopScore < 0)] <- 0



### data deficient sites
polys$DataDeficient <- polys$Days <= 3





## Convert TopScore to character
polys@data <- within(polys@data, {
  TopScore[which(TopScore == 0 & DataDeficient)] <- 'Data deficient'
  TopScore[which(TopScore == 0)] <- 'Low use'
  TopScore[which(TopScore == 1)] <- 'Secondary network'
  TopScore[which(TopScore == 2)] <- 'Primary network'
})
polys$TopScore %<>% factor(levels=c('Data deficient', 'Low use', 'Secondary network', 'Primary network'))

table(polys$TopScore)


# check relationships between the variables
# source('../def_panelutils.R')
# pairs(polys@data[,cols], upper.panel=panel.cor)

