### Script to remove duplicates

### Each recorder is allowed one visit to each site per day
### If they have visited more than once, take maximum count of each species
### As only total assemblages, counts over thresholds and species presence are included

require(magrittr)
require(plyr)


## create dataset including only positive data
# solent_pdata <- subset(solent_jdata, Species %in% negrecs == FALSE ) %>% droplevels


## maximum count of each species at each site per day per recorder
daymaxspp <- ddply(solent_jdata, .(Recorder, Site_code, Date, Species), function(x) {
  data.frame(Max = max(x$Count, na.rm=T))
})


## sites' total assemblage per recorder per day
# first calculate total number of birds seen by each recorder
daytotalrec <- ddply(daymaxspp, .(Recorder, Site_code, Date), function(x) {
  data.frame(Total = sum(x$Max, na.rm=T))
})
# then include only the highest assemblage per day
daytotal <- ddply(daytotalrec, .(Site_code, Date), function(x) {
  data.frame(Total = max(x$Total, na.rm=T))
})


## each site's highest count of each species
sitemaxspp <- ddply(daymaxspp, .(Site_code, Species), function(x) {
  data.frame(Max = max(x$Max, na.rm=T))
})
