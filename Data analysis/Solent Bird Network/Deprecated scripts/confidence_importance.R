### script to calculate the importance and confidence scores of sites
### as categorised by the Footprint Ecology report (Liley & Sharp 2010)
### unless otherwise noted


#######################
##### Brent Geese #####
#######################


#### Confidence ####

range(solent_jdata$Date) # date range of survey
## this period contains 4 survey seasons
## Brent Goose Strategy (2002) assumes this period is 25 weeks
## Therefore, there would be 100 weeks during this period

## How many weeks have coverage?

daterange <- seq(from=range(solent_jdata$Date)[1], 
                   to=range(solent_jdata$Date)[2],
                   by='days') %>% data.frame
names(daterange) <- 'Date'
daterange$weekno <- seq(from=1, to=nrow(daterange), by=1) %/% 7
solent_jdata <- join(solent_jdata, daterange)


# totals per site per week (visits need to be in separate weeks)
bre_week_tots <- ddply(solent_jdata, .(Site_ID, weekno), nrow)
# number of weeks during which each site was visited
Site_weektots <- ddply(bre_week_tots, .(Site_ID), nrow)


# how many weeks were surveys conducted?
# note that some were excluded because e.g. lacking tide data
length(unique(solent_jdata$weekno)) # 103
# strategy assumes 25 weeks per season, = 100 in our survey period
nweeks <- 100

# Confidence = Visits / Number of weeks
brent_Site_confidence <- Site_weektots$V1 / nweeks * 100

sum(brent_Site_confidence >= 5) # 205 sites cross Strategy doc's threshold 5%
hist(brent_Site_confidence) # basically all below 10%
abline(v=5, col='red')


#### Importance ####

## maximum count
brent_Site_max <- ddply(solent_jdata, .(Site_ID), function(x) {
  max(x$Brent.Goose..Dark.bellied.)
})


## frequency of use
brent_Site_freq <- ddply(solent_jdata, .(Site_ID), function(x) {
  sum(x$Brent.Goose..Dark.bellied. > 0) / 
    length(x$Brent.Goose..Dark.bellied.) * 100
})


## importance based on max count
brent_Site_imp_maxcnt <- rep(0, nrow(brent_Site_max))
brent_Site_imp_maxcnt[brent_Site_max$V1 > 20] %<>% add(2)
brent_Site_imp_maxcnt[brent_Site_max$V1 > 196] %<>% add(2)
brent_Site_imp_maxcnt[brent_Site_max$V1 > 981] %<>% add(2)
brent_Site_imp_maxcnt[brent_Site_max$V1 > 2000] %<>% add(2)


## importance based on frequency
brent_Site_imp_frq <- rep(0, nrow(brent_Site_freq))
brent_Site_imp_frq[brent_Site_freq$V1 > 1] %<>% add(1)
brent_Site_imp_frq[brent_Site_freq$V1 > 20] %<>% add(1)
brent_Site_imp_frq[brent_Site_freq$V1 > 40] %<>% add(1)
brent_Site_imp_frq[brent_Site_freq$V1 > 60] %<>% add(1)


### combine importance ratings
brent_Site_imp_sum <- brent_Site_imp_maxcnt + brent_Site_imp_frq

## use thresholds to assign sites to categories A to D
brent_Site_importance <- rep('D', length(brent_Site_imp_sum))
brent_Site_importance[brent_Site_max$V1 == 0] <- 'No Brents'
brent_Site_importance[brent_Site_imp_sum >= 1] <- 'C'
brent_Site_importance[brent_Site_imp_sum >= 5] <- 'B'
brent_Site_importance[brent_Site_imp_sum >= 8] <- 'A'
brent_Site_importance %<>% factor %>% data.frame
brent_Site_importance$Site_ID <- brent_Site_max$Site_ID
names(brent_Site_importance)[1] <- 'Importance'
summary(brent_Site_importance)




## Are sites with higher confidence more likely to be important?
# summary(lm(brent_Site_imp_sum~brent_Site_confidence)) # yes, loads
# # check it out
# plot(brent_Site_confidence, brent_Site_imp_sum)
# abline(lm(brent_Site_imp_sum ~ brent_Site_confidence), col='red') # yes



##################
##### Waders #####
##################

## separate data set that only includes birds close to high tide
## (see time_filters.R)

solent_wader <- solent_jdata[solent_jdata$filter_tide,]

#### Confidence ####


## how many months during study period?
# winters 2012/13; 2013/14; 2014/15; 2015/16
# contains 4*6 = 24 months  


###    % of months during survey period where site was surveyed

nmonth <- length(unique(solent_wader$yeamon)) # 20

# number of site visits in each month
range(solent_wader$Date)
month_tots <- ddply(solent_wader, .(yeamon), nrow)
month_tots # some months with 1 or 0 visits


# visits per site per month
Site_month_tots <- ddply(solent_wader, .(Site_ID, yeamon), nrow)

# Confidence = Number of visits / Number of months
wad_Site_conf <- ddply(Site_month_tots, .(Site_ID), function(x) {
  nrow(x)/nmonth*100
})
names(wad_Site_conf)[2] <- 'Confidence'


# threshold for confidence based on number of months in Footprint report 30%
.t <- 5.4 / nmonth * 100


# Look at data
head(wad_Site_conf)
hist(wad_Site_conf$Confidence, main='Wader confidence', xlab='%')
abline(v=.t, col='red')
sum(wad_Site_conf$Confidence >= .t) # 41 sites with sufficient confidence 
# 30% is same threshold as original strategy
# could use threshold of 6 months of survey, which would be 25% -> 44 sites

wad_Site_conf[wad_Site_conf$Confidence >= .t,]



#### Importance for waders ####

# new species column to match species names borked by ddply functions
species_list$sp <- gsub(pattern='[ ()-]', '.', x=species_list$Species)

# remove non-waders
dropspp <- c('Brent.Goose..Dark.bellied.', 'Canada.Goose', 'Coot','Cormorant',
             'Curlew.Sandpiper' ,'Gadwall', 'Grey.Heron', 'Little.Egret',
             'Mallard', 'Moorhen', 'Mute.Swan', 'Pintail', 'Shelduck', 
             'Shoveler', 'Teal', 'Water.Rail', 'Wigeon')
waderspp <- species_list$sp[species_list$sp %in% dropspp == F]
wadercol <- names(solent_wader) %in% waderspp


### site stats:

# number of wader species
wader_Site_nospp <- ddply(solent_wader, .(Site_ID), function(x) {
  sum( colSums(x[,wadercol]) > 0)
})


# maximum count of individuals
wader_Site_maxcnt <- ddply(solent_wader, .(Site_ID), function(x) {
  max( rowSums(x[,wadercol]))
})


# mean count 
wader_Site_meancnt <- ddply(solent_wader, .(Site_ID), function(x) {
  mean( rowSums(x[,wadercol]))
})


# species incidence (% of visits recording species_i)
wader_Site_spinc <- ddply(solent_wader, .(Site_ID), function(x) {
  colSums(x[,wadercol] > 0) / nrow(x) * 100
})
# head(wader_Site_spinc, 10) 



### importance based on number of species
wad_imp_nospp <- rep('C', nrow(wader_Site_nospp))
wad_imp_nospp[wader_Site_nospp$V1 > 5] <- 'B'
wad_imp_nospp[wader_Site_nospp$V1 > 10] <- 'A'
wad_imp_nospp %<>% factor
summary(wad_imp_nospp)
plot(wad_imp_nospp, wad_Site_conf$Confidence) # very obvious

# based on species incidence
wad_imp_spinc <- rep('C', nrow(wader_Site_spinc))
wad_imp_spinc[(rowSums(wader_Site_spinc[-1] >50 ) >1)] <- 'B'
wad_imp_spinc[(rowSums(wader_Site_spinc[-1] >75) >1)] <- 'A'
wad_imp_spinc %<>% factor
summary(wad_imp_spinc)
plot(wad_imp_spinc, wad_Site_conf$Confidence) # opposite trend

# based on maximum count
wad_imp_maxcnt <- rep('C', nrow(wader_Site_maxcnt))
wad_imp_maxcnt[wader_Site_maxcnt$V1 > 1000] <- 'B'
wad_imp_maxcnt[wader_Site_maxcnt$V1 > 5000] <- 'A'
wad_imp_maxcnt %<>% factor
summary(wad_imp_maxcnt)
plot(wad_imp_maxcnt, wad_Site_conf$Confidence) # more counts ~ higher totals


### Thresholds

### crossing the international threshold
ii <- 0
threshint <- data.frame(matrix(0, nrow = nrow(solent_wader), 
                               ncol = length(waderspp) ))
colnames(threshint) <- waderspp
for(ii in 1:length(waderspp)) {
  threshint[,ii] <- (solent_wader[,wadercol])[,ii] > 
    (species_list$threshInt[species_list$sp %in% waderspp])[ii]
}
threshint$Site_ID <- solent_wader$Site_ID
wader_Site_threshint <- ddply(threshint, .(Site_ID), function(x) {
  sum(rowSums(x[,-23]) > 0)
})

### crossing the national threshold
ii <- 0
threshgb <- data.frame(matrix(0, nrow = nrow(solent_wader), 
                              ncol = length(waderspp) ))
colnames(threshgb) <- waderspp
for(ii in 1:length(waderspp)) {
  threshgb[,ii] <- (solent_wader[,wadercol])[,ii] > 
    (species_list$threshGB[species_list$sp %in% waderspp])[ii]
}
threshgb[is.na(threshgb)] <- FALSE
threshgb$Site_ID <- solent_wader$Site_ID
wader_Site_threshgb <- ddply(threshgb, .(Site_ID), function(x) {
  sum(rowSums(x[,-23]) > 0)
})


## Site importance based on National & International thresholds
wad_imp_thresh <- rep('C', nrow(wader_Site_threshint))
wad_imp_thresh[wader_Site_threshgb$V1 > 0] <- 'B'
wad_imp_thresh[wader_Site_threshint$V1 > 0] <- 'A'
wad_imp_thresh %<>% factor
summary(wad_imp_thresh)
plot(wad_imp_thresh, wad_Site_conf$Confidence) # more surveys, more important


### Combine Importance from all sources
### Site importance is the highest category from each source
wader_importance <- data.frame(
  Site_ID = wad_Site_conf$Site_ID,
  Importance = pmin(as.character(wad_imp_maxcnt), 
                    as.character(wad_imp_nospp),
                    as.character(wad_imp_spinc), 
                    as.character(wad_imp_thresh)) )
wader_importance$Importance %<>% as.character
wader_importance$Importance[wader_Site_maxcnt$V1 == 0] <- 'No waders'
wader_importance$Importance %<>% factor
summary(wader_importance)



###########################
#### Concluding tables ####
###########################

brent_sites <- brent_Site_importance
brent_sites$Confidence <- brent_Site_confidence

wader_sites <- wader_importance
wader_sites$Confidence <- wad_Site_conf$Confidence

brent_sites[brent_sites$Confidence >= 5,]
wader_importance[wad_Site_conf$Confidence >= .t,]

## where are these sites?
(solent_jdata[solent_jdata$Site_ID=='P10', ])
               #wadercol])
                   #"Sitename"])


### clean up 
rm(daterange)
