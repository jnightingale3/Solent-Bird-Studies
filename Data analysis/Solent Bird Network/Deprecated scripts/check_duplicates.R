# check duplicates

### at the moment this script is purely exploratory
### how many duplicates do we have, and how can we identify them?

### trying basic combinations of date, time & recorder, plus some way to narrow down by site
### site name is too vague and non-standardized
### but co-ordinates *may* be vulnerable to too problems:
###      1) the SW&BG Records data have some auto-assigned coordinates, thus increasing duplicates
###      2) users *may* have tried to correct coordinates, but submitted record twice?


##### SUGGESTED PROTOCOL ##
# sort spatially-joined records by date, recorder and polygon
# take maximum count for each species separately, within this subset
# should avoid duplication without throwing records away - those birds were there
# in any case it appears a lot of these are negative records!
## this protocol is tested in the last section of this script.


require(magrittr)
require(plyr)


#### final solution ####

# uses data joined by spatial_join_final.R

### sort spatially-joined records by date, time, recorder and polygon

# create duplication ID column using variables of interest
solent_jdata$dupid <- with(solent_jdata, sprintf('%s %s %s %s', 
                                                 Date, Time, Recorder, Site_ID))

# find all duplicated records
solent_jdata$isdup <- duplicated(solent_jdata$dupid) | 
  duplicated(solent_jdata$dupid, fromLast = T)
sum(solent_jdata$isdup)
sum(solent_jdata$isdup) / nrow(solent_jdata) * 100 # 25% of records!

# data frame without any of the duplicated rows
solent_nodup <- subset(solent_jdata, !isdup)

# which columns are species
species_list$sp <- gsub(' ', '.', species_list$Species)
spcol <- names(solent_jdata)%in%species_list$sp

solent_dups <- subset(solent_jdata, isdup)
solent_dups$spstr <- rep(NA, nrow(solent_dups))
for(ii in 1:nrow(solent_dups)) {
  solent_dups$spstr[ii] <- paste(solent_dups[ii,spcol], collapse=';')
}

# records with same metadata and species records
solent_dups$metasp <- with(solent_dups, paste(dupid, spstr))
solent_dups$metadup <- (duplicated(solent_dups$metasp) | 
                          duplicated(solent_dups$metasp, fromLast = T))
sum(solent_dups$metadup) # most of them
sum( rowSums(solent_dups[solent_dups$metadup,which(spcol)])  ==0 )
sum( rowSums(solent_dups[solent_dups$metadup,which(spcol)])  ==0 ) / 
  sum(solent_dups$metadup) * 100 # ca. 90% of these duplicated records are negatives


# have a look at the non-duplicated entries
solent_dups[!solent_dups$metadup,which(spcol)]
solent_dups[84:85,which(spcol)]
solent_dups[84:85,'metasp']


### take maximum count for each species separately, within this subset

ii <- ij <- 1
dupres <- matrix(rep(NA, length(unique(solent_dups$dupid))*sum(spcol)), ncol=sum(spcol))
  
for(ii in 1:length(unique(solent_dups$dupid))) {
  tmp <- solent_dups[solent_dups$dupid==unique(solent_dups$dupid)[ii], which(spcol)]
  
  for(ij in 1: ncol(tmp)) {
    dupres[ii, ij] <- max(tmp[,ij])
  }
}

dupres %<>% data.frame
names(dupres) <- names(solent_jdata)[which(spcol)]
dupres$dupid <- unique(solent_dups$dupid)

matcher <- match(dupres$dupid, solent_dups$dupid)
sol_dupfix <- solent_dups[matcher,]
sol_dupfix[,which(spcol)] <- dupres[,1:sum(spcol)]
sol_dupfix %<>% subset(select= intersect(names(solent_nodup), names(sol_dupfix)))

solent_jdata <- rbind(solent_nodup, sol_dupfix)

rm(solent_dups, ij, tmp, matcher, sol_dupfix)


#### early exploration ####
# same_day_time <- ddply(solent, .(Date, Time), nrow)
# sum(same_day_time$V1 > 1) # loads from same date and time
# head(same_day_time[same_day_time$V1 > 2,], 10)
# 
# same_day_time_EN <- ddply(solent, .(Date, Time, Easting, Northing), nrow)
# sum(same_day_time_EN$V1 > 1) # but very few of these are from *exact* same location
# 
# same_day_time_EN[same_day_time_EN$V1 > 1, ]
# 
# 
# same_day_time_rec <- ddply(solent, .(Date, Time, Recorder), nrow)
# sum(same_day_time_rec$V1 > 1)
# subset(solent, Date=='2013-02-09' & Recorder=='Ed Rowsell' & Sitename=='Great Deep')
# 
# 
# same_day_time_ENrec <- ddply(solent, .(Date, Time, Easting, Northing, Recorder), nrow)
# sum(same_day_time_ENrec$V1 > 1)
# same_day_time_ENrec[(same_day_time_ENrec$V1 > 1),]
# 
# 
# ##### alternative ways of finding duplicates - probably over conservative #####
# 
# 
# same_day_time_SNrec <- ddply(solent, .(Date, Time, Sitename, Recorder), nrow)
# sum(same_day_time_SNrec$V1 > 1)
# head(same_day_time_SNrec[(same_day_time_SNrec$V1 > 1),])
# 
# same_day_time_EN <- ddply(solent, .(Date, Time, Easting, Northing), nrow)
# sum(same_day_time_EN$V1 > 1)
# 
# 
# same_day_time_sp <- ddply(solent, .(Date, Time, Recorder, Easting, Northing, `Brent Goose (Dark-bellied)`, Oystercatcher), nrow)
# sum(same_day_time_sp$V1 > 1)
# sum(same_day_time_sp$V1 > 1 & rowSums(same_day_time_sp[6:7])>0)
# 
# 
# same_day_time_sp[(same_day_time_sp$V1 > 1 & rowSums(same_day_time_sp[5:6])>0),]
# 
# ### which wader species has most non-zero records?
# max(colSums(solent[, names(solent)%in%species_list$sp] >1) )
# sum(solent$`Brent Goose (Dark-bellied)` >1) # Brent
# 
# 
# 
# subset(solent, Date=='2015-12-23' & Recorder=='Anne De Potier')