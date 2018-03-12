#### Load all WeBS count data 
#### Create 2 data frames, one for low tide counts and one for high tides



#### Setup ####


### load packages
require(plyr) # for join_all() function
require(magrittr) # ceci n'est pas un pipe


### filter to select only winter months 
select_months <- c('November','December',
                   'January','February')#,
# 'October','March') # exclude migration period





#### Low Tide Counts ####

# read in summary data
lowdf <- read.csv('Data_WeBS_LTC/ltc3peak.csv')


# drop all seabirds from the analysis
source('../define_species_groups.R') # seabirds are defined earlier
ourbirds <- unique(lowdf$Species)[
  grep(paste(seabirds, collapse='|'), unique(lowdf$Species), invert=T ) 
  ] %>% droplevels

# only use species of interest
ltc <- subset(lowdf, Species %in% ourbirds)


#### High Tide Counts ####

## this one is a bit more complicated as each sector has a separate file...

## choose folder containing data as the working directory
setwd('~/Dropbox/Solent Bird Studies/Data analysis/WeBS/Data_WeBS_Core')
filesnames <- list.files(pattern='.csv') # which CSV files are in folder?


### read all the files into R

# all columns will be factors except for 'Count'
filecclass <- rep('factor', 7); filecclass[5] <- 'integer'

# load each csv files as a separate dataframe and save in a list
files_list <- lapply(filesnames, read.csv, colClasses=filecclass)


### join all the dataframes into a single data frame (takes a long time!)
websdf <- join_all(files_list, type='full')


### add correct dates instead of just months

# change working directory to parent folder
setwd('..') # go up a level

# read in date lookup table
date_lookup <- read.csv('webs_date_lookup.csv')
# convert to R's date format
date_lookup$date <- as.POSIXct(date_lookup$priority, format='%d/%m/%y')

# join dates with webs data
websdf$date <- date_lookup$date[match(websdf$Visit, date_lookup$recorded)]
# str(websdf)
# summary(websdf)


# add month column
websdf$month <- factor(months(websdf$date))

# add a year column
websdf$year <- as.numeric(format(websdf$date, '%Y'))
# assign early months to previous year's winter
websdf$year[websdf$month %in% c('January', 'February', 'March')] %<>% subtract(1)

## save all data as single CSV file
## note that this will overwrite previous all.csv file without warning
# write.csv(websdf, 'allcore.csv', na="", row.names = F)

## save list of WeBS species names
# webspplist <- data.frame(sp=unique(websdf$Species))
# write.csv(webspplist, '../WeBS_species_list.csv', row.names = F)

# now can remove the individual files from workspace
rm(files_list, filecclass, filesnames, date_lookup)


### only include target species and months of interest
htc <- subset(websdf, month %in% select_months & Species %in% ourbirds) %>% 
  droplevels


### how many sectors covered per visit?
websNvis <- ddply(htc, .(Visit, Species), function(x) {
  .out <- data.frame(
    Sum = sum(x$Count),
    N = nrow(x)
  )
}); websNvis <- ddply(websNvis, .(Visit), function(x) max(x$N, na.rm=T) )
names(websNvis)[2] <- 'N'
Nmax <- max(websNvis$N)


htc <- join(htc, websNvis)