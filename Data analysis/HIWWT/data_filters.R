### script to filter data by time and species
### including only winter records
### and removing records from the night time (which are probably errors)
### and removing species not of interest to this study


#### Remove non-target species ####


# drop all seabirds from the analysis
source('../define_species_groups.R') # seabirds are defined earlier

# only use species of interest: waders and Dark-bellied Brent Geese
filter_spp <- solent$Species %in% waders | solent$Species == 'Brent Goose (Dark-bellied)'


rm(seabirds, goodspp, spp, get_goodspp, nohybrids)




#### Only keep winter records ####

##### Months

### only include winter records
## October to March

## create column of months
solent$month <- factor(months(solent$Date))
## create column of years
solent$year <- factor(format(solent$Date, '%Y'))
## create yearmonth column
solent$yeamon <- sprintf('%s %s', solent$month, solent$year)

# which months do we want?
select_months <- c('October','November','December',
                    'January','February','March')
# Boolean vector to extract records from correct months
filter_months <- solent$month %in% select_months






##### Remove Night-time records #####

### anything before 5am or after 8pm gets cut out
### these are probably errors from misuse of 24hour clock!
filter_dark <-(solent$Time >= as.POSIXct('05:00', format='%R')) & 
  (solent$Time <= as.POSIXct('20:00', format='%R'))
filter_dark[is.na(filter_dark)] <- FALSE # don't want the NA rows





###### Combine All filters #####
filter_all <- filter_dark & filter_months & filter_spp
sum(filter_all)


# solent_filter <- tidejoin[filter_all,]
solent_filter <- solent[filter_all,]



### plotting totals by month

# put months in correct order
solent_filter$month <- factor(solent_filter$month, 
                              levels=c('October', 'November', 'December',
                                       'January', 'February', 'March'))

## divide into early and late period, as in Footprint report
solent_filter$period <- 'Early'
solent_filter$period[solent_filter$month %in% c('January', 'February', 'March'
                                                )] <- 'Late'
solent_filter$period %<>% factor


rm( list = ls()[grep('filter_', ls())])



#### flag negative records to be filtered later as appropriate ####
negrecs <- c('None','no geese','no waders','no brent geese','No birds seen')
