### script to filter data by time and species
### including only winter records
### and removing records from the night time (which are probably errors)
### and removing species not of interest to this study

require(magrittr)

#### Remove non-target species ####


# drop all seabirds (and Merlin and Kingfisher) from the analysis
source('../define_species_groups.R') # seabirds are defined earlier
ourbirds <- unique(solent$Species)[
  grep(paste(seabirds, collapse='|'), unique(solent$Species), invert=T ) 
  ] %>% droplevels

# only use species of interest
filter_spp <- solent$Species %in% ourbirds


rm(ourbirds, seabirds, goodspp, spp, get_goodspp, nohybrids)




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






###### Combine All filters #####
filter_all <- filter_months & filter_spp
sum(filter_all)


# solent_filter <- tidejoin[filter_all,]
solent_filter <- solent[filter_all,]





# put months in correct order
solent_filter$month <- factor(solent_filter$month, 
                              levels=c('October', 'November', 'December',
                                       'January', 'February', 'March'))

## divide into early and late period, as in Footprint report
solent_filter$period <- 'Early'
solent_filter$period[solent_filter$month %in% c('January', 'February', 'March'
                                                )] <- 'Late'
solent_filter$period %<>% factor

rm( list = ls()[grep('tide', ls())] )
rm( list = ls()[grep('filter_', ls())])

