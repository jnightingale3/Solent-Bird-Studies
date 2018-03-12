### Attempt to have a single master document

### loading all data from scratch and processing it

### ending with a summary of which strategy field are important,
### compared with earlier strategy assessments

### and whether any fields previously assessed as 'suitable but unused'
### are now used


#### Setup ####

setwd('~/Dropbox/Solent Bird Studies/Data analysis/Solent Bird Network')
require(plyr)
require(magrittr)
require(readr)


#### Load all data ####

## WeBS data (read from parent folder)
webspplist <- read.csv("../WeBS_species_list.csv")


## Solent Birds data

# load separate data files and 
# check for compatibility amongst them & with WeBS
source('compatibility.R')
# NOTE: the warning
# "In function_list[[k]](value) : NAs introduced by coercion"
# is no problem - it's a deliberate part of fixing incorrectly-recorded
# (non-numeric) longitudes :)

# clean up associated visit information
source('visit_info.R')

# join bird sightings to visit info
# and join all solent data sets together
source('join_all_counts.R')
# output data frame is 'solent'

## filter this data by times
source('data_filters.R') 
# output data is solent_filter


## Spatial data

# join to Strategy polygons and filter by space
source('spatial_join_final.R') ## 


# remove any duplicate entries (same date, time, recorder and polygon)
source('fix_duplicates.R')



#### Summarise by polygons ####

# confidence and importance
source('confidence_importance.R')


# maps
source('maps_importance.R')


#### Compare with old Strategy ####


#### Waders ####


### predicted to be cat A but low/no use

# load table of previous categories
unused <- read.csv("predicted_but_unused.csv")
# add current info
unused_wad <- join(unused, wader_sites)
unused_wad # view results
summary(unused_wad$Importance) # all 4 visited are used; 10 not visited
# write.csv(unused_wad, file='suitable_but_unused.csv', row.names = F, na = '')


### predicted to be cat A but uncertain
uncertain <- read.csv("predicted_but_uncertain.csv")
uncertain_wad <- join(uncertain, wader_sites)
uncertain_wad
summary(uncertain_wad$Importance) # mostly still uncertain!
# write.csv(uncertain_wad, file='suitable_but_uncertain.csv', row.names = F, na = '')



### most used sites
# old maximum counts
old_max <- read.csv("old_max_counts.csv")
old_max_wad <- join(old_max, wader_sites)

# add site stats
fixnames<-function(x,y){names(x)[2] <- y; return(x)}
old_max_wad <- join(old_max_wad, fixnames(wader_site_maxcnt, 'Max'))
old_max_wad <- join(old_max_wad, fixnames(wader_site_meancnt, 'Mean'))
old_max_wad <- join(old_max_wad, fixnames(wader_site_nospp, 'NoSpp'))
old_max_wad # declines in recording and apparently in bird numbers

# write.csv(old_max_wad, file='Site_stats.csv', row.names = F, na='')
## note that >1000 is B, over 5000 is C




#### Brent Geese #####

# predicted to be Major or High importance but unused
unused_Brents <- read.csv("predicted_but_unused_Brents.csv")
unused_Brents %<>% join(brent_sites)
unused_Brents
summary(unused_Brents$Importance) # mostly still unused / unvisited



#### look up sitenames ####
solent_jdata[solent_jdata$Site_ID=='P73',"Sitename"]
