### loading all data from scratch and processing it

### ending with a summary of which strategy sites are important,
### compared with earlier strategy assessments

### and whether any fields previously assessed as 'suitable but unused'
### are now used


#### Setup ####

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


#### Optional species filters

# For ESCP analyses, leave all options below commented out

# For HIWWT analyses, separate results are required for 
# Brent Geese
source('filter_brent.R')
# and waders
# source('filter_waders.R')
# and BOTH waders AND brent
# source('filter_waders_AND_brent.R')

#### analysis continues below




#### Spatial data

# make single spatial dataset of all records
source('record_importance.R')
# this file sources the 'assign_*.R' files 

# join to Strategy polygons and filter by space
source('spatial_join_final.R') ## 



# remove any duplicate entries (same date, time, recorder and polygon)
source('fix_duplicates.R')



#### Summarise by polygons ####

source('site_scores.R')


#### Look at maps
# source('prelim_maps.R')


#### Export results
source('export_final_results.R')
