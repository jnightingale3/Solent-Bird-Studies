### loading all data from scratch and processing it

### ending with a summary of which strategy sites are important,
### compared with earlier strategy assessments

### and whether any fields previously assessed as 'suitable but unused'
### are now used


#### Setup ####

require(plyr)
require(magrittr)
require(readr)

# load data from Data collation
solent <- read.csv('../Data collation/Data/solent.csv')
# fix time columns
solent$Date <- as.POSIXct(solent$Date, format='%F')
solent$Time <- as.POSIXct(solent$Time, format='%H:%M')

## filter this data by times
source('data_filters.R') 
# output data is solent_filter


#### Optional species filters

# For ESCP analyses, leave all options below commented out

# For HIWWT analyses, separate results are required for 
# Brent Geese
# source('filter_brent.R')
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
