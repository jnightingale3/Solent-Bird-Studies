### Solent and Wader Brent Goose Strategy analysis ESCP

### This master script will load collated data
### and process it, producing GIS layers of results



#### Setup ####

require(magrittr)
require(rgdal)
require(chron)

#### Load all data ####

## WeBS species list
webspplist <- read.csv("../WeBS_species_list.csv")


## shapefile giving strategy codes
stratcode <- readOGR(dsn='../../Solent Bird Studies GIS Data/HIWWT Strategy Layer', 
                     layer='SWBGS')

## shapefile of sites to be analysed
polys <- readOGR(dsn='../../Solent Bird Studies GIS Data/Asterix Sites', 
                          layer='asterisk_diss_simple')
polys %<>% spTransform(stratcode@proj4string) # reproject to same CRS
polys$Site_code <- polys$Site_name # need a column called Site_code containing unique identifiers of sites


### all solent bird records
solent <- read.csv('Data/solent.csv')
# sort out time formats
solent$Date %<>% as.POSIXct
solent$Time %<>% as.POSIXct(format='%H:%M') %>% format('%T') %>% times
solent$datetime %<>% as.POSIXct(format='%Y-%m-%d %H:%M:%S')

## filter this data by times
source('data_filters.R') 
# output data is solent_filter




#### Spatial data

# make single spatial dataset of all records
source('record_importance.R')
# this file sources the 'assign_SPA.R' and 'assign_webs.R' files 

# join to Strategy polygons and filter by space
source('spatial_join_final.R') 



# remove any duplicate entries (same date, recorder and polygon)
source('fix_duplicates.R')



#### Summarise by polygons ####

source('site_scores.R')
# this file sources the 'assign_gauge.R' file

#### Look at maps
# source('prelim_maps.R')


#### Export results
source('export_final_results.R')
