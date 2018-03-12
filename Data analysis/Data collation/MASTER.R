### load all data from scratch
### process it into a single, consistent format
### ready for analysis 




#### Setup: load packages ####

require(plyr)
require(magrittr)
# require(readr)
require(rgdal)


# load shapefile containing the strategy codes
stratcode <- readOGR(dsn='../../Solent Bird Studies GIS Data/HIWWT Strategy Layer', layer='SWBGS')



### load species list of all species recorded in the Solent
#(derived from WeBS data)
webspplist <- read.csv("../WeBS_species_list.csv")



### Load Solent Birds data

# load separate data files and 
# check for compatibility amongst them & with WeBS
source('compatibility.R')
# NOTE: the warning
# "In function_list[[k]](value) : NAs introduced by coercion"
# is no problem - it's a deliberate part of fixing incorrectly-recorded
# (non-numeric) longitudes.

# clean up associated visit information
source('visit_info.R')

# join bird sightings to visit info
# and join all solent data sets together
source('join_all_counts.R')


# add Eastings and Northings to records which don't have them
# but have recorded a strategy code
# using the shapefile loaded above
source('swbg_record_coords.R')


#### write joined data to a CSV
#### note that this will overwrite the previous version without warning!
# write.csv(solent, file='Data/solent.csv', na="", row.names = F)
