# Master script to run movement network analysis


# load movement data and calculate origins and destinations
source('origins_and_destinations.R') # all ESCP movements
source('add_hiwwt_records.R')

# join terrestrial sites to WeBS sectors (offshore sites)
source('join_sectors.R')

# create smaller version of map only including relevant harbours
source('extract_polygons.R')

# plot network, and analyse its properties
source('network_vis.R')