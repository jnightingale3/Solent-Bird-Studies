# Script to analyse eBird records

# read in data
source('load.count_data.R')
source('load.spatial_data.R')

# analyse data and create sub-datasets
source('run.analysis.R')

# print plots and results tables; statistcal tests; etc
source('run.plots.R')