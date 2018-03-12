#### Master script for WeBS analysis ####

### read in data

source('load.count_data.R')
source('load.spatial_data.R')


### analyse data
source('run.analysis.R')


### make plots

source('run.plots.R') # note that most plots are not printed using source()
                      # need to run line-by-line to obtain images
