#### script to take Solent surveys and calculate totals for each species
#### on the species list used in those surveys - including all zero counts
#### and then join complete totals to the old data

#### NB: need to have all WeBS data and both Solent surveys loaded and 
#### edited for compatibility - use data_join.R and compatibility.R


## setting up 

# load in Solent surveys' species list
species_list <- read.csv("../Target_species_list.csv")
# and create species lists from each dataset
birds_splist <- unique(birds$sp)[which(unique(birds$sp) %in% negrecs == FALSE)]
clean_splist <- unique(clean$sp) # doesn't contain negative record pseudo-taxa



###### which species from the species_list were seen?


#### dataset: birds ####

bseen <- expand.grid(       # creates table of every combination of
  unique(birds$Visit.ID),   # unique visit IDs and
  species_list$Species      # species on the target species list
)
names(bseen)[1:2] <- c('Visit.ID', 'sp') # standardise column names

# columns of species-visit combinations
bseen$spvis <- sprintf('%s %s', bseen$Visit.ID, bseen$sp)
birds$spvis <- sprintf('%s %s', birds$Visit.ID, birds$sp)

# match the visit-target species combinations across datasets
matcher <- match(bseen$spvis, birds$spvis)

# extract total number seen
bseen$count <- birds$Number.See[matcher]
# What % of these are NAs?
sum(is.na(bseen$count)) / nrow(bseen) * 100 # almost all - 98%
bseen$count[is.na(bseen$count)] <- 0 # set NAs to 0 as they represent negative records



#### dataset: clean ####

cseen <- expand.grid(       # creates table of every combination of
  unique(clean$visitid),    # unique visit IDs and
  species_list$Species      # species on the target species list
)
names(cseen)[1:2] <- c('visitid', 'sp')

# columns of species-visit combinations
cseen$spvis <- sprintf('%s %s', cseen$visitid, cseen$sp)
clean$spvis <- sprintf('%s %s', clean$visitid, clean$sp)

# match the visit-target species combinations across datasets
matcher_c <- match(cseen$spvis, clean$spvis)

# extract total number seen
cseen$count <- clean$Number[matcher_c]
# What % of these are NAs?
sum(is.na(cseen$count)) / nrow(cseen) * 100 # almost all - 94%
cseen$count[is.na(cseen$count)] <- 0 # set NAs to 0 as they represent negative records



#### dataset: swbg ####

sseen <- expand.grid(       # creates table of every combination of
  unique(swbg$visitid),    # unique visit IDs and
  species_list$Species      # species on the target species list
)
names(sseen)[1:2] <- c('visitid', 'sp')

# columns of species-visit combinations
sseen$spvis <- sprintf('%s %s', sseen$visitid, sseen$sp)
swbg$spvis  <- sprintf('%s %s', swbg$visitid,  swbg$sp)

# match the visit-target species combinations across datasets
matcher_s <- match(sseen$spvis, swbg$spvis)

# extract total number seen
sseen$count <- swbg$Count[matcher_s]
# What % of these are NAs?
sum(is.na(sseen$count)) / nrow(sseen) * 100 # almost all - 98.9%
sseen$count[is.na(sseen$count)] <- 0 # set NAs to 0 as they represent negative records


# keeps things tidy
rm(matcher, matcher_c, matcher_s)

#### how does this compare with WeBS?

## this will only work if the websdf data frame has been loaded
## via the script core_join.R in the folder WeBS

# # raw WeBS data including all species on *their* list:
# sum(websdf$Count < 1) / nrow(websdf) * 100 # quite a lot - 70%
# # and using the matching process
# wseen <- expand.grid(       # creates table of every combination of
#   unique(sprintf('%s%s', websdf$Visit, websdf$sector)),    # unique visit IDs and
#   species_list$Species      # species on the target species list
# )
# # columns of species-visit combinations
# wseen$spvis <- sprintf('%s %s', wseen$Var1, wseen$Var2)
# websdfspvis <- sprintf('%s%s %s', websdf$Visit,websdf$sector, websdf$Species)
# 
# # match the visit-target species combinations across datasets
# matcher_w <- match(wseen$spvis, websdfspvis)
# 
# # extract total number seen
# wseen$count <- websdf$Count[matcher_w]
# # What % of these are NAs?
# sum(is.na(wseen$count)) / nrow(wseen) * 100 # comparatively few - 24%
# rm(wseen, matcher_w)