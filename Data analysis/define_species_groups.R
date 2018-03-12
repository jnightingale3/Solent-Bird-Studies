## Script to define various groups of species used throughout analysis

## e.g., waders or domestic species


#### all species recorded in WeBS data ####

spp <- as.character( read.csv('../WeBS_species_list.csv')$sp )


#### Waders ####

waders<-c('Oystercatcher', 'Avocet', 'Ringed Plover', 'Golden Plover', 'Grey Plover',
          'Lapwing', 'Knot', 'Curlew Sandpiper', 'Dunlin', 'Ruff', 'Snipe', 
          'Black-tailed Godwit', 'Bar-tailed Godwit', 'Whimbrel', 'Curlew', 
          'Common Sandpiper', 'Green Sandpiper', 'Spotted Redshank', 'Greenshank',
          'Redshank', 'Turnstone', 'American Golden Plover', 'Semipalmated Sandpiper',
          "Baird's Sandpiper", 'Pectoral Sandpiper', 'Long-billed Dowitcher', 
          'Grey Phalarope', 'Semipalmated Plover', 'Little Ringed Plover',
          'Purple Sandpiper', 'Sanderling', 'Jack Snipe')


#### Seabirds ####
seabirds <- c('Gull', 'Tern', 'Diver', 'Grebe', 'Shag', 'Merganser', 'Kittiwake',
              'Kingfisher', 'Petrel', 'Merlin')


#### species to include ####
goodspp <- spp[!grepl(paste(c('Domestic', 'domestic', seabirds), collapse='|'), 
                  ignore.case=F, spp)]

# remove seabirds and domestic forms
get_goodspp <- function(x) {
  return(x[!grepl(paste(c('Domestic', 'domestic', seabirds), collapse='|'), 
                      ignore.case=F, x)])
}

# list without hybrids and unidentified
nohybrids <- function(x) {
  x[!grepl(paste(c('hybrid', 'unidentified', 'sp.'), collapse='|'), ignore.case=T, x)]
}
