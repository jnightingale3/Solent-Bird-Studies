### Code for converting Eastings and Northings to 6 figure OS Grid Refs

## adapted from python code at http://oobrien.com/2010/02/en-to-gridref-in-python/
## which was based on http://www.movable-type.co.uk/scripts/latlong-gridref.html

ogr_convert <- function(e, n) { # e is Eastings, n is Northings
  
  # Note that letter I is not used in grid refs
  grid_chars <- LETTERS[-9]
  
  # get the 100km-grid indices
  e100k = floor(e/100000)
  n100k = floor(n/100000)
  
  # not sure what this bit is meant to do?
  # if e100k6 or n100k12:
    # return ''
  
  # translate those into numeric equivalents 
  # of the grid letters
  l1 = (19-n100k)-(19-n100k) %% 5 + floor((e100k+10)/5)
  l2 = (((19-n100k)*5) %% 25) + (e100k %% 5)
  
  # compensate for skipped 'I' and build grid letter-pairs
  l1[l1 > 7] <- l1[l1 > 7] + 1
  l2[l1 > 7] <- l2[l1 > 7] + 1
  let_pair = sprintf('%s%s', grid_chars[l1],  grid_chars[l2])
  
  # strip 100km-grid indices from easting & northing,
  # round to 100m
  e100m = floor(round(e/100))
  # add trailing whitespace to numbers with only 3 digits
  egr = substr(format(e100m, justify = 'right', width=4), 
                start=2, stop=4) # then select last 3 digits (i.e., remove the first)
    
  n[n >= 1000000] <- n[n >= 1000000] - 1000000 # Fix Shetland northings
  n100m = floor(round(n/100))
  ngr = substr(format(n100m, justify = 'right', width=4), start=2, stop=4)
  
  return(paste(let_pair, egr, ngr, sep=''))
  
}
