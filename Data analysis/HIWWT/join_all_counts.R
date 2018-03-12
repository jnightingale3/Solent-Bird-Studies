## join all data sources together 
## and perform final standardisation

require(R.utils) # for capitalize

# check that all column names are consistent 
all.equal(names(birds_visitinfo), names(clean_visitinfo), names(swbg_visitinfo),
          names(sb_visitinfo), names(hiwwt_visitinfo), names(snap_visitinfo, es_visitinfo)) # yes

solent <- rbind(birds_visitinfo, clean_visitinfo, swbg_visitinfo, sb_visitinfo,
                hiwwt_visitinfo, snap_visitinfo, es_visitinfo)

rm(birds, birds_visitinfo, clean, clean_visitinfo, hiwwt, hiwwt_visitinfo, 
   sb, sb_visitinfo, swbg, swbg_visitinfo, snap, snap_visitinfo, es, es_visitinfo)


# add a date-time column
solent$datetime <- solent$Date %>% paste( format(solent$Time, '%H:%M') ) %>%
  as.POSIXct(format='%Y-%m-%d %H:%M')



# add Eastings and Northings to records from the SW&BG2016 dataset
# which was largely without them
# using the old shapefile 'Waders & Brent Geese Sites_region'
source('swbg_record_coords.R')




#### standardise recorders' names
## by changing 'Surname, Firstname' to 'Firstname Surname',
## capitalizing names and correcting nicknames & duplicates where possible
## mainly to allow checking for duplicates later

# fix comma situation

# which names have commas?
hascomma <- grepl(',', solent$Recorder)

# what is the position of the comma in the text string?
poscomma <- regexpr(',', solent$Recorder)

# how long is the text string?
namelength <- nchar( as.character(solent$Recorder) )

# surnames are the text before the comma 
surnames <- substr(solent$Recorder, 1, poscomma-1) %>% capitalize
# firstnames come after the space after the comma
firstnames <- substr(solent$Recorder, poscomma+2, namelength) %>% capitalize

# new vector for fixed names
fixnames <- rep(NA, length(hascomma))
# names without commas are preserved
fixnames[!hascomma] <- as.character(solent$Recorder[!hascomma])
# otherwise, join names together in correct order
fixnames[hascomma] <- paste(firstnames, surnames)[hascomma]

# capitalise both halves of names
fixnames <- sapply(fixnames, function(x) {
  paste(sapply(strsplit(x, ' '), capitalize), collapse=' ')
})

# change nicknames etc
fixnames %<>% replace(which(fixnames=='Nicky H'), 'Nicky Horter')
fixnames %<>% replace(which(fixnames=='Ed Test'), 'Ed Rowsell')

# add to original data set
solent$Recorder <- factor(fixnames)




## assume that NA counts should be 0
solent$Count[is.na(solent$Count)] <- 0


# clean up
rm(hascomma, poscomma, fixnames, firstnames, namelength, surnames)




#### write joined data to a CSV
#### note that this will overwrite the previous version without warning!
 # write.csv(solent, file='Data/solent.csv', na="", row.names = F)

# write random subsample of data to CSV to test loading into QGIS
# write.csv( solent[sample(1:nrow(solent), 100, replace=F),] , 
#                   file='solent_test.csv', na="", row.names = F)