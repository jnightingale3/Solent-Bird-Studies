## join all data sources together 
## and perform final standardisation

require(magrittr)
require(R.utils) # for capitalize

# check that all column names are consistent 
all.equal(names(birds_visitinfo), names(clean_visitinfo), names(swbg_visitinfo),
          names(sb_visitinfo), names(hiwwt_visitinfo), names(snap_visitinfo), names(es_visitinfo)) # yes

solent <- rbind(birds_visitinfo, clean_visitinfo, swbg_visitinfo, sb_visitinfo,
                hiwwt_visitinfo, snap_visitinfo, es_visitinfo)

rm(birds, birds_visitinfo, clean, clean_visitinfo, hiwwt, hiwwt_visitinfo, 
   sb, sb_visitinfo, swbg, swbg_visitinfo, snap, snap_visitinfo, es, es_visitinfo)


# fix time format
solent$Time %<>% format(format='%H:%M')

# add a date-time column
solent$datetime <- paste(solent$Date, solent$Time)  %>%
  as.POSIXct(format='%Y-%m-%d %H:%M')



##### Fix Brent Goose nomenclature #####

solent$Species[which(solent$Species=='Brent Goose (dark-bellied)')] <-
  'Brent Goose (Dark-bellied)'
solent$Species[which(solent$Species=='Brent Goose')] <-
  'Brent Goose (Dark-bellied)'


##### Standardise terminolgy for negative records #####
negrecs <- c('None','no geese','no waders','no brent geese','No birds seen')

solent$Species[solent$Species %in% negrecs] <- 'None'
solent$Species %<>% droplevels



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
