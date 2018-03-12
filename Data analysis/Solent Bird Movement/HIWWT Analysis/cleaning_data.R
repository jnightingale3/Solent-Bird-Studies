library(magrittr)

# load data
x <- read.csv("N:/Filing/15. Projects/15.00 Ecology and Evidence Projects/15.04.06 Wader & Brent Goose Strategy/1. SW&BG Strategy 2018/6. Survey Results/12. FINAL ANALYSIS/cleaning_temp.csv")

# check it out
str(x)


# what do the RecordIDs look like?
levels(x$RecordID)
# which(x$RecordID == '\tCO5') # FIXED

# what are OKnown and DKnown like?
levels(x$Oknown) # fine
levels(x$Dknown) # full of comments in the wrong box! # FIXED

# Create subset for movements
# to make sure they all have Oknown and Dknown filled correctly
mov <- subset(x, grepl('MO', x$RecordID)) %>% droplevels

levels(mov$Oknown) # are any lacking origin info?
mov$UniqueID[which(mov$Oknown == '')] # FIXED

levels(mov$Dknown) # are any lacking destination info?
mov$UniqueID[which(mov$Dknown == '')] # FIXED


# Strategy codes - check that none have letter O instead of number 0, etcetera
levels(x$FieldID_Origin)#[grep('VP', levels(x$FieldID_Origin))]
which(x$FieldID_Origin=='VP9') # FIXED
grep('VP', levels(x$FieldID_Origin))
levels(x$Destination_code) # 
which(x$Destination_code == "'Usual' birds moved elsewhere.")
