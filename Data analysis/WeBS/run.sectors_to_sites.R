### uses the separate shape files for each low tide sites
### looks for overlap of core count sectors with those sites
### and creates a data frame saying which site each sector overlaps with

require(sp) # for over and cbind.spatial functions

# Site shapefiles are: bea, bem, chi, lan, new, nws, pag, por, sou
htc_sites <- cbind(over(core_sects, bea), core_sects@data$LOC_LABEL)
htc_sites[!is.na(over(core_sects, bem)$Site), 1:4] <- na.omit(over(core_sects, bem))
htc_sites[!is.na(over(core_sects, chi)$Site), 1:4] <- na.omit(over(core_sects, chi))
htc_sites[!is.na(over(core_sects, lan)$Site), 1:4] <- na.omit(over(core_sects, lan))
htc_sites[!is.na(over(core_sects, new)$Site), 1:4] <- na.omit(over(core_sects, new))
htc_sites[!is.na(over(core_sects, nws)$Site), 1:4] <- na.omit(over(core_sects, nws))
htc_sites[!is.na(over(core_sects, pag)$Site), 1:4] <- na.omit(over(core_sects, pag))
htc_sites[!is.na(over(core_sects, por)$Site), 1:4] <- na.omit(over(core_sects, por))
htc_sites[!is.na(over(core_sects, sou)$Site), 1:4] <- (
  over(core_sects, sou))[!is.na(over(core_sects, sou)$Site), ]

# rename LOC_LABEL column
names(htc_sites)[5] <- 'sector'

# remove duplicated column

htc_sites <- htc_sites[!duplicated(htc_sites$sector),]

# some high tide sites do not overlap with low tide counts
# assign them to their sites manually for calculating totals
# but in a different column, so as not to influence comparisons between htc/ltc

htc_sites$siteman <- htc_sites$Site
htc_sites$siteman[which(htc_sites$sector %in% c('17012', '17013'))] <- 
  'North-west Solent'
htc_sites$siteman[which(htc_sites$sector %in% c('17371', '17915', '17917', 
                                                '17331'))] <- 'Southampton Water'
htc_sites$siteman[which(htc_sites$sector %in% c('18401'))] <- 'Yar Estuary'
htc_sites$siteman[which(htc_sites$sector %in% c('18403'))] <- 'Medina Estuary'
htc_sites$siteman[which(htc_sites$sector %in% c('18001', '18404'))] <- 'Wootton Creek'
htc_sites$siteman[which(htc_sites$sector %in% c('18405'))] <- 'Kings Quay'
htc_sites$siteman[which(htc_sites$sector %in%  c('18407', '18410'))] <- 
  'Ryde Pier to Puckpool Point'
htc_sites$siteman[which(htc_sites$sector %in% c('18406'))] <- 'Foreland'
htc_sites$siteman[which(htc_sites$sector %in% c('18408'))] <- 'Thorness Bay'
htc_sites$siteman[which(htc_sites$sector %in% c('17231'))] <- 'Langstone Harbour'
htc_sites$siteman[which(htc_sites$sector %in% c('17332'))] <- 'Portsmouth Harbour'
htc_sites$siteman[which(htc_sites$sector %in% c('17438', 
                                                '17439'))] <- 'South Hayling Seafront'
htc_sites$siteman[which(htc_sites$sector %in% c('20X01', '20807', '20807', '20806', 
                                             '20804', '20803', '20808'))] <- 'Medmerry'
htc_sites$siteman %<>% as.character

htc_sites$LOC_LABEL <- htc_sites$sector

## also want a polygon layer for Sites (not sectors)
webs_sites <- core_sects
webs_sites@data %<>% join(subset(htc_sites, LOC_LABEL %in% core_sects$LOC_LABEL))
