# read all the Chimet files and join and clean

require(plyr)
require(magrittr)

chi_list <- paste0('./Data_Gauges/Chi/', list.files('./Data_Gauges/Chi') )

chi_files <- lapply(chi_list, read.csv)

chi_all <- join_all(chi_files, type='full')


chi_fix <- chi_all

chi_fix$DATE %<>% as.character
chi_fix$Date %<>% as.character
chi_fix$TIME %<>% as.character
chi_fix$Time %<>% as.character

chi_fix$DATE[is.na(chi_fix$DATE)] <- chi_fix$Date[is.na(chi_fix$DATE)]
chi_fix$TIME[is.na(chi_fix$TIME)] <- chi_fix$Time[is.na(chi_fix$TIME)]

chi_fix$datetime <- paste(chi_fix$DATE, chi_fix$TIME) %>% 
  as.POSIXct(format='%d/%m/%Y %H:%M')

chi_fix %<>% subset(select=c(datetime, DEPTH)) %>% na.omit

names(chi_fix) <- c('time', 'h')


write.csv(chi_fix, file='chi.csv', row.names = F)
# write.csv(chi_all, file='chi_all.csv', row.names=F)
