require(magrittr)
require(plyr)

all <- list.files() %>% extract(
  grepl( '.csv', list.files(), fixed=T)
) %>% lapply(read.csv, stringsAsFactors=F) %>% 
  join_all(type='full')


str(all)

all$Map.cross.ref[which(all$Unique.code==''&is.na(all$Map.cross.ref))] <- 
  all$Map.Cross.Ref[which(all$Unique.code==''&is.na(all$Map.cross.ref))]

all.out <- subset(all, select=c(Unique.code, Date, Time, Species, Count, Comments))

all.out$Map.cross.ref <- gsub(' ', '', all$Map.cross.ref, fixed=T)

write.csv(all.out, file='Snapshot_all.csv', row.names = F)
