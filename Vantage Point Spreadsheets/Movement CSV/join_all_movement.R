require(magrittr)
require(plyr)

all <- list.files() %>% extract(
  grepl( '.csv', list.files(), fixed=T)
) %>% lapply(read.csv, stringsAsFactors=F) %>% 
  join_all(type='full')


str(all)


all.out <- subset(all, select=c(Unique.Code, Date, Time, Species, Count, 
                                Origin, Oknown, Destination, Dknown, Comments))

all.out$Map.cross.ref <- gsub(' ', '', all$Map.cross.ref, fixed=T)

write.csv(all.out, file='Movement_all.csv', row.names = F)
