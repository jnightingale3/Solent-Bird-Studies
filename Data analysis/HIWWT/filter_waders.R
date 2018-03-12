# Data to ensure that only wader records are included 
# for HIWWT wader results

solent_filter %<>% subset(Species %in% waders)
