# Data to ensure that only Brent records are included 
# for HIWWT Brent results

solent_filter %<>% subset(Species == 'Brent Goose (Dark-bellied)')
