# include both brents and waders for HIWWT analysis

solent_filter %<>% subset(Species %in% waders |
                            Species=='Brent Goose (Dark-bellied)')
