## script to summarize WeBS core data by sector; year; period (early/late)

require(plyr)
require(magrittr)

#### Monthly occurrence and abundance per species ####
htc_visits <- ddply(htc, .(Visit, Species), function(x) {
  .out <- data.frame(
    Occurrence = sum(x$Count > 0),
    Total = sum(x$Count),
    Corr.Totl = sum(x$Count * (Nmax / x$N)),
    Max = max(x$Count)
  )
})

# find the year
# using regular expression to remove everything before the - in Visit
htc_visits$Year <- as.integer( gsub('^[^-]*-', '', htc_visits$Visit) )
htc_visits <- join(htc_visits, websNvis)

head(htc_visits, 12)


# exclude seabirds

htc_assemblage_visit <- ddply(htc_visits_ourbirds, .(Visit), function(x) {
  .out <- data.frame(Total = sum(x$Total, na.rm = T),
                     Corr.Totl = sum(x$Corr.Totl, na.rm = T) )
})
htc_assemblage_visit$Year <- as.integer( # add year
  gsub('^[^-]*-', '', htc_assemblage_visit$Visit) )


#### Monthly totals ####
totals_months <- ddply(htc, .(Species, month, year), function(x) {
  .out <- data.frame(Sum = sum(x$Count))
  return(.out) # total for all sectors 
})  


#### Yearly totals ####
annual_peak <- ddply(totals_months, .(Species, year), function(x) {max(x$Sum)})


#### Long-term averages ####
webs_5yrave <- ddply(annual_peak, .(Species), function(x) {
  round( mean(x$V1), digits=1) }) %>% subset(Species %in% ourbirds)

names(webs_5yrave)[2] <- 'Peak'
webs_totals <- subset(webs_5yrave, Peak >5)
webs_totals # average population per species

# write webs_totals to CSV file
# write.csv(webs_totals, file='webs_totals.csv', row.names = F)