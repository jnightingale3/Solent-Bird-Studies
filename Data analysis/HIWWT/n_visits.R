
#### Calculate umber of visits ####


### only use counts from within last 10 years
today <- Sys.Date()
limit <- today - years(10)

# split out those data
vis10y <- subset(solent_jdata, Date >= limit)


### number of visits to each site
sitevisits <- ddply(vis10y, .(Site_code), nrow)
names(sitevisits)[2] <- 'Visits'