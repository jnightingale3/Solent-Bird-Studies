### Calculate average counts for each target species at each subsite
### using WeBS data
require(plyr)
require(magrittr)

### how many sectors covered per visit?
websNvis <- ddply(websdf, .(Visit, Species), function(x) {
  .out <- data.frame(
    Sum = sum(x$Count),
    N = nrow(x)
  )
}); websNvis <- ddply(websNvis, .(Visit), function(x) max(x$N, na.rm=T) )
names(websNvis)[2] <- 'N'
Nmax <- max(websNvis$N)

### select only species of interest from SPA designations
spp_webs <- c('Brent Goose (Dark-bellied)', 'Wigeon', 'Teal', 'Pintail', 
              'Little Egret', 'Ringed Plover', 'Grey Plover', 'Dunlin', 'Ruff', 
              'Bar-tailed Godwit', 'Black-tailed Godwit', 'Curlew', 'Greenshank', 
              'Redshank', 'Oystercatcher', 'Avocet', 'Sanderling', 'Turnstone')

### high tide counts to 
### only include target species and months of interest
htc <- subset(websdf, month %in% select_months 
              # & Species %in% spp_webs
) %>% droplevels
htc$year <- as.numeric(format(htc$date, '%Y'))
# assign early months to previous year's winter
htc$year[htc$month %in% c('January', 'February', 'March')] %<>% subtract(1)

htc2 <- join(htc, websNvis)
htc <- htc2; rm(htc2)

# averages and other summary stats for each sector
# htc_sectors <- ddply(htc, .(sector, Species), function(x) {
#   .out <- data.frame(
#     Mean = mean(x$Count),
#     Median = median(x$Count),
#     StDev = sd(x$Count),
#     Min = min(x$Count),
#     Max = max(x$Count)
#   )
#   return(.out)
# })




# totals for each visit per species
htc_visits <- ddply(htc, .(Visit, Species), function(x) {
  .out <- data.frame(
    Sum = sum(x$Count),
    Total = nrow(x),
    Corr.Totl = sum(x$Count * (Nmax / x$N)),
    Max = max(x$Count)
  )
})


# find the year
# using regular expression to remove everything before the - in Visit
htc_visits$Year <- as.integer( gsub('^[^-]*-', '', htc_visits$Visit) )
htc_visits <- join(htc_visits, websNvis)


head(htc_visits)


## check for bias
with(htc_visits, plot(N, Total)) # number of sectors visited vs no. where recorded
with(htc_visits, cor.test(N, Total, method='sp'))
with(htc_visits, plot(N, Corr.Totl))
with(htc_visits, cor.test(N, Corr.Totl, method='sp'))
with(htc_visits, plot(N, log1p(Corr.Totl)))
with(htc_visits, boxplot(log1p(Corr.Totl) ~ N))
with(htc_visits, plot(N, Sum)) # number of sectors visited vs number of indivs seen
with(htc_visits, cor.test(N, Total, method='sp'))
with(subset(htc_visits, Sum!=0), plot(N, Sum)) # number of sectors visited vs number of indivs seen
with(subset(htc_visits, Sum!=0), cor.test(N, Total, method='sp'))



with(subset(htc_visits, Sum != 0), boxplot(Sum ~ N))
with(subset(htc_visits, Sum != 0), boxplot(Corr.Totl ~ N))
with(subset(htc_visits, Sum != 0), boxplot(log1p(Sum) ~ N))
with(subset(htc_visits, Sum != 0), boxplot(log1p(Corr.Totl) ~ N))

# overall summaries for each species
totals_species <- ddply(htc_visits, .(Species), function(x) {
  .out <- data.frame(
    Mean_Total = mean(x$Sum),
    Mean_Corr = mean(x$Corr.Totl),
    StDevTotal = sd(x$Sum),
    StDevCorr = sd(x$Corr.Totl),
    Max_Total = max(x$Sum),
    Max_Corr = max(x$Corr.Totl),
    Overall_max = max(x$Max)
  ) %>% round(1)
  return(.out)
})
totals_species[,1:3]
subset(totals_species, Mean_Total > 1)[,1:3]


### yearly totals
### use max N. sector surveyed per winter as proxy for N. of sampling units
totals_months <- ddply(htc, .(month, Species, year), function(x) {
  .out <- data.frame(Sum = sum(x$Count), N = nrow(x))
  return(.out) # total for all sectors 
})  

annual_peak <- ddply(totals_months, .(Species, year), function(x) {max(x$Sum)})
webs_5yrave <- ddply(annual_peak, .(Species), function(x) {
  round( mean(x$V1), digits=1) })

names(webs_5yrave)[2] <- 'AnnPeak'
webs_5yrave$Mean_Total <- totals_species$Mean_Total
webs_5yrave$Mean_Corr <- totals_species$Mean_Corr

pairs(webs_5yrave[,-1], panel = function(x,y,...){
  points(x,y,...)
  abline(a = 0,b = 1,...)
})

webs_5yrave
subset(webs_5yrave, AnnPeak > 50)


totals_years <- ddply(totals_months, .(Species, year), function(x) {
  .out <- data.frame(
    #Mean_mean = mean(x$Mean), Max_mean = max(x$Mean),
    Total_mean = mean(x$Sum, na.rm=T), sd_mean = sd(x$Sum, na.rm=T),
    SE = sd(x$Sum, na.rm=T) / sqrt(6)#sqrt(max(x$N))
  ) %>% round(1)
  return(.out)
})

## error bars - +/- one standard error
totals_years$ul <- totals_years$Total_mean + totals_years$SE
totals_years$ll <- totals_years$Total_mean - totals_years$SE
totals_years$ll[totals_years$ll <0 ] <- 0 # no negative error bars in plot!

## plot yearly trends
require(ggplot2)
q <- ggplot(totals_years, aes(x=year, group=Species))
q <- q + geom_ribbon(aes(ymax=ul, ymin=ll), fill='lightgrey')
q <- q + geom_path(aes(y=Total_mean)) + geom_point(aes(y=Total_mean))
q <- q + facet_wrap(~Species, scales='free') + theme_bw()
q <- q + xlab('Winter beginning') + ylab('Mean monthly WeBS count')
print(q)


####################################

# with(totals_months, plot(N, Sum))
# with(totals_months, plot(N, log1p(Sum)))
# with(totals_months, cor.test(N, Sum, method='spearman'))

#### trends in Brents over time ####

# ### separate dataset
# web_brents <- subset(htc, Species=='Brent Goose (Dark-bellied)')
# 
# ## and time of year - because 2009 and 2014 are half-seasons only
# web_brents$period <- 'Early'
# web_brents$period[web_brents$month %in% c('October', 'November', 'December')
#                   ] <- 'Late' 
# 
# with(web_brents, boxplot(Count ~ year)) # looks like a modest increase
# 
# 
# p <- ggplot(web_brents, aes(x=year, y=Count)) 
# p <- p + geom_boxplot(aes(group=year), alpha=.5)
# p <- p + geom_point(aes(fill=period, color=period)) 
# p <- p + geom_smooth(method='glm', formula=y~x,
#                      method.args=list(family=quasipoisson(link="log")))
# p <- p + theme_bw() + ylab('Dark-bellied Brent Geese') + xlab('Year')
# p <- p + theme(legend.position='none')
# print(p)
# 
# nnn <- glm(Count ~ year+period, data=web_brents, family=quasipoisson(link="log"))
# summary(nnn) # significant increase in number of brents over time
#              # late period has fewer geese than early period