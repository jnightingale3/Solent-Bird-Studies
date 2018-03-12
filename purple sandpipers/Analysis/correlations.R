require(magrittr)

# load data

hos <- read.csv('hos.csv', header = T, na.strings = '') %>% subset(Count > 0)

hos$Date %<>% as.character()
hos$Time %<>% as.character()

hos$datetime[which(!is.na(hos$Time))] <- with(subset(hos, !is.na(Time)), sprintf('%s %s', Date, Time))

hos$Date %<>% as.POSIXct(format='%d/%m/%Y')
hos$Time %<>% as.POSIXct(format='%T')
hos$datetime %<>% as.POSIXct(format='%d/%m/%Y %T')

pairs(hos)


##### Time analyses #####


#### Time of year ####
hos$doy <- as.numeric(format(hos$Date, '%j')) # day of year
hos$doyx <- hos$doy; hos$doyx[which(hos$doy<200)] <- hos$doy[which(hos$doy<200)]+365
hos$year <- as.numeric(format(hos$Date, '%Y')) # year only

with(hos, plot(doy, Count))

require(plyr)
purpday <- ddply(hos, .(doy), nrow)

require(ggplot2)
p <- ggplot(hos,aes(x=doyx, y=Count))
p <- p + geom_smooth() + geom_point(shape=21) + theme_classic() + xlab('') + 
  scale_x_continuous(breaks=c(213, 244, 274, 305, 335, 366, 397, 425, 456, 486, 517, 547),
                     labels=c('Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul')) +
  ylim(0,max(hos$Count))
print(p)



#### Trend over time ####
purpyear <- ddply(hos, .(year), nrow)
plotnames <- paste0(purpyear$year, ' \n ', 'n=', purpyear$V1)

with(hos, plot(factor(year), Count, xlab='Year', ylab='Count'))



##### Tide analyses #####

# load packages
require(Tides)

# load data
# datasets are loaded and processed in tides_max_min.R

pmth_extma$time <- as.POSIXct(pmth_extma$time, format='%Y-%m-%d %H:%M:%S')
pmth_extma$date <- format(pmth_extma$time, format='%Y-%m-%d') %>% as.POSIXct(format='%Y-%m-%d')
pmth_extma$only_time <- format(pmth_extma$time, format='%T') %>% as.POSIXct(format='%T')
pmth_extma$datetide <- paste(pmth_extma$date, pmth_extma$HL)
y <- subset(pmth_extma, only_time > as.POSIXct('06:00:00', format='%T') & only_time < as.POSIXct('19:00:00', format='%T'))

# subset HOS data - only those rows we can use
hostide <- subset(hos, !(is.na(Time) & is.na(Tide)))

#### Tide height ####


# get dates and tides for compatibility with pmth_extma high/low tide dataset
hostide$datetide <- paste(hostide$Date, hostide$Tide)





# add the matched data to hostide
sum(is.na(hostide$datetime))

sel <- which(is.na(hostide$Time))
x <- match(hostide$datetide[sel], y$datetide)
sel <- which(is.na(hostide$Time))[which(!is.na(x))]
hostide[sel,]$datetime <- y$time[na.omit(x)]



# find closest tide height measurement
# using the nearest() function defined in find_nearest.R

hostide$chm <- NA
for (ii in 1:nrow(hostide)) {
  hostide$chm[ii] <- nearest(
    as.numeric(hostide$datetime[ii]), as.numeric(pmouth$time))
}
hostide$chm[which(hostide$datetime>max(pmth_extma$time))] <- NA # remove those with no accurate tide data
hostide$Height <- pmouth$h[hostide$chm]





#### Tide state ####

sum(is.na(hostide$Tide))

# find closest tide extremum
hostide$cte <- NA
for (ii in 1:nrow(hostide)) {
  hostide$cte[ii] <- nearest(
    as.numeric(hostide$datetime[ii]), as.numeric(pmth_extma$time))
}
hostide$cte[which(hostide$datetime>max(pmth_extma$time))] <- NA # remove those with no accurate tide data
hostide$cte_gap <- abs( hostide$datetime - pmth_extma$time[hostide$cte] )

# number of hours either side of extremum
hours_window <- 2.5

hostide$inwindow <- hostide$cte_gap <= (hours_window*60*60)

sel <- (is.na(hostide$Tide) & hostide$inwindow)
hostide$Tide[which(sel)] <- pmth_extma$HL[which(sel)]


#### Counts by tide state####
with(hostide, plot(Tide, Count))
with(hostide, wilcox.test(Count~Tide))
with(hostide, plot(Height, Count))
with(hostide, cor.test(Height, Count))

#### Behaviour ####

str(hostide)

# Difference in % feeding at high vs low tide?
wilcox.test(FeedPC ~ Tide, data = hostide) # yes - more feeding at low tide
with(hostide, plot(Tide, FeedPC))

# Difference in % feeding according to tide height (rather than state)?
glm.fpc.ht <- glm((FeedPC/100) ~ Height, weights=Count, data=hostide, family = binomial(link='logit'))
summary(glm.fpc.ht) # yes - less feeding activity as the tide rises
with(hostide, plot(Height, FeedPC))

# Difference in habitat use according to tide height?
rockwall <- subset(hostide, Habitat %in% factor(c('Rock', 'Sea wall'))) %>% droplevels 
wilcox.test(Height ~ Habitat, data=rockwall) # not proven
with(hostide, plot(Habitat, Height, ylab='Tide height (m)'))



#### Fancy plotting ####

require(ggplot2)

# predict data
preddat <- predict(glm.fpc.ht, se.fit = T)

# add data to predictions
plotdat <- data.frame(
  FeedPC = glm.fpc.ht$y,
  Height = hostide$Height[-(glm.fpc.ht$na.action)],
  Count = hostide$Count[-(glm.fpc.ht$na.action)],
  fitted = exp(preddat$fit)/(1+(exp(preddat$fit))),
  ul = exp(preddat$fit + (1.96*preddat$se.fit))/(1+exp(preddat$fit + (1.96*preddat$se.fit))),
  ll = exp(preddat$fit - (1.96*preddat$se.fit))/(1+exp(preddat$fit - (1.96*preddat$se.fit)))
)

p <- ggplot(plotdat, aes(x=Height))
p <- p + geom_ribbon(aes(ymin=ll, ymax=ul), fill='pink')
p <- p + geom_line(aes(y=fitted), linetype='dashed')
p <- p + geom_point(aes(y=FeedPC, size=Count), shape=21) 
p <- p + theme_classic() + xlab('Tide height (m)') + ylab('Proportion feeding') 
p <- p + ylim(0, 1) + theme(legend.position = c(.1, .25))
print(p)


#### Time of day ####

with(hostide, plot(Time, Count))
with(hostide, cor.test(as.numeric(Time), Count, method='pe'))
  with(hostide, scatter.smooth(Time, Count, add=T))
  