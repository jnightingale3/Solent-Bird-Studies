### Summarise data by Site and compare numbers of birds at high and low tide

require(plyr)
require(magrittr)

##### read in data

## raw data - not summaries prepared by WeBS team
rawd <- read.csv('Data_WeBS_LTC/ltc1raw.csv')
rawd <- subset(rawd, Species %in% ourbirds)

## fix count data - original used . to indicate 0 :(
fix_num <- function(x) {
  x <- as.character(x) # convert from factor to character string
  x[x=='.'] <- 0       # change '.' to '0' (would be invalid factor level)
  x <- as.numeric(x)   # convert to a number
  return(x)
}
rawd$Nov%<>% fix_num; rawd$Dec%<>% fix_num; rawd$Jan%<>% fix_num; rawd$Feb%<>% fix_num
rawd$Winter.maximum %<>% fix_num
rm(fix_num)

##### calcualate mean and peak for each Site (SPA)

## mean
rawd_mean <- ddply(rawd, .(Species, Site), function(x) {
  .tot <- with(x, mean(Nov, Dec, Jan, Feb))
})

## Whole-Solent mean low-tide total
solent_ltc_means <- ddply(rawd_mean, .(Species), function(x) sum(x$V1))

# join with Peak from core counts
join_webs_means <- join(solent_ltc_means, webs_totals)
head(join_webs_means)
with(join_webs_means, plot(log1p(Peak), log1p(V1)))
abline(0,1)
# conclusion - Peaks from WeBS are always higher than means from core counts...


## peak
rawd_peak_month <- ddply(rawd, .(Species), function(x) {
  nov <- sum(x$Nov)
  dec <- sum(x$Dec)
  jan <- sum(x$Jan)
  feb <- sum(x$Feb)
  
  return(which.max(c(nov, dec, jan, feb)))
})
names(rawd_peak_month)[2] <- 'Peak.Month'
rawd <- join(rawd, rawd_peak_month)

rawd_totpk <- ddply(rawd, .(Species, Site), function(x) {
  sum(x[,(x$Peak.Month + 1)])
})

solent_ltc_peaks <- ddply(rawd_totpk, .(Species), function(x) sum(x$V1)) #%>%
  # subset(V1 > 5) # exclude very rare species
plot(solent_ltc_means$V1, solent_ltc_peaks$V1)
abline(0,1)


# join with Peak from core counts
join_webs_peaks <- join(solent_ltc_peaks, webs_totals)
names(join_webs_peaks)[2] <- 'LTC.Peak'
head(join_webs_peaks)
with(join_webs_peaks, plot(log1p(Peak), log1p(V1)))
abline(0,1)

with(join_webs_peaks, wilcox.test(x=Peak, y=V1, paired=T))
with(join_webs_peaks, hist(Peak-V1))


# total assemblage
site_tots_ltc <- ddply(rawd_totpk, .(Site), function(x) {sum(x$V1)})
site_tots_ltc[order(site_tots_ltc$V1, decreasing = T),]
