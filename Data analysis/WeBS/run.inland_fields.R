### look at use of inland fields & its influence on difference between 
### high and low tide totdals

require(plyr)
require(magrittr)
require(MuMIn)

fields <- read.csv('../terrestrial_habitats.csv')

fsp <- join(ass_spp, fields)



fsp$male_mass<-NULL; fsp$female_mass<-NULL
fsp %<>% na.omit
lm.full <- lm(log1p(HTC) ~ log1p(LTC) + fields*Site + Mass, data=fsp, na.action=na.fail)

#opts <- options()
#options(na.action = 'na.fail')
dr.dre <- dredge(lm.full)
head(dr.dre)

lm.top <- lm(log1p(HTC) ~ log1p(LTC) + fields + Site, data=fsp)
summary(lm.top)  

lm.2nd <- lm(log(HTC) ~ log(LTC) + Site, data=fsp)
summary(lm.2nd)

lm.3rd <- lm(log1p(HTC) ~ log1p(LTC) + fields + Site + Mass, data=fsp)
summary(lm.3rd)  # nonsig



#### plotting ####

require(ggplot2)
require(gridExtra)

p <- ggplot(fsp, aes(x=log1p(LTC), y=log1p(HTC)))
p <- p + geom_abline(aes(slope=1, intercept=0), colour='grey', linetype='dashed')
p <- p + geom_smooth(method='lm', formula=y~x)
p <- p + geom_point(aes(color=fields))
p <- p + facet_wrap(~fields)
p <- p + theme_classic()
print(p)



fsp$abdif <- fsp$HTC / (fsp$HTC + fsp$LTC) 
# with(fsp, plot(abdif~fields))


p <- ggplot(fsp, aes(x=fields, y=abdif))
p <- p + geom_hline(aes(yintercept=0.5), color='grey', linetype='dashed')
p <- p + geom_boxplot(outlier.color='white', outlier.fill='white') + geom_point(aes(size=LTC), shape=21)
p <- p + facet_wrap(~Site)
p <- p + theme_classic() + theme(legend.position=c(.85, .1))
print(p)

# q <- ggplot(fsp, aes(x=fields, y=abdif))
# q <- q + geom_hline(aes(yintercept=0.5))
# q <- q + geom_boxplot() + geom_point(aes(size=LTC), shape=21)
# q <- q + facet_wrap(~Species)
# q <- q + theme_classic()
# print(q)



lm.full <- lm(abdif ~ + fields*Site + Mass + Species, data=fsp, na.action=na.fail)

#opts <- options()
#options(na.action = 'na.fail')
dr.dre <- dredge(lm.full)
head(dr.dre)

lm.top <- lm(abdif ~ fields + Site, data=fsp)
summary(lm.top)  

lm.2nd <- lm(abdif ~ Site, data=fsp)
summary(lm.2nd)

lm.3rd <- lm(abdif ~  fields * Site, data=fsp)
summary(lm.3rd)  # nonsig



fsp.pred <- predict(lm(abdif~fields, data=fsp), se.fit=T)
fsp$phat <- fsp.pred$fit
fsp$ul <- fsp.pred$fit + 1.97*fsp.pred$se.fit
fsp$ll <- fsp.pred$fit - 1.97*fsp.pred$se.fit

#### assemblage 
ass_field$Label <- rep(c('BE', 'BH', 'CH', 'LH', 'NH', 'NS', 'PH', 'SW'), each=2) %>% factor
levels(ass_field$fields) <- c('Offshore only', 'Inland fields')
ass_field$fields %<>% relevel(ref='Inland fields')
p.ass_field <- ggplot(ass_field, aes(x=LTC, y=HTC, label=Label)) + 
  geom_abline(slope = 1, intercept = 0, linetype=2, color='grey') +
  geom_smooth(method='lm', fill='lightblue') + geom_point(color='green') +
  geom_text() + theme_classic() + xlab('No. birds low tide') + ylab('No. birds high tide') +
  facet_wrap(~fields, scales='free')
print(p.ass_field)
summary(lm(HTC~LTC, data=ass))
summary(lm(HTC~LTC, data=subset(ass_field, fields=='Inland fields')))
summary(lm(HTC~LTC, data=subset(ass_field, fields=='Offshore only')))


sp_tots <- ddply(ltc, .(Species), function(x) sum(x$Peak.count))
sp_tots <- sp_tots[order(sp_tots$V1, decreasing = T),]
head(sp_tots)


### mean abdif by species
ass_spp$abdif <- ass_spp$HTC / (ass_spp$HTC + ass_spp$LTC)
spabdif <- ddply(ass_spp, .(Species), function(x) {mean(x$abdif, na.rm=T)})
spabdif <- spabdif[order(spabdif$V1),]
names(spabdif)[2] <- 'abdif'
spabdif

spdata <- join(sp_tots, spabdif)
spdata %<>% na.omit
spdata %<>% join(fields)

btocode <- read.csv('../BTO_codes.csv')
names(btocode)[1] <- 'Label'
spdata %<>% join(btocode)

spdata$Species %<>% droplevels
levels(spdata$Species) <- spdata$Species

pp <- ggplot(spdata, aes(x=V1, y=abdif, fill=fields, label=Label)) + 
  geom_hline(aes(yintercept=0.5), color='grey', linetype='dashed') +
  # geom_smooth(aes(x=V1, y=abdif), method = 'lm', formula=y~x, se=T) +
  geom_label(color='black', label.padding = unit(0.05, 'lines')) + 
  scale_x_log10() + theme_classic() + theme(legend.position = 'none') +
  xlab('Low tide total (log)') + ylab('Proportion at high tide')
# plot(pp)

qq <- ggplot(spdata, aes(x=fields, y=abdif, fill=fields, label=Label)) + 
  geom_hline(aes(yintercept=0.5), color='grey', linetype='dashed') +
  # geom_label(color='black', label.padding = unit(0.05, 'lines')) +
  geom_point() +
  geom_boxplot(alpha=.67) + theme_classic() +
  theme(legend.position = 'none') +
  xlab('Uses inland fields?') + ylab('Proportion recorded at high tide')
plot(qq)

l# grid.arrange(pp, qq, nrow=1)
