#### function to calculate various diversity measures of the bird data
require(vegan) # for diversity()


# extract just the variables related to species' abundances
# and therefore only those that pass through the tide filter
solentspp <- subset(solent_jdata, filter_tide,
                    select=c('Brent.Goose..Dark.bellied.', waderspp))


## Diversity metrics per site
soldiv <- data.frame(
  Visit.ID = solent_wader$Visit.ID,
  #Area = solent_wader$Area_ha,
  Indivs = rowSums(solentspp),
  NoSpp  = rowSums(solentspp > 0),
  Simpson = diversity(solentspp, 'simpson')
)
head(soldiv, 20)
summary(soldiv)

# only include surveys with at least one species observed
soldivps <- subset(soldiv, Indivs > 0 )#& Area > 0)
summary(soldivps)

# rm(solentspp, soldiv, soldivps)