# asterix sites results summaries

require(rgdal)
require(magrittr)
require(plyr)

# load shapefile of results
ast <- readOGR(dsn='./Final results layer/',
               layer='Asterisk_results_final')

# table of overall categories
table(ast$Overall)
# as percentage of sites
(table(ast$Overall)/nrow(ast)*100) %>% round(1)

# export CSV file of site data
ast@data[order(ast$Overall, ast$Site_name),] %>%
  write.csv(file='Final results layer/Asterisk_table.csv', row.names = F)
