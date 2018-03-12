gos <- readOGR(dsn='Final results layer', layer='ESCP_results_final')@data %>% 
  subset(grepl('^C', Strat_code))
  
summary(gos$Overall) / nrow(gos)
