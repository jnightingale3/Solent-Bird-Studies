# scrape weather data from chimet / cambermet websites

dates <- seq.Date(from=as.Date('2009-01-01'), 
                  to=as.Date('2017-03-31'),
                  by=1)

dates_filter <- subset(dates, months(dates) %in% c('January', 'February', 'March',
                                                   'October', 'November', 'December'))

URLs <- 
  # head( # uncomment this line to test the code
    paste0(
      #'http://www.cambermet.co.uk/archive/',
      'http://www.chimet.co.uk/archive/', #2009/January/CSV/Chi01Jan2009.csv
      format(dates_filter, '%Y'),
      '/', format(dates_filter, '%B'), '/CSV/Chi',
      format(dates_filter, '%d%b%Y'),
      '.csv'
    )
    # , 20) # uncomment this line to test the code

URLs <- subset(URLs, substr(URLs, start=53, stop=68) %in% list.files(path = 'Chi/')
              == F)

ii <- 1
for (ii in 1:length(URLs)) {
  print( 
    paste0('File ', ii,  ' / ', length(URLs), ' (', ii/length(URLs) * 100, '% complete)')
    )
  tryCatch({
    closeAllConnections()
    
    dat <-  read.csv(URLs[ii]) 
    write.csv(dat, file=paste0('Chi/', substr(URLs[ii], start=53, stop=68)) )
    
    Sys.sleep(5)
  }, finally=next)
  
}
rm(dat)
