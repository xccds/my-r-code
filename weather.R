getweather <- function() {
  library(RCurl)
  library(RJSONIO)
  library(XML)
  city <- as.character(readline('please input the city name：'))
  requestUrl<-paste("http://maps.googleapis.com/maps/api/geocode/xml?address=",city,"&sensor=false", sep="")
  xmlResult<-xmlTreeParse(requestUrl,isURL=TRUE)
  root <- xmlRoot(xmlResult)
  lat <-xmlValue(root[['result']][['geometry']][['location']][['lat']])
  lon <-xmlValue(root[['result']][['geometry']][['location']][['lng']])
  url <- 'http://api.wunderground.com/api/a98d04ac43156c84/conditions/forecast/lang:CN/q/'
  finalurl <- paste(url,as.character(lat),',',as.character(lon),'.json',sep='')
  web <- getURL(finalurl)
  raw <-fromJSON(web)
  high <- raw$forecast$simpleforecast$forecastday[[2]]$high['celsius']
  low <- raw$forecast$simpleforecast$forecastday[[2]]$low['celsius']
  condition <- raw$forecast$simpleforecast$forecastday[[2]]$conditions
  currenttemp <- raw$current_observation$temp_c
  currentweather <- raw$current_observation$weather
  result <-list(current=paste(currenttemp,'°C ',currentweather,sep=''),tomorrow=paste(high,'°C','-',low,'°C ',condition,sep=''))
  names(result) <-c('当前', '明天')
  return(result)
}
