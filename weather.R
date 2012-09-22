# 调用天气API的函数
# http://xccds1977.blogspot.com/2012/05/rjsonio.html

# 加载所需扩展包 
library(RCurl)
library(RJSONIO)
library(XML)
# 建立一个根据网址提取天气预报的子函数
fromurl<- function(finalurl) {
  # 先读取网页，再解析JSON数据存在raw中  
  web <- getURL(finalurl)
  raw <-fromJSON(web)
  high <- raw$forecast$simpleforecast$forecastday[[2]]$high['celsius']
  low <- raw$forecast$simpleforecast$forecastday[[2]]$low['celsius']
  condition <- raw$forecast$simpleforecast$forecastday[[2]]$conditions
  currenttemp <- raw$current_observation$temp_c
  currentweather <- raw$current_observation$weather
  city <- as.character(raw$current_observation$display_location['full'])
  result <-list(city=city,current=paste(currenttemp,'°C ',currentweather,sep=''),tomorrow=paste(high,'°C','-',low,'°C ',condition,sep=''))
  names(result) <-c('城市','当前', '明天')
  return(result)
    }
# 提取天气预报的主函数
getweather <- function(city='') {
  # 如果用户输入为空，则根据IP地址来查询
  if (city == '') {
    finalurl <- 'http://api.wunderground.com/api/a98d04ac43156c84/conditions/forecast/lang:CN/q/autoip.json'
   return(fromurl(finalurl))
  # 否则就调用google API，这时需要用XML包来解析数据得到经纬度
    } else {
    requestUrl<-paste("http://maps.googleapis.com/maps/api/geocode/xml?address=",city,"&sensor=false", sep="")
   xmlResult<-xmlTreeParse(requestUrl,isURL=TRUE)
   root <- xmlRoot(xmlResult)
   lat <-xmlValue(root[['result']][['geometry']][['location']][['lat']])
   lon <-xmlValue(root[['result']][['geometry']][['location']][['lng']])
   url <- 'http://api.wunderground.com/api/a98d04ac43156c84/conditions/forecast/lang:CN/q/'
   # 将经纬度与其它信息相结合，形成查询地址
    finalurl <- paste(url,as.character(lat),',',as.character(lon),'.json',sep='')
   return(fromurl(finalurl))
  }
}
    
# http://en.wikipedia.org/wiki/China
# http://en.wikipedia.org/wiki/List_of_cities_in_the_People%27s_Republic_of_China_by_population
# 
# 提取历史数据，获得三个城市一年的数据进行比较
date <- seq.Date(from=as.Date('2011-01-01'), to=as.Date('2011-12-31'), by='1 day')
date.range <- as.character(format(date,"%Y%m%d"))
n <- length(date.range)
temp <- humi <- rep(0,n)
for (i in 1:n) {
  url <- 'http://api.wunderground.com/api/a98d04ac43156c84/'
  finalurl <- paste(url,'history_',date.range[i],'/q/29.5630100,106.5515570.json',sep='')
  web <- getURL(finalurl)
  raw <-fromJSON(web)
  temp[i] <- raw$history$dailysummary[[1]]$meantempm
  humi[i] <- raw$history$dailysummary[[1]]$humidity
  Sys.sleep(7)
}
plot(temp,humi)
install.packages("googleVis")
library(googleVis)
dataset <- data.frame(temp,humi,date,stringsAsFactors=F)
dataset$city <- rep('chongqing',365)
dataset$temp <- as.numeric(dataset$temp)
dataset$humi <- as.numeric(dataset$humi)
write.csv(data.chongqing,'weather-chongqing.csv')
M <- gvisMotionChart(dataset,idvar='city',timevar='date')
plot(M)
season <- rep('season',365)
season[months(dataset$date) %in% c("一月","二月","十二月")]<-'冬季'
season[months(dataset$date) %in% c("三月","四月","五月")]<-'春季'
  season[months(dataset$date) %in% c("六月","七月","八月")]<-'夏季'
  season[months(dataset$date) %in% c("九月","十月","十一月")]<-'秋季'
dataset$season <- as.factor(season)
with(dataset,plot(temp,humi,col=as.numeric(city)))
  # 箱线图 气泡图 一年只有四季？聚类分析， 一个城市的日历热图

dataset <- rbind(data.wuhan,data.nanjing,data.chongqing)

library(ggplot2)
p <- ggplot(dataset,aes(city,temp)) 
  p + geom_boxplot()
  
p <- ggplot(dataset,aes(temp,humi,colour=city))
  p + geom_point()
p <- ggplot(dataset,aes(temp,..count..,fill=city))
  p + geom_histogram(position='fill')
p <- ggplot(dataset,aes(city,temp,fill=city,colour=city))
  p+ geom_violin(alpha=0.3,width=0.5)+
    geom_boxplot(width=0.2,outlier.colour=NA)
  
  library(plyr)
  dataset$city <- as.factor(dataset$city)
  howmany <- function(df) length(df$temp[df$temp>30])
  ddply(dataset,.(city),howmany)
  
  
# 日历热图
require(quantmod)

dat<-data.frame(date=data.wuhan$date,value=data.wuhan$temp)

# show week-of-month versus weekday

# the month too 
dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)
# but turn months into ordered facors to control the appearance/ordering in the presentation
dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
# the day of week is again easily found
dat$weekday = as.POSIXlt(dat$date)$wday
# again turn into factors to control appearance/abbreviation and ordering
# I use the reverse function rev here to order the week top down in the graph
# you can cut it out to reverse week order
dat$weekdayf<-factor(dat$weekday,levels=rev(0:6),labels=rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),ordered=TRUE)
# the monthweek part is a bit trickier 
# first a factor which cuts the data into month chunks
# dat$yearmonth<-as.yearmon(dat$date)
# dat$yearmonthf<-factor(dat$yearmonth)
# then find the "week of year" for each day
dat$week <- as.numeric(format(dat$date,"%W"))
# and now for each monthblock we normalize the week to start at 1 
dat<-ddply(dat,.(monthf),transform,monthweek=1+week-min(week))

# Now for the plot
P<- ggplot(dat, aes(monthweek, weekdayf, fill = value)) + 
    geom_tile(colour = "white") + facet_wrap(~monthf ,nrow=3) +
    scale_fill_gradient(low="red", high="yellow") +
    opts(title = "武汉市2011年气温日历热图") +  xlab("Week of Month") + ylab("")
P
