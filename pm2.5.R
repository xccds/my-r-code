# 北上广三地空气数据的分析
# http://xccds1977.blogspot.com/2012/06/blog-post_07.html

library(twitteR)
library(plyr)
library(ggplot2)

# 抓取北京和上海空气数据的推文
airb <- userTimeline("beijingair", n=660)
airs <- userTimeline("CGShanghaiAir", n=660)
airg <- userTimeline("Guangzhou_Air", n=660)

#提取文本后用正则表达式分割
pattern  <- '; | \\(|\\)'
extract <- function(x) {
    strsplit(x$text,pattern)
}
textb <- sapply(airb,extract)
texts <- sapply(airs,extract)
textg <- sapply(airg,extract)

#转成数据框格式
datab <-ldply(textb,.fun=function(x) as.data.frame(t(x),stringsAsFactors=F))
datab <- datab[,c(1,4,5)]
datas <-ldply(texts,.fun=function(x) as.data.frame(t(x),stringsAsFactors=F))
datas <- datas[,c(1,4,5)]
datag <-ldply(textg,.fun=function(x) as.data.frame(t(x),stringsAsFactors=F))
datag <- datag[,c(1,4,5)]

# 合并数据，并转换AQI为数值
data <- rbind(datab,datas,datag)
names(data) <- c('time','AQI','type')
data$AQI <- as.numeric(data$AQI)

# 加入城市变量
city <- factor(rep(1:3,each=660),labels = c('北京','上海','广州'))
data$city <- city

# 加入星期变量
time.date <-as.Date(data$time,"%m-%d-%Y")
data$week = as.POSIXlt(time.date)$wday

# 加入钟点变量
data$clock <- factor(substr(data$time,start=12,stop=13))

# 去除异常值
data <- subset(data,AQI<800)

# 小提琴图观察不同城市的空气质量
p1 <- ggplot(data,aes(city,AQI,fill=city))
p1 + geom_violin(alpha=0.3,width=0.3) +
    geom_jitter(alpha=0.3,shape=21)

# 观察一周的不同日子空气质量中位数
# aggregate(data$AQI,list(data$week),mean,na.rm=T)
p3 <- ggplot(data,aes(factor(week),AQI,colour=city,group=city))
p3 +stat_summary(fun.y = median, geom='line',size=1.2)+
    stat_summary(fun.y = median, geom='point',size=3)+
    coord_cartesian(xlim=c(1,7))

# 观察不同时点空气质量中位数
p2 <- ggplot(data,aes(clock,AQI,colour=city,group=city))
p2 +stat_summary(fun.y = median, geom='line',size=1.2) +
    stat_summary(fun.y = median, geom='point',size=3)+
    coord_cartesian(xlim=c(3,26))