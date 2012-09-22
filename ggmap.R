# 用ggmap来展示地震数据
# http://xccds1977.blogspot.com/2012/06/ggmap.html

# 加载扩展包
library(ggmap)
library(animation)
library(XML)
# 从网页上抓取数据，并进行清理
webpage <-'http://data.earthquake.cn/datashare/globeEarthquake_csn.html'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
raw <- tables[[6]]
data <- raw[-1,c('V1','V3','V4')]
names(data) <- c('date','lan','lon')
data$lan <- as.numeric(data$lan)
data$lon <- as.numeric(data$lon)
data$date <- as.Date(data$date,  "%Y-%m-%d")
# 用ggmap包从google读取地图数据，并将之前的数据标注在地图上。
ggmap(get_googlemap(center = 'china', zoom=4,maptype='terrain'),extent='device',legend = 'bottomright')+
geom_point(data=data,aes(x=lon,y=lan),colour = 'red',alpha=0.7)+
stat_density2d(aes(x=lon,y=lan,fill=..level..,alpha=..level..),
                   size=2,bins=4,data=data,geom='polygon')
                     

# 为了生成动画，先准备好一个绘图函数
plotfunc <- function(x) {
    df <- subset(data,date <= x)
    df$lan <- as.numeric(df$lan)
    df$lon <- as.numeric(df$lon)
    p <- ggmap(get_googlemap(center = 'china', zoom=4,maptype='terrain'),,extent='device')+
        geom_point(data=df,aes(x=lon,y=lan),colour = 'red',alpha=0.7)
}
# 获取地震的日期
time <- sort(unique(data$date))
# 生成并保存动画
saveMovie(for( i in time) print(plotfunc(i)))


# 从地图上标注两个点然后得到之间的地图距离
qmap(location = 'beijing,china', zoom=10,maptype='roadmap',extent='device')
click <- gglocator(1)
qmap(location = as.numeric(click), zoom=10,maptype='roadmap',extent='device')
click <- gglocator(2)
from <- as.numeric(click[1,])
to <- as.numeric(click[2,])
mapdist(from,to,mode='walking')


