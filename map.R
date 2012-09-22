# 用openstreetmap包来画地图
# http://xccds1977.blogspot.com/2012/04/openstreetmap.html

library(XML)
library(OpenStreetMap)
library(ggplot2)
# 从wiki获取中国前十大城市人口数据
url <-'http://zh.wikipedia.org/wiki/%E4%B8%AD%E5%9B%BD%E5%9F%8E%E5%B8%82%E5%88%97%E8%A1%A8'
tables <- readHTMLTable(url)
raw <- tables[[2]]
data <- raw[1:10,c(2,4,5)]
names(data) <- c('cname','name','pop')
data$name <- as.character(data$name)
# 将人口数从字符型转为数值型
data$pop <- as.numeric(gsub(',','',data$pop))

# 建立函数，输入参数为城市名称，返回经纬坐标
# 先利用google API得到xml形式结果，再用XML包函数提取经纬度
geoencode <- function(name) {
  requestUrl<-paste(
   "http://maps.googleapis.com/maps/api/geocode/xml?address=",
   name,"&sensor=false", sep="")
  xmlResult<-xmlTreeParse(requestUrl,isURL=TRUE)
  root <- xmlRoot(xmlResult)
  latitude <-xmlValue(root[['result']][['geometry']][['location']][['lat']])
  longitude <-xmlValue(root[['result']][['geometry']][['location']][['lng']])
  return(list(latitude=latitude,longitude=longitude))
}
# 得到十个城市的经纬度，结果为矩阵形式，整合为数据框
tempdata <- sapply(data$name,geoencode)
data <- cbind(data,data.frame(t(tempdata)))
data$latitude <-as.numeric(unlist(data$latitude))
data$longitude <-as.numeric(unlist(data$longitude))
# 将经纬度转为open street map绘图所需的输入参数
data <- cbind(data,projectMercator(data$latitude,data$longitude))
# 获取从东经100到125，北纬21到41之间的地图资料
map <- openmap(c(41,100),c(21,125),zoom=5,type = "osm")
# 利用ggplot2绘图包增加城市散点，点的大小表示人口
autoplot(map)+geom_point(data=data,aes(x=x,y=y
  ,size=pop),shape=16,colour='gold',alpha = 0.6)+
  geom_point(data=data,aes(x=x,y=y
  ,size=pop),shape=1,colour='red4')+
  scale_size_continuous(range= c(8,16))+
  opts(axis.line=theme_blank(),axis.text.x=theme_blank(),
       axis.text.y=theme_blank(),axis.ticks=theme_blank(),
       axis.title.x=theme_blank(),
       axis.title.y=theme_blank()) +
  geom_text(data=data,colour='black',
            aes(x=x,y=y,label=cname),hjust=1.4,vjust=0)
# 保存图片
ggsave("plot1.png", dpi=600)