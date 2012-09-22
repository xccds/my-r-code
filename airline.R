# 中国机场数据可视化的代码
# http://xccds1977.blogspot.com/2012/07/blog-post_26.html
library(ggmap)
data.port <- read.csv('d:\\airports.dat',F)
data.line <- read.csv('d:\\routes.dat',F)
library(stringr)
# 找到中国的机场
portinchina <- str_detect(data.port[,'V4'], "China")
chinaport <- data.port[portinchina,]
# 去除少数几个没有编号的机场
chinaport <-chinaport[chinaport$V5!='',
                      c('V3','V5','V7','V8','V9')]
names(chinaport) <- c('city','code','lan','lon','att')

# 找出国内航班
lineinchina <- (data.line[,'V3'] %in% chinaport$code) & (data.line[,'V5'] %in% chinaport$code)
chinaline <- data.line[lineinchina,c('V3','V5','V9')]
names(chinaline) <- c('source','destination','equipment')

#构建一个函数，根据机场编码得到经纬度
findposition <- function(code) {
    find <- chinaport$code==code
    x <- chinaport[find,'lon']
    y <- chinaport[find,'lan']
    return(data.frame(x,y))
}

# 将机场代号转为经纬度
from <- lapply(as.character(chinaline$source),findposition)
from <- do.call('rbind',from)
from$group <- 1:dim(from)[1]
names(from) <- c('lon','lan','group')

to <- lapply(as.character(chinaline$destination),findposition)
to <- do.call('rbind',to)
to$group <-1:dim(to)[1]
names(to) <-c('lon','lan','group')
data.line <- rbind(from,to)
temp<- data.line[data.line$group<100,]
# 用ggmap包从google读取地图数据，并将之前的数据标注在地图上。
ggmap(get_googlemap(center = 'china', zoom=4,
                    maptype='roadmap'),extent='device')+
    geom_point(data=chinaport,aes(x=lon,y=lan),
               colour = 'red4',alpha=0.8)+
    geom_line(data=data.line,aes(x=lon,y=lan,group=group),
              size=0.1,alpha=0.05,color='red4')

# 哪个航班最远、最近，哪条航线最繁忙，飞机档次，哪个机场最高，哪个机场最忙

table(factor(chinaline$equipment))

