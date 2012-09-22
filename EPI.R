# 环境数据的比较
# http://xccds1977.blogspot.com/2012/08/blog-post.html

# 读取数据
raw <- read.csv('http://www.stat.yale.edu/~jay/EPI_data_download/EPI_2012_Final_Results.csv',T)
names(raw)
data <- raw[,c(2,7,8,10,23:44)]
data <- data[!is.na(data$GDPgroup),]
library(reshape)
# 数据整理
data.melt <-  melt(data,id=c('Country','GDPCAP','GDPgroup'))
data.melt.china  <- data.melt[data.melt$Country=='China',]
data.melt.china$variable <- with(data.melt.china,
                                 reorder(variable,value,function(x) x))
data.melt$variable <- ordered(data.melt$variable,
                         levels=levels(data.melt.china$variable))

library(ggplot2)
p <- ggplot(data=data.melt,aes(x=variable,y=value))
p + geom_violin(width=1.5,fill='skyblue',color='skyblue')+
    geom_point(data=data.melt.china,
             aes(x=variable,y=value),color='red',size=3)+
    coord_flip()

# 平行坐标图
p + geom_line(aes(group=Country,color=GDPgroup),
             alpha = 0.3,position='jitter')+
    coord_flip()+facet_grid(.~ GDPgroup)+
    opts(legend.position = "none")

# 研究人均GDP和EPI之间关系
d <- ggplot(data=data,aes(x=GDPCAP,y=EPI))
d + geom_point(aes(color=GDPgroup)) +
  scale_x_continuous(limit=c(0,80000))+
  geom_point(x=data[data$Country=='China',]$GDPCAP,
             y=data[data$Country=='China',]$EPI,
             color='red4',size=3) +
  geom_text(x=data[data$Country=='China',]$GDPCAP,
            y=data[data$Country=='China',]$EPI,
            label='CHINA',hjust=-0.2, vjust=1.2)+
  opts(legend.position = "none")
