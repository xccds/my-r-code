# 回答一个问题画图的问题
# http://xccds1977.blogspot.com/2012/06/ggplotjpeg.html

# 读入数据和加载包
data <-read.csv('d:\\data.csv',T)
library(ggplot2)
library(ellipse)
library(gridExtra)
library(plyr)

#建立一个函数以生成置信椭圆
generatfun <- function(x) {
    as.data.frame(with(data,ellipse(cor(a,b),scale=c(sd(a),sd(b)),
             level=x,centre=c(mean(a),mean(b)))))}
# 根据不同的置信度来生成多个数据框并整合
i <-  seq(0.1,0.9,by=0.1)
data2 <- adply(i,1,generatfun)
names(data2) <- c('level','x','y')

# 绘制主图散点图，并将图例去除，这里point层和path层使用了不同的数据集
scatter <- ggplot() + 
    geom_point(data=data,aes(a,b,shape=type))+
    geom_path(data=data2,aes(x,y,group=level))+
    opts(legend.position = "none")+
    geom_vline(xintercept = mean(data$a),linetype=2)+
    geom_hline(yintercept = mean(data$b),linetype=2)
# 绘制上边的直方图，并将各种标注去除
hist_top <- ggplot()+geom_histogram(aes(data$a),colour='black',fill='gray',binwidth = 0.3)+
    opts(panel.background=theme_blank(),
         axis.title.x=theme_blank(), 
         axis.title.y=theme_blank(),
         axis.text.x=theme_blank(),
         axis.text.y=theme_blank(),
         axis.ticks=theme_blank())
# 同样绘制右边的直方图
hist_right <- ggplot()+geom_histogram(aes(data$b),colour='black',fill='gray',binwidth = 0.1)+
    opts(panel.background=theme_blank(),
         axis.title.x=theme_blank(), 
         axis.title.y=theme_blank(),
         axis.text.x=theme_blank(),
         axis.text.y=theme_blank(),
         axis.ticks=theme_blank())+
         coord_flip()

empty <- ggplot(data.frame(x=1:10,y=1:10),aes(x,y))+
    geom_point(colour='white')+
    opts(panel.background=theme_blank(),
         axis.title.x=theme_blank(), 
         axis.title.y=theme_blank(),
         axis.text.x=theme_blank(),
         axis.text.y=theme_blank(),
         axis.ticks=theme_blank())
# 最终的组合
grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4,1), heights=c(1,4))
