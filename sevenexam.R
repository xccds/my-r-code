# 七周七语言系列活动之三 “R语言” 的课后作业练习
# http://topgeek.org/?p=546

# 读入数据
data <- read.csv('cellphone.csv',T,stringsAsFactors =F)
names(data)

attach(data)
library(stringr)
library(plyr)

# 将位置信息分割，取出省份信息
place <- str_split(Place,' ')
place <- laply(place,function(x) x[1])
place <- str_replace(place, "货到付款", "")

# 取出手机单价，未处理运费和信用卡信息
price <- str_split(Price,' ')
price <- laply(price,function(x) x[x!=''][1])
price <- as.numeric(price)

# 为了练习目的，取出月销售和月评价
sale <- str_extract_all(Sale, "[0-9]+")
sale <- do.call('rbind',sale)
sale <- data.frame(sale)

# 取出消费者保障等逻辑值
logic.xb <- str_detect(Legend, "消费者保障")
logic.qt <- str_detect(Legend, "七天退换")
logic.zp <-str_detect(Legend, "正品保障")

# 组成新的数据框
data2 <- data.frame(logic.xb,logic.qt,logic.zp,MSales,
              Comments, price,place,Seller)

sapply(data2,class)
summary(data2)

library(ggplot2)
# 哪个省份消费最多，分布如何
p <- ggplot(data2,aes(place))

temp <- table(data2$place)
p + geom_bar(fill='lightblue',color='black') +
    scale_x_discrete(limits = levels(data2$place)[order(temp)])+
    coord_flip()

# 价格的直方图
p <- ggplot(data2,aes(price))
p + geom_histogram()

# 月销售量和价格之间的对数散点图
p <- ggplot(data2,aes(price,MSales))
p + geom_point()+ scale_x_log10()+ scale_y_log10()
# 月销售量和信用评价之间的对数散点图
p <- ggplot(data2,aes(MSales,Comments))
p + geom_point()+ scale_x_log10()+ scale_y_log10()

# 有七天或正保证品的商品会销售的更好吗？是的
(temp <-ddply(data2,logic.qt~logic.zp,function(x) mean(x$MSales)))

data2$qt.zp <- factor(with(data2,logic.qt+logic.zp),
                      labels=c('noqt','qt.nozp','zp'))
p <- ggplot(data2,aes(x=qt.zp,y=MSales,fill=qt.zp))
p + geom_boxplot() +
        scale_y_log10()

# 单价会更贵吗？
(temp <-ddply(data2,logic.qt~logic.zp,function(x) mean(x$price)))

(temp <-ddply(data2,logic.qt~logic.zp,function(x) mean(x$Comments)))

# 回归 
fomular <- MSales ~ price + logic.qt + logic.zp 
model <- lm(fomular,data=data2)
summary(model)

# 先降维，后聚类
mds <- cmdscale(dist(data2[,c('MSales','Comments','price')]),
             k=2,eig=T)
x <- mds$points[,1]
y <- mds$points[,2]

after.data <- data.frame(x,y)

hc <- hclust(dist(after.data))
result <- cutree(hc,k=4)

table(result)
data[result==1,'Summary']
data[result==3,'Summary']
data[result==4,'Summary']


p <- ggplot(after.data,aes(x,y))
p+geom_point(size=3,alpha=0.8,
             aes(colour=factor(result)))