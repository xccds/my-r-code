# titanic 数据分析
# http://xccds1977.blogspot.com/2012/04/blog-post_07.html

# 获取数据
url <- 'http://stats.math.uni-augsburg.de/Mondrian/Data/Titanic.txt'
data <- read.table(url,T)
summary(data)
# 绘制条形图
library(ggplot2)
p <- ggplot(data,aes(x=Class,fill=Survived))
p + geom_bar(position='stack')+ coord_flip() + facet_wrap(~Sex) 
# 绘制mosaic图
library(vcd)
mosaic(Survived~ Class+Sex, data = data,shade=T, highlighting_direction = "right")
# 建立存活率表
fx <- function(x) length(x[x=='Yes'])
table1 <- data.frame(with(data,aggregate(x=Survived,by=list(Class,Sex,Age),FUN=length)))
table2 <- data.frame(with(data,aggregate(x=Survived,by=list(Class,Sex,Age),FUN=fx)))
table1$y <- table2$x
table1$survived <- round(table1$y /table1$x,digits=2)
table1[order(table1$survived,decreasing=T),]
# 建立决策树模型
library(rpart)
formula <- Survived~ Class+Sex+Age
fit <- rpart(formula,data)
library(maptree)
draw.tree(fit)