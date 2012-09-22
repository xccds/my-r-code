# 财富500强数据的分析
# http://xccds1977.blogspot.com/2012/07/500.html

mydata <- read.csv('d:\\ft500.csv',T,stringsAsFactors = F)
data  <- transform(mydata,
                   Country = factor(Country),
                   Sector = factor(Sector),
                   value = as.numeric(gsub(',','',value)),
                   netincome = as.numeric(gsub(',','',netincome)),
                   employees = as.numeric(gsub(',','',employees)),
                   totalasset = as.numeric(gsub(',','',totalasset)),
                   turnover = as.numeric(gsub(',','',turnover)),
                   price = as.numeric(gsub(',','',price))
                   )
library(ggplot2)
# 哪个行业的上榜公司最多
data$Sector <- with(data,reorder(Sector,Sector,function(x) length(x)))
p <- ggplot(data=data)
p + geom_bar(aes(x=Sector,y=..count..),fill='deepskyblue') + coord_flip()
# 哪个国家的上榜公司最多？
data$Country <- with(data,reorder(Country,Country,function(x) length(x)))
p + geom_bar(aes(x=Country,y=..count..),fill='deepskyblue') + coord_flip()

# 哪个行业最赚钱
isna <- is.na(data$employees) |is.na(data$netincome) 
data2 <- data[!isna,]
data2$Sector <- with(data2,reorder(Sector,netincome,median))
p2 <- ggplot(data=data2,aes(x=Sector,y=netincome))
p2 + stat_summary(fun.y=median,geom='bar',fill='deepskyblue') + coord_flip()

# 排名变化情况
data$X2011[is.na(data$X2011)] <- 501
data$change <- with(data,X2011-X2012)
p + geom_histogram(aes(x=change,y=..density..),
                   fill='deepskyblue',color='white')
# 进步前十名
data[order(data$change,decreasing=T),][1:10,]
# 退步前十名
data[order(data$change,decreasing=F),][1:10,]

# 行业的进退
data$Sector <- with(data,reorder(Sector,change,median))
p3 <- ggplot(data=data,aes(x=Sector,y=change))
p3 + stat_summary(fun.y=median,geom='bar',fill='deepskyblue') + coord_flip()

# 国家的进退
data$Country <- with(data,reorder(Country,change,median))
p4 <- ggplot(data=data,aes(x=Country,y=change))
p4 + stat_summary(fun.y=median,geom='bar',fill='deepskyblue') + coord_flip()

# 各变量之间的相关性
library(corrgram)
data3 <- data[,c(6:13,15)]
corrgram(order = T, data3, lower.panel = panel.shade, upper.panel = panel.pie)
