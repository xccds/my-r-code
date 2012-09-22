#本程序用来发现朋友的朋友，以作为推荐关注用户
# http://xccds1977.blogspot.com/2012/02/twitter_25.html

rm(list=ls())
library(twitteR) #加载包
myid <- getUser('xccds') #获取用户信息
#取得100名关注对象的用户名
myfo <- twListToDF(myid$getFriends(n=100))$screenName  
ffo <-list()
record <- character()
for (i in 1:100){
  user <- getUser((myfo[i]))  #获取关注对象的信息
  #取得关注对象的关注对象
  ffo <- twListToDF(user$getFriends(n=100))$screenName
  record <-  c(record ,as.character(ffo))
}
# 生成频数表
table.record <- table(record)
# 从表格转化为数据框
data = as.data.frame(table.record,stringsAsFactors=F)
# 将已经关注的对象从中删除
data <- data[data$record%in%setdiff(record,myfo),]
# 选择频数最高的五人
data <- data[order(data$Freq,decreasing=T)[1:5],]
# 加载包并绘制条形图
library(ggplot2) 
p <- ggplot(data,aes(record,Freq))
p + geom_bar(aes(fill=Freq))+coord_flip()