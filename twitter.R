# twitter包的使用
# http://xccds1977.blogspot.com/2012/02/twittertwitter.html

library(twitteR)
library(ggplot2)
me <- getUser('aiww')
lianyue <- getUser('lianyue')

summary(df.sub[,c(2,3,5)])

follow <- me$getFollowers(n=5000)
follow.ai <- follow
follow.lian <- lianyue$getFollowers(n=5000)
df.ai <- df
df.lian <- do.call('rbind',lapply(follow.lian,as.data.frame))
df.sub <- subset(df.lian,friendsCount<2300 & followersCount<3000 & statusesCount<10000)
df.sub$time <- as.Date(df.sub$created)
df.sub$ntime <- as.numeric(df.sub$time)

#观察用户的开推时间
p<-ggplot(df.sub,aes(x=time))
p+geom_bar(fill='red',colour='black',binwidth=30)

#散点图
v=median(df.sub$friendsCount)
p <- ggplot(data=df.sub,aes(x=friendsCount,y=followersCount))
p + geom_point(aes(size=statusesCount,colour=ntime),alpha=0.8)
+coord_cartesian(xlim = c(0, 500),ylim = c(0, 500))
#加性模型
library(mgcv)
model <- gam(followersCount~s(friendsCount)+s(statusesCount)+s(ntime),data=df.sub)
par(mfrow=c(1,3))
plot(model,se=T)


#+coord_cartesian(xlim = c(0, 500),ylim = c(0, 2000)) 

p <- ggplot(data=df.sub,aes(x=friendsCount,y=followersCount))
p + geom_point(aes(size=statusesCount),alpha=0.4)

five.mao <- subset(df.sub, friendsCount>100 & followersCount<=1)
length(five.mao)

which(df$followersCount>50000 & df$friendsCount>25000)
df[1802,]

with(df,summary(friendsCount)
with(df,summary(followersCount)     

p <- ggplot(data=df,aes(y=friendsCount))
p + geom_boxplot()

with(df,plot(followersCount~statusesCount))
df$created
     
     
     
