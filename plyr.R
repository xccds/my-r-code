# 关于plyr包的使用方法
# http://xccds1977.blogspot.com/2012/04/plyrapply.html

library(plyr)
library(reshape2)

# 三种方式进行数据汇总
data.ddp <- ddply(iris,.(Species),function(df) mean(df[1:4]))
data.agg <- aggregate(iris[1:4],list(iris$Species),mean)

data.melt <- melt(iris,id=c('Species'))
data.dcast <- dcast(data.melt,Species~variable,mean)

# 更为复杂的数据分组计算
model <- function(x) {
    lm(Petal.Length~Petal.Width,data=x)
}

# 如果要用普通函数完成
pieces <- split(iris,list(iris$Species))
models <- lapply(pieces,model)
result <- lapply(models,coef)
do.call('rbind',result)
# 用plyr包只用下面
result1 <- dlply(iris,.(Species),model)
result2 <- ldply(result1,function(x) coef(x))
names(result2)[2:3] <- c('intercept', 'slope')

 # d dataframe a array l list
d_ply(iris,.(Species),function(df) mean(df[1:4]),.print=T)

# 两个特别的函数

mapply(rnorm,mean=1:5,sd=1:5, n=2)
mdply(data.frame(mean = 1:5, sd = 1:5), rnorm, n = 2)
replicate(n=20,expr=mean(runif(100)))
rdply(20, mean(runif(100)))

library(nnet)
nnet.m <- function(...) {
  nnet(Species~.,data=iris,trace=F,...)
}
opts <- data.frame(size=1:10,maxiter=50)
accuracy <- function(mod,true) {
  pred <- factor(predict(mod,type='class'),levels=levels(true))
  tb <- table(pred,true)
  sum(diag(tb))/sum(tb)
}
models <- mlply(opts,nnet.m)
ldply(models,'accuracy',true=iris$Species)

# 看这个网址 http://plyr.had.co.nz/09-user/
# http://www.jstatsoft.org/v40/i01/paper

