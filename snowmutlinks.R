# 用snow包来多核心计算
# http://xccds1977.blogspot.com/2012/02/snow.html

system.time({
library(MASS)
result <- kmeans(Boston,4,nstart=100000)
})
library(snow)
system.time({
data(Boston)
cl <- makeCluster(2,type='SOCK')
clusterExport(cl,'Boston')
#ignore <- clusterEvalQ(cl,{library(MASS); NULL})
results <- clusterApply(cl,rep(50000,2),function(nstart) kmeans(Boston,4,nstart=nstart))
i <- sapply(results,function(result) result$tot.withinss)
result <- results[[which.min(i)]]
stopCluster(cl)})



#失败的多机
system.time({
data(Boston)
win <-
    list(host="192.168.79.225",
         rscript="d:/Program Files/R/R-2.14.1/bin/Rscript.exe",
         snowlib="d:/Program Files/R/R-2.14.1/library")
cl <- makeCluster(2, type="SOCK")
cl <- makeCluster(win,manual=TRUE, type='SOCK',homogeneous=F)
clusterExport(cl,'Boston')
#ignore <- clusterEvalQ(cl,{library(MASS); NULL})
results <- clusterApply(cl,rep(50000,2),function(nstart) kmeans(Boston,4,nstart=nstart))
i <- sapply(results,function(result) result$tot.withinss)
result <- results[[which.min(i)]]
})

#用clustercall来计算自助法
data(mtcars)

rsq=function(data,indices){
  d=data[indices,]
  fit=lm(formula=mpg~wt+disp,data=d)
  return(summary(fit)$r.square)
  }

library(boot)

system.time({
results <- boot(data=mtcars,statistic=rsq,R=10000)
result <- unclass(results$t)
print(c(mean(result),sd(result)))
})

system.time({
cl <- makeCluster(2,type='SOCK')
results <- clusterCall(cl,boot,data=mtcars,statistic=rsq,R=5000)
result <- c(unclass(results[[1]]$t),unclass(results[[2]]$t))
print(c(mean(result),sd(result)))
stopCluster(cl)})

#snowfall
library(snowfall)
system.time({
    data(Boston)
    sfInit(parallel=T,cpus=2)
    sfExport()
        results <- clusterApply(cl,rep(50000,2),function(nstart) kmeans(Boston,4,nstart=nstart))
    i <- sapply(results,function(result) result$tot.withinss)
    result <- results[[which.min(i)]]
    stopCluster(cl)})