detectCores(logical = FALSE)

library(parallel)
detectCores()

# 并行计算euler14问题
# 自定义函数以返回原始数值和步数
func <- function(x) {
    n = 1
    raw <- x
    while (x > 1) {
        x <- ifelse(x%%2==0,x/2,3*x+1)
        n = n + 1
    }
    return(c(raw,n))
}

library(parallel)
# 用system.time来返回计算所需时间
system.time({
    x <- 1:1e6
    cl <- makeCluster(4)  # 初始化四核心集群
    results <- parLapply(cl,x,func) # lapply的并行版本
    res.df <- do.call('rbind',results) # 整合结果
    stopCluster(cl) # 关闭集群
})
# 找到最大的步数对应的数字
res.df[which.max(res.df[,2]),1]

# 生成随机数
RNGkind("L'Ecuyer-CMRG")
cl <- makeCluster(4)
clusterSetRNGStream(cl, 123)
unlist(clusterEvalQ(cl, rnorm(10)))
stopCluster(cl)

library(foreach)
# 非并行计算方式，类似于sapply函数的功能
x <- foreach(x=1:1000,.combine='rbind') %do% func(x)

# 启用parallel作为foreach并行计算的后端
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
# 并行计算方式
x <- foreach(x=1:1000,.combine='rbind') %dopar% func(x)
stopCluster(cl)

# 随机森林的并行计算
library(randomForest)
    cl <- makeCluster(4)
    registerDoParallel(cl)
    rf <- foreach(ntree=rep(25000, 4), 
                  .combine=combine,
                  .packages='randomForest') %dopar%
          randomForest(Species~., data=iris, ntree=ntree)
    stopCluster(cl)

# plyr的并行处理
library(plyr)
x <- seq_len(20)
wait <- function(i) Sys.sleep(0.3)
system.time(llply(x, wait)) # 6 sec
system.time(llply(x, wait, .parallel = TRUE)) # 3.53 sec