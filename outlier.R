# 在R中进行异常值检验
#  http://xccds1977.blogspot.com/2012/07/r.html

library(mvoutlier)
set.seed(1234)
x <- cbind(rnorm(80), rnorm(80))
y <- cbind(rnorm(10, 5, 1), rnorm(10, 5, 1))
z <- rbind(x,y)
# 一维数据的异常检验
res1 <- uni.plot(z)
# 返回异常值的编号
which(res1$outliers==T)
# 基于稳健马氏距离的多元异常值检验
res2 <-aq.plot(z)
# 返回异常值的编号
which(res2$outliers==T)
# 在高维空间中，数据之间变得稀疏，而使得距离不再有很大意义，此时可以融合主成分降维的思路来进行异常值检验，两种高维检验的方法
data(swiss)
res3 <- pcout(swiss)
# 返回异常值的编号
which(res3$wfinal01==0)



