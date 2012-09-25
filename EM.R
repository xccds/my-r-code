# 已知条件
h = 20
c = 10
d = 10

# 随机初始两个未知量
miu = runif(1,0,1/6)
b = round(runif(1,1,20))

iter = 1
nonstop=TRUE
while (nonstop) {
    # E步骤，根据假设的miu来算b
    b = c(b,miu[iter]*h/(0.5+miu[iter]))
    # M步骤，根据上面算出的b再来计算miu
    miu = c(miu,(b[iter+1] + c)/(6*(b[iter+1]+c+d)))
    # 记录循环次数
    iter = iter + 1
    # 如果前后两次的计算结果差距很小则退出
    nonstop = (miu[iter]-miu[iter-1]>10^(-4))
}
print(cbind(miu,b))

# 设置模拟参数
miu1 <- 3
miu2 <- -2
sigma1 <- 1
sigma2 <- 2
alpha1 <- 0.4
alpha2 <- 0.6
# 生成两种高斯分布的样本
n <- 5000
x <- rep(0,n)
n1 <- floor(n*alpha1)
n2 <- n - n1
x[1:n1] <- rnorm(n1)*sigma1 + miu1
x[(n1+1):n] <- rnorm(n2)*sigma2 + miu2
hist(x,freq=F)
lines(density(x),col='red')


# 设置初始值
m <- 2
miu <- runif(m)
sigma <- runif(m)
alpha <- c(0.2,0.8)
prob <- matrix(rep(0,n*m),ncol=m)

for (step in 1:200){
    # E步骤
    for (j in 1:m){
        prob[,j]<- sapply(x,dnorm,miu[j],sigma[j])
    }
    sumprob <- prob %*% alpha

    for (j in 1:m){
        prob[,j] <- alpha[j]*prob[,j]/sumprob
    }
    

    oldmiu <- miu
    oldsigma <- sigma
    oldalpha <- alpha

    # M步骤
    for (j in 1:m){
        p1 <- sum(prob[ ,j])
        p2 <- sum(prob[ ,j]*x)
        miu[j] <- p2/p1
        alpha[j] <- p1/n
        p3 <- sum(prob[ ,j]*(x-miu[j])^2)
        sigma[j] <- sqrt(p3/p1)
    }

    # 变化
    epsilo <- 1e-4
    if (sum(abs(miu-oldmiu))<epsilo &
        sum(abs(sigma-oldsigma))<epsilo &
        sum(abs(alpha-oldalpha))<epsilo) break
    cat('step',step,'miu',miu,'sigma',sigma,'alpha',alpha,'\n')
}

library(mclust)
model <- densityMclust(x)
plot(model)
mc <-  Mclust(iris[,1:4], 3)
plot(mc, data=iris[,1:4], what="classification",dimens=c(3,4))
table(iris$Species, mc$classification)