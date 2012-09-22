# 用glmnet包实施套索算法
# http://xccds1977.blogspot.com/2012/05/glmnetlasso.html

library(ggplot2)
library(glmnet)
library(reshpae)
# 读入数据
data <- read.csv('d:/ex2data2.txt',F)
# 散点图
ggplot()+ 
  geom_point(data=data,aes(V1,V2,colour=factor(V3),
                           shape=factor(V3)),size=3)
   
# 建立六阶多项式自变量
attach(data)
degree = 6
X = matrix(rep(1,length(V1)),ncol=1)
for (i in 1:degree) {
    for (j in 0:i) {
        X <-cbind(X, (V1^(i-j))*V2^j)
        }
    }
x<- X[,-1]
Y <- data$V3

# 用glmnet包建模
model <- cv.glmnet(x,Y,family="binomial",type.measure="deviance")
# 绘制CV曲线图，选择最佳lambda值
plot(model)
model$lambda.1se
# 提取最终模型
model.final <- model$glmnet.fit
# 取得简洁模型的参数系数
model.coef <- coef(model$glmnet.fit, s = model$lambda.1se)
# 取得原始模型的参数系数
all.coef <- coef(model$glmnet.fit, s =  max(model.final$lambda))

# 可以用predict进行预测
# pre <-predict(model.final,newx=x,s=model$lambda.1se,type='class')
# table(Y,pre)

# 下面的工作全部是为了绘制决策边界
u <- seq(-1,1.2, len=200)
v <- seq(-1,1.2, len=200)

z28 <-z9  <- matrix(0, length(u), length(v))

mapFeature <- function(u,v, degree=6) {
    out <- sapply(0:degree,function(i)
                  sapply(0:i, function(j)
                         u^(i-j) * v^j
                         )
                  )
    out <- unlist(out)
    return(out)
}

for (i in 1:length(u)) {
    for (j in 1:length(v)) {
        features <- mapFeature(u[i],v[j])
        z9[i,j] <- as.numeric(features %*% model.coef)
       z28[i,j] <- as.numeric(features %*%all.coef)
    }
}

rownames(z9) <- rownames(z28) <- as.character(u)
colnames(z9) <- colnames(z28) <-  as.character(v)

z9.melted <- melt(z9)
z28.melted <- melt(z28)
z9.melted <- data.frame(z9.melted, lambda=9)
z28.melted <- data.frame(z28.melted, lambda=28)

zz <- rbind(z9.melted, z28.melted)
zz$lambda <- factor(zz$lambda)
colnames(zz) <- c("u", "v", "z", "lambda")

p <- ggplot()+ 
  geom_point(data=data,aes(V1,V2,colour=factor(V3),shape=factor(V3)),size=3) +
  geom_contour(data=zz, aes(u, v, z = z,
                            group=lambda, colour=lambda),bins=1,size=1)
   
p