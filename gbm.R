# 用gbm包实现随机梯度提升算法
# http://xccds1977.blogspot.com/2012/07/gbm.html

library(gbm)


data(PimaIndiansDiabetes2,package='mlbench')
# 将响应变量转为0-1格式
data <- PimaIndiansDiabetes2
data$diabetes <- as.numeric(data$diabetes)
data <- transform(data,diabetes=diabetes-1)
# 使用gbm函数建模
model <- gbm(diabetes~.,data=data,shrinkage=0.01,
             distribution='bernoulli',cv.folds=5,
             n.trees=3000,verbose=F)
# 用交叉检验确定最佳迭代次数
best.iter <- gbm.perf(model,method='cv')
# 观察各解释变量的重要程度
summary(model,best.iter)
# 变量的边际效应
plot.gbm(model,1,best.iter)

# 用caret包观察预测精度
library(caret)
data <- PimaIndiansDiabetes2
fitControl <- trainControl(method = "cv", number = 5,returnResamp = "all")
model2 <- train(diabetes~., data=data,method='gbm',distribution='bernoulli',
                trControl = fitControl,verbose=F,
                tuneGrid = data.frame(.n.trees=best.iter,
                                      .shrinkage=0.01,.interaction.depth=1))
model2
