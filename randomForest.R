# 随机森林和副产品
# http://xccds1977.blogspot.com/2012/07/blog-post_15.html

# 加载数据
data(PimaIndiansDiabetes2,package='mlbench')
# 用rf补充缺失值
library(randomForest)
data <- rfImpute(diabetes~.,PimaIndiansDiabetes2,iter=5)
model <- randomForest(diabetes~.,data=data,proximity=T,importance=T)
model$confusion

# 处理不平衡问题
model2 <- randomForest(diabetes~.,data=data,classwt=c(100,1),proximity=T,importance=T)
model2$confusion

# 用caret包观察最优参数
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
model3 <- train(diabetes~., data=data,method='rf',trControl = fitControl)
model3


# 模型误差率绘图，观察随着子树数目的变化，误差率的变化，黑色为袋外误差，绿色为pos类型的误差，红色为neg类型的误差
plot(model)

# 非监督学习
model <- randomForest(data[,-1])
# 将相似矩阵做为距离来绘制多维标度
MDSplot(model,data[,1])
# 根据相似矩阵来计算距离矩阵
dist <- 1-model$proximity
# 聚类分析
clust <- pam(dist,k=2) 

# 变量选择
result <- rfcv(data[,-1],data[,1],cv.fold=5)
result$error.cv

# 发现原型
prototype <- classCenter(data[,-9],data[,9],model2$prox)

# 变量重要程度
importance(model,type=1)
varImpPlot(model)

# 异常检验
plot(outlier(model2),type='h',col=c("red", "blue")[as.numeric(data$Class)])

# 观察变量的偏效应
partialPlot(model2, data, age, which.class='pos')

# 缺失值插补
data.imputed <- rfImpute(diabetes~.,PimaIndiansDiabetes2,iter=10)
