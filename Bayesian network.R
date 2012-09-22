# 贝叶斯信念网络
# http://xccds1977.blogspot.com/2012/07/blog-post.html

install.packages('bnlearn')
source("http://bioconductor.org/biocLite.R")
biocLite("graph")

# 加载包
library(bnlearn)
# 离散化
data2 <- discretize(data[-9],method='quantile')
data2$class <- data[,9]
# 使用爬山算法进行结构学习
bayesnet <- hc(data2)

# bn.d <- gs(d)
# 显示网络图
plot(bayesnet)
# 修改网络图中的箭头指向
bayesnet<- set.arc(bayesnet,'age','pregnant')
# skeleton(bn.d)
# 观察连接强度
# arc.strength(bayesnet,data2)
# ci.test('class','age',data=data2)
# 参数学习
fitted <- bn.fit(bayesnet, data2,method='mle')
# 训练样本预测并提取混淆矩阵
pre <- predict(fitted,data=data2,node='class')
confusionMatrix(pre,data2$class)
# 进行条件推理
cpquery(fitted,(class=='pos'),(age=='(36,81]'&mass=='(34.8,67.1]'))

fitted$class$prob

# res <- empty.graph(names(learning.test))
# modelstring(res)<-'[A][C][F][B|A][D|A:C][E|B:F]'
# plot(res)
# 
# 
# blacklist
# fitted$A$prob <- c(0.1,0.4,0.5)

library(klaR)
model1 <- NaiveBayes(Species~., data=iris,usekernel=T)
pred1 <- predict(model1, iris[-5])
table(pred1$class, iris$Species)

# 加载扩展包和数据
library(caret)
data(PimaIndiansDiabetes2,package='mlbench')
# 对缺失值使用装袋方法进行插补
preproc <- preProcess(PimaIndiansDiabetes2[-9],method="bagImpute")
data <- predict(preproc,PimaIndiansDiabetes2[-9])
data$Class <- PimaIndiansDiabetes2[,9]
# 使用朴素贝叶斯建模，这里使用了三次10折交叉检验得到30个结果
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
model1 <- train(Class~., data=data,method='nb',trControl = fitControl,tuneGrid = data.frame(.fL=1,.usekernel=F))

model1 <- train(Class~., data=data,method='svmRadialCost',trControl = fitControl)
# 观察30次检验结果，发现准确率在0.75左右
resampleHist(model1)
# 返回训练数据的混淆矩阵
pre <- predict(model1)
confusionMatrix(pre,data$Class)


model1 <- train(Class~., data=data,method='svmRadialCost',trControl = fitControl)
# 观察30次检验结果，发现准确率在0.75左右
resampleHist(model1)

model1 <- train(Class~., data=data,method='svmLinear',trControl = fitControl)
# 观察30次检验结果，发现准确率在0.75左右
resampleHist(model1)

model1 <- train(Class~., data=data,method='svmPoly',trControl = fitControl)
# 观察30次检验结果，发现准确率在0.75左右
resampleHist(model1)