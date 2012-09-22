# stacking 
# 集成学习之堆叠泛化方法
# 
# 集成学习在前面文章已经提到不少了，今天就说的是一种称之为堆叠泛化(Stacked Generalization)方法，又叫作Stacking。这种方法是Wolpert在1992年提出来的，其具体计算步骤如下：
# 将样本分成训练数据和检验数据
# 用训练数据来训练若干基学习器
# 用检验数据来得到基学习器的拟合值
# 在此基础上新建一个模型，以前步得到的拟合值为输入，以检验数据的真实值为输出。
# 
# http://www.scholarpedia.org/article/Ensemble_learning
# http://en.wikipedia.org/wiki/Ensemble_learning


data(PimaIndiansDiabetes2,package='mlbench')
# 用rf补充缺失值
library(randomForest)
data <- rfImpute(diabetes~.,PimaIndiansDiabetes2,iter=5)

n <- dim(data)[1]
# 建立训练样本、调试样本和检测样本
sample <- sample(1:n,size=n,replace=F)
train.data <- data[1:(n/3),]
vali.data <- data[(n/3+1):(n*2/3),]
test.data <- data[(n*2/3+1):n,]

# NN
library(nnet)
library(caret)
# 获取最优参数并观察单独准确率
fitControl <- trainControl(method = "cv", 
                           number = 10,returnResamp = "all")
model.nnet <- train(diabetes~., data=train.data,method='nnet',
                trControl = fitControl,trace=F,maxit=200,
                tuneGrid = data.frame(.size=6,.decay=0.01))
pre.nnet <- predict(model.nnet,vali.data)                                      
confusionMatrix(pre.nnet,vali.data[,1])
# 单独的NN准确率0.76


model.nnet <- nnet(diabetes~.,data=train.data,
                   size=6,maxit=200,decay=0.01)
pre.nnet <- predict(model.nnet,vali.data,type='raw')

# RF
model.rf <- train(diabetes~., data=train.data,method='rf',
                    trControl = fitControl)
model.rf <- randomForest(diabetes~.,data=train.data,mtry=5)
pre.rf <- predict(model.rf,test.data,type='class')
confusionMatrix(pre.nnet,test.data[,1])
pre.rf <- predict(model.rf,vali.data,type='class')
# 55%

# SVM
library(kernlab)
model.svm <- train(diabetes~.,data=train.data,method='svmRadialCost',
      trControl = fitControl)
model.svm <- ksvm(diabetes~.,data=train.data,kernel='rbfdot',C=1)
pre.svm <- predict(model.svm,test.data,type='response')
confusionMatrix(pre.svm,test.data[,1])
pre.svm <- predict(model.svm,vali.data,type='response')

# ada
library(ada)
model.ada <- ada(diabetes~.,data=train.data,iter=350)
pre.ada <- predict(model.ada,vali.data)
confusionMatrix(pre.ada,vali.data[,1])

# stacking
data.stacking <- data.frame(target=test.data$diabetes,v1=pre.ada,
                            v2=pre.svm,v3=pre.rf,v4=pre.nnet)
library(klaR)
model <- NaiveBayes( target~ ., data = data.stacking)
pre <- predict(model,data.stacking)$class
confusionMatrix(pre,test.data[,1])


