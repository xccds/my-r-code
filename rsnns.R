#BP  RBF 神经网络比较
# http://xccds1977.blogspot.com/2011/09/rrsnns.html

library(caret)


data(LetterRecognition,package='mlbench')
head(LetterRecognition)

fitControl <- trainControl(method = "cv", number = 10,returnResamp = "all")

# 用caret包观察最优参数
library(RSNNS)
data(iris)

#shuffle the vector
n <- nrow(LetterRecognition)
data <- LetterRecognition[sample(1:n,n),1:ncol(LetterRecognition)]

targets <- decodeClassLabels(data[,1])
values <- data[,-1]

temp <-1:20
for ( i in 1:20){
    data <- splitForTrainingAndTest(values, targets, ratio=0.2)
    data <- normTrainingAndTestSet(data)
    model <- mlp(data$inputsTrain, data$targetsTrain, size=(i+9), inputsTest=data$inputsTest, targetsTest=data$targetsTest)
    predictions <- predict(model,data$inputsTest)
    table <- confusionMatrix(data$targetsTest,predictions)
    temp[i]<-sum(diag(table))/4000
}
temp

plotROC(predictions[,2], data$targetsTest[,2])

