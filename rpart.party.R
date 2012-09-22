# rpart 和 party两种决策树的比较
# http://xccds1977.blogspot.com/2012/05/rpartparty.html

library(rpart)
install.packages('partykit')
library(partykit)
library(party)
names(iris)
model <- rpart(Species~.,data=iris,control=rpart.control(cp=0.01))
plotcp(model)
printcp(model)
plot(as.party(model))
pre1 <- predict(model,iris,type='class')
table(pre1,iris$Species)

model2 <-ctree(Species~.,data=iris)
plot(as.party(model2))
pre2 <- predict(model2,iris)
table(pre2,iris$Species)

data = iris
v = 10
cl = data$RainTomorrow
formula = RainTomorrow~.
# vknn = function(v,data,cl,k){
    # 分割原始数据
    grps = cut(1:nrow(data),v,labels=FALSE)[sample(1:nrow(data))]
    # 对每份数据分别运行KNN函数
    pred = lapply(1:v,function(i){
        omit = which(grps == i)
        pcl = predict(rpart(formula,data[-omit,]),data[omit,],type='class')
    })
    # 整合预测结果
    wh = unlist(pred)
    table(wh,cl[order(grps)])

pred = lapply(1:v,function(i){
    omit = which(grps == i)
    pcl = predict(ctree(formula,data[-omit,]),data[omit,])
})
# 整合预测结果
wh = unlist(pred)
table(wh,cl[order(grps)])

除了比较预测能力，还要比较第一个划分变量和树深度。