# 高维数据的降维，过滤方法
# http://xccds1977.blogspot.com/2012/06/blog-post_05.html

# 加载扩展包和数据集mdrr，得到自变量集合mdrrDescr和因变量mdrrClass
library(caret)
data(mdrr)
# 先删去近似于常量的变量
zerovar <- nearZeroVar(mdrrDescr)
newdata1 <- mdrrDescr[,-zerovar]
# 再删去相关度过高的自变量
descrCorr <- cor(newdata1)
highCorr <- findCorrelation(descrCorr, 0.90)
newdata2 <- newdata1[, -highCorr]
# 数据预处理步骤（标准化，缺失值处理）
Process <- preProcess(newdata2)
newdata3 <- predict(Process, newdata2)
# 用sbf函数实施过滤方法，这里是用随机森林来评价变量的重要性
data.filter <- sbf(newdata3,mdrrClass,
              sbfControl = sbfControl(functions=rfSBF,
                                      verbose=F,
                                      method='cv'))
# 根据上面的过滤器筛选出67个变量
x <- newdata3[data.filter$optVariables]
# 再用rfe函数实施封装方法，建立的模型仍是随机森林
profile <- rfe(x,mdrrClass,
               sizes = c(10,20,30,50,60),
               rfeControl = rfeControl(functions=rfFuncs
               ,method='cv'))
# 将结果绘图，发现20-30个变量的模型精度最高
plot(profile,type=c('o','g'))
