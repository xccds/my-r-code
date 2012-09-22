
# 单变量时间序列分析函数小结

library(forecast)
library(fpp)

seasonplot(a10,year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

# 显示时序图
tsdisplay(a10)
Acf(a10)
Pacf(a10)
# boxcox transform
lambda <- BoxCox.lambda(a10) 
plot(BoxCox(a10,lambda))

# STL分解
fit1 <- stl(a10,s.window="periodic" )
plot(fit1)
plot(a10,col="gray")
lines(fit1$time.series[,2],col='red',ylab='Trend')

# 自动建模holtwinter
train <- window(a10,end=c(2007,1))
test <- window(a10,start=c(2007,1))
fit2 <- ets(train)
plot(forecast(fit2)
# 观察残差
tsdisplay(fit2$residuals)
#  检验是否存在自相关
Box.test(fit2$residuals, lag=10)
# 单位根检验平稳性
adf.test(fit2$residuals)
# 预测误差
accuracy(forecast(fit2), test)

# 自动建模 arima
fit3 <- auto.arima(train, stepwise=FALSE, approximation=FALSE))
plot(forecast(fit3))
# 观察残差
tsdisplay(fit3$residuals)
# 检验是否存在自相关
Box.test(fit3$residuals, lag=10)
# 单位根检验平稳性
adf.test(fit3$residuals)
# 预测误差
accuracy(forecast(fit3), test)

#参考资料
http://www.jstatsoft.org/v27/i03/paper
http://otexts.com/fpp/

          