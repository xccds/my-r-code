# arima时间序列模型

data(Tbrate,package='Ecdat')
library(tseries)
plot(Tbrate)
acf(Tbrate)
adf.test(Tbrate[,2])

diff_rate = diff(Tbrate)
adf.test(diff_rate[,1])
adf.test(diff_rate[,2])
adf.test(diff_rate[,3])
pairs(diff_rate) # scatterplot matrix
plot(diff_rate) # time series plot
acf(diff_rate)

par(mfrow=c(1,1))
boxplot(diff_rate[,1] ~ cycle(diff_rate))

library(forecast)
auto.arima(Tbrate[,1],max.P=0,max.Q=0,ic="aic")

fit1 = arima(Tbrate[,1],order=c(0,1,1))
acf(residuals(fit1))
Box.test(residuals(fit1), lag = 10, type="Ljung")
AIC(fit1)

resid2 = residuals(fit1)^2
plot(resid2)
plot(residuals(fit1))
acf(resid2)
Box.test(resid2, lag = 10, type="Ljung")
