# 用lubridate包处理时间数据
# http://xccds1977.blogspot.com/2012/07/lubridate.html

library(quantmod)
library(ggplot2)
library(lubridate)
# 读取上证指数历史数据
getSymbols('^SSEC',src='yahoo',from = '1997-01-01')
time <- ymd(as.character(index(SSEC)))
open <- as.numeric(Op(SSEC))
high <- as.numeric(Hi(SSEC))
low <- as.numeric(Lo(SSEC))
close <- as.numeric(Cl(SSEC))
volume <- as.numeric(Vo(SSEC))
# 根据收盘和开盘计算当日收益率
profit <- (close-open)/open
# 提取时间数据中的周数和月份
wday <- wday(time)-1
mday <-month(time)

data <- data.frame(time,wday,mday,profit)
p <- ggplot(data,aes(factor(mday),factor(wday),z=profit))
# 收益率的热图，图中颜色越浅，表示汇集到这个组中的收益率中位数越高
p +stat_summary2d(fun=function(x) median(x))+
  opts(legend.position = "top")+ labs(x='月份',y='星期')



# 直方图和估计
p <- ggplot(data,aes(profit))
p + geom_histogram()
mean(profit)
sd(profit)
length(profit[profit>0.0153])/length(profit)

# 观察大涨的日期
p <- ggplot(data,aes(factor(mday),factor(wday),z=big))
# 波动或是收益的热图，周一容易大涨
p + stat_summary2d(fun=function(x) sum(x))
# 观察大跌的日期
p <- ggplot(data,aes(factor(mday),factor(wday),z=bigloss))
# 周一容易大涨
p + stat_summary2d(fun=function(x) sum(x))

# 观察连续涨跌的游程
table(rle(winloss))
600111.SS

# 建立一个函数来返回图形
stockfun <- function(x) {
    x <- as.character(x)
setSymbolLookup(name=list(name=x, src="yahoo"))
getSymbols('name')
time <- ymd(as.character(index(NAME)))
open <- as.numeric(Op(NAME))
high <- as.numeric(Hi(NAME))
low <- as.numeric(Lo(NAME))
close <- as.numeric(Cl(NAME))
range <- (high-low)/open
profit <- (close-open)/open
wday <- wday(time)
mday <-month(time)
yday <- yday(time)
year <- year(time)
data <- data.frame(time,year,wday,mday,range,profit,yday)
p <- ggplot(data,aes(factor(mday),factor(wday),z=profit))+
    stat_summary2d(fun=function(x) median(x))
print(p)
}
