#以T检验为基础的交易系统
#首先载入两个所需程序包
library(quantmod)
library(PerformanceAnalytics)
#从YAHOO得到上海综指数据
getSymbols('^SSEC',src='yahoo')
#取出收盘价
close=as.numeric(Cl(SSEC))
#计算差分
d=c(NA,diff(close))
#计算总时长
n=length(close)
#预先赋空值 mean.c为收盘价均值，mean.d为波动均值，sd为波动标准差,
#test为判断趋势的函数，即T 统计量，sig为仓位,up/down为上下界
mean.c=mean.d=sd=test=sig=up=down=max.c=min.c=numeric(n)

#以约20单位为一周期，循环计算各变量
for ( i in 20:(n-2)){
  mean.d[i]=mean(d[(i-19):i],na.rm=T)#计算本周期波动均值
  mean.c[i]=mean(close[(i-19):i])#计算本周期收盘均值
  sd[i]=sd(d[(i-19):i],na.rm=T)#计算本周期标准差
  test[i]=abs(mean.d[i]/(sd[i]/sqrt(20)))#计算判断指标，T 统计量
  up[i]=mean.c[i]+2*sd[i]#计算突破上界
  down[i]=mean.c[i]-2*sd[i]#计算突破下界
   max.c[i]=min.c[i]=close[i]

  #若为震荡市，出现突破则入场
  if (test[i]<0.5&close[i+1]>up[i]) sig[i+2]=1 
  if (test[i]<0.5&close[i+1]<down[i]) sig[i+2]=-1 
  #若为趋势市，则入场
  if (test[i]>0.8&mean.d[i]>0) sig[i+1]=1
  if (test[i]>0.8&mean.d[i]<0) sig[i+1]=-1

  #若收盘打到止损位，出场
  if (sig[i]==1){
    max.c[i]=max(max.c[i-1],close[i])
  if (close[i+1]<max.c[i]-1.5*sd[i]) sig[i+2]=0
  }
  if (sig[i]==-1){
  min.c[i]=min(min.c[i-1],close[i])
  if (close[i+1]>min.c[i]+1.5*sd[i]) sig[i+2]=0
  }
}
#计算收益率
ret=na.omit(ROC(type='discrete',close)*sig)
eq=cumprod(1+ret)
plot(eq,type='l')

