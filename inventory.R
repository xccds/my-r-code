# 订货模型
# http://xccds1977.blogspot.com/2012/04/r.html

rm(list=ls())
inventory = function(s,S) {
    #x为存货，T为总时长，lambda为需求速率，（s,S）为存货策略
    #h为单位库存费用,L为订货周期，d为订货费用函数，lose为损失函数
    #a为需求量，b为相应的概率
    x=4; L=2; h =0.5
    r=12; a=1:4; b=c(0.7,0.2,0.08,0.02)
    lambda=8; T=30
    d = function(x) 10+ 5*x
    lose = function(D,x) (D-x)*2
    # t0为下一个顾客到达时间，服从指数分布，t1为订货交付时间
    #若没有订货则设为Inf，H为累积库存费用，C为累积订货费用，R为收入
    #y为存货订货量，loss为损失金额，k为计数
    t=0; t0=rexp(1,lambda)
    t1=Inf; H=0; C=0; R=0; y=0
    loss=0; k=1
    while(t0[k] <= T) {
#         browser()
        k = k+1
        if (t0[k-1] < t1[k-1]) {
            #计算t时到下一个顾客到来时的库存费用
            H= H + (t0[k-1] - t) * h*x
            t = t0[k-1]
            #D为模拟的顾客需求量，w为购买量
            D = sample(a,1,prob=b)
            w = min(D,x)
            #需求大于存货，则发生损失
            if (D > x) { loss[k-1]=lose(D,x) }
            else {loss[k-1] =0 }
            # 计算收入并更新存货
            R = R + w*r; x=x-w
            # 若存货不足则进行订货补充，到货时间为t1。
            if(x <s & y==0) {
                y = S-x; t1[k] = t +L
            } else { t1[k]= t1[k-1]}
           # 模拟新的顾客需求
            t0[k]=t+rexp(1,lambda)
            loss[k] =0
            # 若新的存货比需求先到达，则计算订货成本并更新存货
        } else {
            H= H + (t1[k-1]-t)*h*x
            t=t1[k-1]
            C=C+d(y); x=x+y
            y=0;t1[k]= Inf; loss[k]=0; t0[k]= t0[k-1]
        }
    }
    return((R-H-C-sum(loss))/T)
}

library(lattice)
s <- 20:50
S <- 40:70
data <- expand.grid(s,S)
for (i in 1:900) {
    data$mean[i] <- mean(replicate(100,inventory(data$Var1[i],data$Var2[i])))
    }
wireframe(mean ~ Var1 * Var2, data = data,
          scales = list(arrows = FALSE),xlab = "再订货点",
          ylab = "订货数量", zlab='平均收入',
          drape = TRUE, colorkey = TRUE,
          screen = list(z = 120, x = -60))
levelplot(mean ~ Var1 * Var2, data = data,xlab = "再订货点",
          ylab = "订货数量")

data[order(data$mean,decreasing=T)[1:10],]

result <- replicate(1000,inventory(29,57))
library(ggplot2)
p <- ggplot(data.frame(result),aes(x=result))
p+geom_histogram(colour = "darkgreen", fill = "white",
                    aes(y = ..density..)) +
                    stat_density(geom = 'line',colour='red4',size=1)