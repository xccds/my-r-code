# 布丰投针试验的R实现
# http://xccds1977.blogspot.com/2012/03/r_27.html



needle <- function(d=1,l=0.5,n=100000) {
    #d表示表格空，l表示针长度
    y <- runif(n,min=0,max=d/2)
    x <- runif(n,0,pi)
    cross <- ifelse(0.5*l*sin(x)>=y,1,0)
    return(1/mean(cross))
}

needle()

rm(list=ls())
# 绘制空白图形
plot(c(0,2),c(0,2),type='n',main='布丰投针实验',xlab='X',ylab='Y')
# 增加平行线
abline(h=0.5)
abline(h=1.5,col='red')
finished <- FALSE
# trial为实验次数，cross为交叉次数
trial <- 0
cross <- 0
while (!finished) {
    # Dist为针的中心距离红线的垂直距离
    # Theta为针的角度
    Dist <- runif(1,min=0,max=1/2)
    Theta <- runif(1,0,pi)
    # central.x为针中心点的横坐标
    # central.y为针中心点的纵坐标
    central.x <- runif(1,0.5,1.5)
    central.y <- Dist +1
    # 计算针两端的坐标
    y1 <- sin(Theta)/4 + central.y
    x1 <- cos(Theta)/4 + central.x
    y2 <- sin(Theta+pi)/4 + central.y
    x2 <- cos(Theta+pi)/4 + central.x
    trial <- trial +1
    # 计数交叉次数
    cross <- cross + ifelse(0.25*sin(Theta)>=Dist,1,0)
    # 绘制针的线型和中心点
    lines(c(x1,x2),c(y1,y2),lty=2)
    points(central.x,central.y,pch=16,col='grey')
    cat('trial=',trial,'cross=',cross,'PI=',trial/cross,'\n')
    #continue?
    input <- readline('stop?')
    # 若输入y，则结束实验
    if (input =='y') finished <- TRUE
}

trial <- 0
cross <- 0
for (i in 1:10000) {
    Dist <- runif(1,min=0,max=1/2)
    Theta <- runif(1,0,pi)
    central.x <- runif(1,0.5,1.5)
    central.y <- Dist +1
    y1 <- sin(Theta)/4 + central.y
    x1 <- cos(Theta)/4 + central.x
    y2 <- sin(Theta+pi)/4 + central.y
    x2 <- cos(Theta+pi)/4 + central.x
    trial <- trial +1
    cross <- cross + ifelse(0.25*sin(Theta)>=Dist,1,0)
#     lines(c(x1,x2),c(y1,y2),lty=2)
#     points(central.x,central.y,pch=16,col='grey')
    #     cat('central=',c(central.x,central.y),'\n')
    cat('trial=',trial,'cross=',cross,'PI=',trial/cross,'\n')
}