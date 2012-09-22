# 狼兔追逐问题
# http://xccds1977.blogspot.com/2012/04/blog-post_09.html

# V为狼的速度倍数，delta_T为时间间隔，eps为抓到的距离
trace <- function(v=2,delta_T=0.001,eps=0.01) {
  plot(c(0,33,35,0),c(0,0,35,20),xlab='',ylab='')
  text(0,20,labels='B',adj=c(0.3,-0.8))
  text(33,0,labels='A',adj=c(0.3,-0.8))
  text(0,0,labels='O',adj=c(-0.5,0.1))
  # x为兔子的坐标矩阵，y为狼的坐标矩阵
  x <- matrix(c(0,0),nrow=1)
  y <- matrix(c(33,0),nrow=1)
  a <- 0
  b <- 0
  time <- 20/delta_T
  for (j in 1:time) { 
    # d为二者之间的距离，a为兔子的新位置，b为狼的新位置
    d <- sqrt((x[j,1]-y[j,1])^2+(x[j,2]-y[j,2])^2)
    a[1] <- 0
    a[2] <- x[j,2] + delta_T
    b[1] <- y[j,1] + v*delta_T*(x[j,1] - y[j,1])/d
    b[2] <- y[j,2] + v*delta_T*(x[j,2] - y[j,2])/d
    # 若距离小于抓捕距离则离开循环
    if(d < eps) break
    # 将新位置加入到坐标矩阵中
    x <- rbind(x,a)
    y <- rbind(y,b)
  }
  # 绘制二者的追逐的路线
  lines(x[1:j,1],x[1:j,2],col=3)
  lines(y[1:j,1],y[1:j,2],col=2)
  if ( d < eps & j <= time) return(list(result='TRUE',x=x[[j,1]],y=x[[j,2]]))
  else return(list(result='FALSE',dist=as.vector(d)))
}
