# 你应该相几次亲？
# http://xccds1977.blogspot.com/2012/03/blog-post_11.html

selection <- function(n) {
  raw.data <- sample(1:100,100)
  first.group <- raw.data[1:n]
  second.group <- raw.data[(n+1):100]
  first.max <- max(first.group)
  morethan.first <- second.group > first.max
  my.select <- ifelse(any(morethan.first) == TRUE, 
                      second.group[morethan.first][1],0)
  return(my.select)
}


data <- matrix(rep(0,10000*100),ncol=100)
result <- rep(0,100)
for (i in 1:100) {
  temp <- replicate(n=10000,selection(i))
  data[ ,i] <- temp
  result[i] <- sum(data[,i] == 100)
}
which.max(result)
plot(result,type='l')
library(ggplot2)
index <- 1:100
p <- ggplot(data=data.frame(index,result),aes(index,result))
p+geom_line(size=1, colour='turquoise4') + 
  geom_point(aes(x = which.max(result),y=result[which.max(result)]),colour=alpha('red',0.5),size=5) 

hist(replicate(n=10000,selection(37)))
table(replicate(n=10000,selection(7)))


result <- rep(0,100)
for (i in 1:100) {
  result[i] <- mean(replicate(n=10000,selection(i)))
}
p <- ggplot(data=data.frame(index,result),aes(index,result))
p+geom_line(size=1, colour='turquoise4') + 
  geom_point(aes(x = which.max(result),y=result[which.max(result)]),colour=alpha('red',0.5),size=5)
which.max(result)


howmany <- function(n) {
  raw.data <- sample(1:100,100)
  first.group <- raw.data[1:n]
  second.group <- raw.data[(n+1):100]
  first.max <- max(first.group)
  morethan.first <- second.group > first.max
  which.select <- ifelse(any(morethan.first) == TRUE, 
                      which(morethan.first==T)[1],0)
  return(which.select)
} 

result<- replicate(n=10000,howmany(7))
length(result[result>0 & result <10])/10000
length(result[result>0 & result <30])/10000

p <- ggplot(data=data.frame(result),aes(result))
p + geom_histogram(binwidth=1,position = 'identity',
    alpha=0.5,
    aes(y = ..density..,))+
stat_density(geom = 'line')