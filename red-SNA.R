# 进一步我们用igraph包探索红楼梦中的各人物之间社交关系，
# 本文对关系的定义比较粗浅，即如果有两个人的名字出现在同一段落中，可以认为之间有联系。

# 前八十回里面，妙玉和元春没有联系，妙玉和惜春、巧姐之间也没有联系，其它人之间联系非常多。但在后四十回中，元春去世使得联系大减，妙玉被劫，她和湘云之间不再有联系。

# 导入数据
text <- readLines('d:\\honglou.txt',encoding='UTF-8')
#去除空白行
text <- text[text!='']
# 找出每一章节的头部行数和尾部行数
chapbegin <- grep('第.{1,3}回 ',text)
chapend <- c((chapbegin-1)[-1],length(text))
# 计算出每一回的段落数
paragraph <- chapend-chapbegin

# 建立一个搜索函数，可以返回出现的频次
find80 <- function(name) {
  gregout <- gregexpr(name,text) 
  namefun <- function(x) { #x表示章节序号
    temp <- attr(gregout[[x]],'match.length')
    ifelse(temp[1]>0,length(temp),0)
  }
  sapply(1:before80,FUN=namefun)
}

find40 <- function(name) {
  gregout <- gregexpr(name,text) 
  namefun <- function(x) { #x表示章节序号
    temp <- attr(gregout[[x]],'match.length')
    ifelse(temp[1]>0,length(temp),0)
  }
  sapply((before80+1):length(text),FUN=namefun)
}


# 前八十回的段落数
before80 <- sum(paragraph[1:80])
  
  
people <- c('宝玉','黛玉','宝钗','湘云','巧姐',
  '探春','元春','惜春','凤姐','妙玉','迎春')
library(plyr)
x <- aaply(.data=people,.margins=1,.fun=find40)
x[x>=1] <- 1



matr.adj <- x %*%t(x)
g <- graph.adjacency(matr.adj,weighted=T,mode='undirected')

g <-simplify(g)
V(g)$label<-c('宝玉','黛玉','宝钗','湘云','巧姐',
              '探春','元春','惜春','凤姐','妙玉','迎春')
V(g)$degree<- degree(g)

layout1 <- layout.fruchterman.reingold(g)
egam <- (log(E(g)$weight)+0.4) / max(log(E(g)$weight)+0.4)
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
E(g)$width <- egam
E(g)$color <- rgb(0, 0, 1, egam)

plot(g, layout=layout1)

is.connected(g)
clusters(g)
summary(g)
degree(g)
graph.density(g)
transitivity(g)

# 可以使用最基本的graph函数，用向量作为参数来创建图形，之后用plot绘制出结果
library(igraph)
g1 <- graph( c(0,1, 1,2, 2,3, 3,4))
plot(g1,layout=layout.circle(g1))
# 也可以画出一些特殊结构的图形，例如下面的星形图
g2 <- graph.star(10, mode = "in")
plot(g2,layout=layout.fruchterman.reingold(g2))
# 这是一个立方体的透视
g3 <- graph.famous('Cubical')
plot(g3,layout=layout.fruchterman.reingold(g3))
# 当然也可能从文件创建图形
# 首先读入数据，整理后用graph.data.frame函数创建图形对象
traits <- read.csv('http://igraph.sourceforge.net/igraphbook/traits.csv', head=FALSE)
names(traits) <- c('name','age','gender')
traits[,1] <- sapply(strsplit(as.character(traits[,1]),' '),'[',1)
relation <- read.csv('http://igraph.sourceforge.net/igraphbook/relations.csv', head=FALSE)
names(relation) <- c('from','to','sameroom','friendship','advice')
g4 <- graph.data.frame(relation,vertices=traits)
plot(g4,layout=layout.kamada.kawai,vertex.shape='rectangle',vertex.label=V(g4)$name,vertex.size=20,asp=F)
# 可以通过summary函数观察内部结构，看到了10个顶点，34条边
summary(g4)

