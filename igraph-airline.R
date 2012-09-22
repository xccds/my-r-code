# 用网络图来分析机场数据
# http://xccds1977.blogspot.com/2012/07/blog-post_31.html

# 读取数据 
data.port <- read.csv('d:/airports.dat',F)
data.line <- read.csv('d:/routes.dat',F)

# 机场数据整理，除去重复的和没有编号的机场
airports <-data.port[data.port$V5!='',
                      c('V5','V4','V3')]
names(airports) <- c('name','country','city')
airports$city <- as.character(airports$city)
airports <- airports[!duplicated(airports$name),]

# 航线数据整理，只保留起飞和降落地点
goodlines <- (data.line[,'V3'] %in% airports$name) &
       (data.line[,'V5'] %in% airports$name)
airlines <- data.line[goodlines,c('V3','V5')]
names(airlines) <- c('from','to')

library(igraph)
# 生成网络图对象
g <- graph.data.frame(airlines, vertices=airports,directed=F)
# 删除没有航线的机场
g<- g - V(g)[degree(g)==0]
summary(g)
# 存在多条航线,合并航线，同一航线的频次放入weight属性中
is.multiple(g)
E(g)$weight <- count.multiple(g)
g1 <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE,
              edge.attr.comb = 'mean')
summary(g1)

# 从上面观察整个机场群共有3218个机场，16822条航线，来判断这些机场整体是否连通？
is.connected(g1)
# 存在一些不能连通的机场，观察有哪些相互断开的机场群？
clusters(g1)$csize
V(g1)[clusters(g1)$membership==2]$country
# 发现有一些机场属于西南太平洋上的岛国，去掉这些处于边缘状态的机场
g2 <- g1 - V(g1)[clusters(g1)$membership!=1]

#哪一对机场之间航班数最多
E(g2)[which.max(E(g2)$weight)]
V(g2)['BKK']$city
V(g2)['HKG']$city

# 哪个机场连接数最多,法兰克福与237个机场有直飞航班
V(g2)[which.max(degree(g2))]$city
plot(degree.distribution(g2), log="xy")
V(g2)$city[order(degree(g2),decreasing=T)][1:10]

# 机场前十强，根据page.rank算法
V(g2)$city[order(page.rank(g2)$vector,decreasing=T)][1:10]

# 观察某个结点相关的连线信息
# incident(g,'PEK',mode='total')
# neighbors(g,'PEK',mode=1)
# E(g1) [ 'PEK'%->% 'SHA' ]
# E(g1) [ 'PEK'%<-% 'SHA' ]
# V(g1)['PEK']$city
# V(g1)['SHA']$city

# 中枢机场，哪些点是关节点
# V(g2)$city[articulation.points(g2)]


# 两个结点之间是否连通直航
are.connected(g2,'WUH','CAI')

# 最短连线，如何转机wuhan->cairo
# airports[airports$city=='Wuhan',]
# airports[airports$city=='Cairo',]
V(g2)[get.shortest.paths(g2,'WUH','CAI')[[1]]]$city

# 社群探测
commu <-  fastgreedy.community(g2)
commu$membership
sizes(commu)
V(g2)[commu$membership==1]$country
V(g2)[commu$membership==2]$country
V(g2)[commu$membership==3]$country


# 压缩节点
# contract.vertices


# 为画图准备
rank.max <- max(page.rank(g2)$vector)
rank.min <- min(page.rank(g2)$vector)
size <- 20*(page.rank(g2)$vector -rank.min)/(rank.max-rank.min)+1
V(g2)$size <- size
V(g2)$color <- 'grey'
V(g2)$frame.color <- 'black'

V(g2)[commu$membership==1]$color <- 'red4'
V(g2)[commu$membership==2]$color <- 'darkseagreen3'
V(g2)[commu$membership==3]$color <- 'darkslategray3'

V(g2)$label <- NA
E(g2)$color <- 'white'

#V(g1)$label <- V(g1)$city

plot(g2, layout=layout.sphere)
plot(g2,layout=layout.fruchterman.reingold)




res <- spinglass.community(g)
str(g)
degree(g)
vcount(g)
neighbors(g,'PEK')

# 中心评分，对有向图
alpha.centrality(g)
betweenness(g)
closeness(g2)

# 计算关键节点
V(g)[articulation.points(g)]$city


# 计算有关键节点连接的社群
biconnected.components(g1)
# 派系探测
V(g2)[largest.cliques(g2)[[1]]]$city
V(g2)[largest.cliques(g2)[[5]]]$city

