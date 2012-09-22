# 折腾一下红楼梦的数据
# http://xccds1977.blogspot.com/2012/06/blog-post.html

# 导入数据
text <- readLines('d:\\honglou.txt',encoding='UTF-8')

library(ggplot2)
library(rmmseg4j)
library(tm)
library(MASS)
library(proxy)

#去除空白行
text <- text[text!='']

# 找出每一章节的头部行数和尾部行数
chapbegin <- grep('第.{1,3}回 ',text)
chapend <- c((chapbegin-1)[-1],length(text))

# 计算出每一回的段落数，似乎在后四十回段落数较小
paragraph <- chapend-chapbegin
plotdata1 <- data.frame(freq=paragraph,cha=1:120)       
p1 <- ggplot(data=plotdata1,aes(x=cha,y=freq))
p1 + geom_bar(stat="identity",fill='black',colour='white')+
    geom_vline(x=80,colour='red4',size=1,linetype=2)


# 检验两部分的段落数是否有显著差异
wilcox.test(paragraph[1:80],paragraph[81:120])
library(coin) #置换检验
testdata <- data.frame(freq=paragraph,cha=factor(rep(1:2,times=c(80,40))))
oneway_test(freq~cha,data=testdata,distribution='exact')

# 将整个文本分成120个段落，每个段落为一个章节
chaptext <- character(120)
for ( i in 1:120) {
  temp <- text[chapbegin[i]:chapend[i]]
  chaptext[i] <- paste(temp,collapse='')
}
# # 计算每个章节的字数
parachar <- nchar(chaptext)

# 建立一个搜索函数，可以返回出现的频次
findfun <- function(name) {
   gregout <- gregexpr(name,chaptext) 
   namefun <- function(x) { #x表示章节序号
     temp <- attr(gregout[[x]],'match.length')
     ifelse(temp[1]>0,length(temp),0)
   }
   sapply(1:120,FUN=namefun)
}


x1 <- findfun('宝玉')
x2 <- findfun('黛玉')
x3 <- findfun('宝钗')
x <- c(x1,x2,x3)
people <- factor(rep(1:3,each=120),
                 labels=c('宝玉','黛玉','宝钗'))
plotdata4 <- data.frame(freq=x,people=people,cha=1:120)
                
p4 <- ggplot(data=plotdata4,aes(x=cha,y=freq,fill=people,colour=people))
p4 + geom_bar(stat="identity",position='fill')

# 搜索标点的出现次数，标点的次数意味着句子数量的多少。
x1 <- findfun('？')
x2 <- findfun('。')
x3 <- findfun('！')


biaodian <- x1+x2+x3

#建立一个数据框，其中有段落数、句子数和字数
# 绘制每章节中句子的数量
plotdata2 <- data.frame(paragraph,biaodian,parachar,cha=1:120)       
p2 <- ggplot(data=plotdata2,aes(x=cha,y=biaodian))
p2 + geom_bar(stat="identity",fill='black',colour='white')+
    geom_vline(x=80,colour='red4',size=1,linetype=2)

# 绘制每章节中每段句子的数量
p3 <- ggplot(data=plotdata2,aes(x=cha,y=biaodian/paragraph))
p3 + geom_point()+stat_smooth(method='loess',se=F,span = 0.2)+
    geom_vline(x=80,colour='red4',size=1,linetype=2)




# x <- c(x1,x2,x3,x4,x5,x6,x7)
# people <- factor(rep(1:7,each=120),
#                  labels=c('逗号','句号','冒号','问号','顿号','引号','感叹号'))
# plotdata2 <- data.frame(freq=x,people=people,cha=1:120)
# p <- ggplot(data=plotdata2,aes(x=cha,y=freq,fill=people,colour=people))
# p + geom_bar(stat="identity",position='stack')



# 中文分词
words <- unlist(lapply(X=chaptext,FUN=mmseg4j))

# 组成语料库格式
wordcorpus <- Corpus(VectorSource(words))
# 为什么这个词频很奇怪，只有三个字以上的词出现？
# 使用Tf-idf算法计算词频矩阵
dtm <- DocumentTermMatrix(wordcorpus,
                          control = list(weighting = weightTfIdf))


# 
# dtm2 <- removeSparseTerms(dtm, sparse=0.95)
# data <- as.data.frame(inspect(dtm2))

# 计算文档间的距离
chapdist <- dissimilarity(dtm, method = "cosine")
# 降到二维画图
mds=isoMDS(chapdist,k=2)
x = mds$points[,1]
y = mds$points[,2]
cha=factor(rep(1:2,times=c(80,40)),labels=c('前八十回','后四十回'))
plotdata3 <- data.frame(x,y,cha)
p=ggplot(plotdata3,aes(x,y))
p+geom_point(aes(colour=cha))

# word <- strsplit(word,'') #用单个字来作特征
