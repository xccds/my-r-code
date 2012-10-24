install.packages('twitteR')
library(twitteR)
library(ggplot2)


# 搜索某个关键词的推文（关于moyan的发推用户和内容） --------------------------------------------------------------

# 观察哪些用户使用了这个关键词
moyanTweets <- searchTwitter('#moyan', n = 1000)
moyandf <- twListToDF(rdmTweets)
#下一步提取推文用户名并制成频数表
counts <- table(moyandf$screenName)
cc <- data.frame(subset(counts, counts>5))
names(cc) <- 'value'
cc <- data.frame(value = cc$value,name = rownames(cc))
newname <- with(cc, reorder(name, value))
data <- data.frame(cc, newname)
#载入ggplot2包，绘制条形图。
p <- ggplot(data,aes(y=value,x=newname))
p+geom_bar(stat='identity',aes(fill=value))+coord_flip()

ggsave('1.png')
# simopieranni是驻北京的一位意大利记者 SCMP_news南华早报

# 用词云研究推文中的内容
library(tm)
library(wordcloud)
#为了回避一些推文中的网址等符号，用gsub加以去除
moyantext <- moyandf$text
pattern <- "http[^ ]+|RT |@[^ ]+"
text <- gsub(pattern, "", moyantext)
# 再用tm包建立文本库和词频矩阵
tw.corpus <- Corpus(VectorSource(text))
tw.corpus <- tm_map(tw.corpus, stripWhitespace)
tw.corpus <- tm_map(tw.corpus, removePunctuation)
tw.corpus <- tm_map(tw.corpus,tolower)
tw.corpus <- tm_map(tw.corpus,removeWords,stopwords('english'))

doc.matrix <- TermDocumentMatrix(tw.corpus,control = list(minWordLength = 1))
dm <- as.matrix(doc.matrix)
v <- sort(rowSums(dm),decreasing=T)
d <- data.frame(word=names(v),freq=v)

#去除moyan和nobel这两个词后，生成最后的词云
worddata <- d[3:50,]
worddata$word <- factor(worddata$word)
mycolors <- colorRampPalette(c("white","red4"))(200)
wc <-wordcloud(worddata$word,worddata$freq,min.freq=13,colors=mycolors[100:200])

# 分析这些推文中的情绪
#从这个地址将包含正面和负面情绪词汇的文本包下载到本地 http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
pos <- scan('positive-words.txt',what='character',comment.char=';')
neg <- scan('negative-words.txt',what='character',comment.char=';')

# score.sentiment 函数
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
        
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        # and convert to lower case:
        sentence = tolower(sentence)
        
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
        
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
        
        return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}
#最后利用score.sentiment函数将推文与情绪文本进行比对，结果存于result变量中，其中score保存着各推文出现代表不同情绪的词频数。score为正表示正面情绪，为负表示负面情绪。
result <- score.sentiment(text,pos,neg)
sentscore <- result$score
scoretab <- as.data.frame(table(factor(sentscore)))

p <- ggplot(scoretab,aes(y=Freq,x=Var1))
p+geom_bar(stat='identity',aes(fill=Freq))+
    labs(x='推文情绪值',y='频数')+
    theme(legend.position='top')
ggsave('6.png')


# 分析某个用户的发推内容，提取数据并统计（PM2.5） ----------------------------------------------

library(plyr)
# 抓取北京和上海空气数据的推文
airb <- userTimeline("beijingair", n=1000)
airs <- userTimeline("CGShanghaiAir", n=1000)

#提取文本后用正则表达式分割
pattern  <- '; | \\(|\\)'
extract <- function(x) {
    strsplit(x$text,pattern)
}
textb <- sapply(airb,extract)
texts <- sapply(airs,extract)

#转成数据框格式
datab <-ldply(textb,.fun=function(x) as.data.frame(t(x),stringsAsFactors=F))
datab <- datab[,c(1,4,5)]
datas <-ldply(texts,.fun=function(x) as.data.frame(t(x),stringsAsFactors=F))
datas <- datas[,c(1,4,5)]

# 合并数据，并转换AQI为数值
data <- rbind(datab,datas)
names(data) <- c('time','AQI','type')
data$AQI <- as.numeric(data$AQI)

# 加入城市变量
city <- factor(rep(1:2,each=1000),labels = c('北京','上海'))
data$city <- city

# 加入星期变量
time.date <-as.Date(data$time,"%m-%d-%Y")
data$week = as.POSIXlt(time.date)$wday

# 加入钟点变量
data$clock <- factor(substr(data$time,start=12,stop=13))


# 小提琴图观察不同城市的空气质量
p1 <- ggplot(data,aes(city,AQI,fill=city))
p1 + geom_violin(alpha=0.3,width=0.3) +
    geom_jitter(alpha=0.3,shape=21) +
    geom_hline(y=c(100,200,300),color='grey20',linetype=2)+
    theme(legend.position='none')
ggsave('3.png')

# 上海和北京AQI的分布比较?
p <- ggplot(data,aes(x=AQI,group=city))
p + geom_histogram(aes(fill=city,y=..density..),
                   alpha=0.5,color='black')+
     stat_density(geom='line',position='identity',
                   size=0.9,alpha=0.5,color='black')+
      scale_fill_brewer(palette='Set3')+
      facet_wrap(~city,ncol=1)+
      theme(legend.position='none')
ggsave('4.png')


# 观察一周的不同日子空气质量中位数
# aggregate(data$AQI,list(data$week),mean,na.rm=T)
p3 <- ggplot(data,aes(factor(week),AQI,colour=city,group=city))
p3 +stat_summary(fun.y = median, geom='line',size=1.2,aes(linetype=city))+
    stat_summary(fun.y = median, geom='point',size=4,aes(shape=city))+
    coord_cartesian(xlim=c(1,7)) +
    theme_bw() +
    theme(legend.position=c(0.9,0.9))+
    labs(x='星期', y='AQI')
ggsave('5.png')

# 观察不同时点空气质量中位数
p2 <- ggplot(data,aes(clock,AQI,colour=city,group=city))
p2 +stat_summary(fun.y = median, geom='line',size=1.2) +
    stat_summary(fun.y = median, geom='point',size=3)+
    coord_cartesian(xlim=c(3,26))


# 对某个用户的研究 ----------------------------------------------------------------
#获取用户信息
myid <- getUser('xccds')
myid$name
myid$lastStatus
myid$description
myid$statusesCount
myid$followersCount
myid$friendsCount
myid$created

# 研究某人lady.gaga的粉丝特点 ---------------------------------------------------------------


ladygaga <- getUser('ladygaga')
follow.lady <- ladygaga$getFollowers(n=5000)
df.lady <- do.call('rbind',lapply(follow.lady,as.data.frame))
p <- ggplot(data=df.lady,aes(x=friendsCount,y=followersCount))
p + geom_point(aes(size=statusesCount),color='red4',alpha=0.5)+
    scale_x_log10(breaks=c(10,100,1000,5000))+
    scale_y_log10(breaks=c(10,100,1000,1000,20000))+
    scale_size('发推数',range=c(1,12))+
    labs(x='关注对象',y='粉丝数')+
    theme_bw()+
    theme(legend.position=c(0.9,0.2))
ggsave('8.png')

df.lady$time <- as.Date(df.lady$created)
df.lady$ntime <- as.numeric(df.lady$time)

library(mgcv)
model <- gam(followersCount~s(friendsCount)+s(statusesCount),data=df.lady)
par(mfrow=c(1,2))
plot(model,se=T)

# 研究推特趋势 ------------------------------------------------------------------

yesterdayTrends <- getTrends('daily', date=as.character(Sys.Date()-1))
yesterdayTrends


# 研究上推设备 ------------------------------------------------------------------

sources <- sapply(moyanTweets, function(x) x$getStatusSource())
sources <- str_extract(sources, "&gt;.+&lt")
sources <- str_replace_all(sources, "&gt;|&lt", "")
sources <- str_replace(sources,'BlackBerry.+','Blackberry')

counts <- as.data.frame(table(sources))
counts <- subset(counts,Freq>20)
counts$sources <- factor(counts$sources)
#载入ggplot2包，绘制条形图。
p <- ggplot(counts,aes(y=Freq,x=sources))

p + geom_bar(aes(fill=Freq),color='black',stat='identity') +
    scale_x_discrete(limits = levels(counts$sources)[order(counts$Freq)])+
    geom_text(aes(y=Freq+40,label=paste(Freq/10,'%',sep='')))+
    coord_flip() + theme(legend.position='none')+
    labs(y='频数',x='设备')
ggsave('7.png')

# 收集xccds的一千条推文
library(twitteR)
xccds <- userTimeline("xccds", n=1000)
xccds[[1]]$getCreated()

extracttime <- function(x) {
    return(x$getCreated())
}
xccds.time <- lapply(xccds,extracttime)


library(lubridate)
# 转成本时区
timefunc <- function(x) {
    return(with_tz(x,tzone='asia/shanghai'))
}
xtime <- ldply(xccds.time, .fun=timefunc)
xtime$hour <- factor(hour(xtime$V1))
xtime$week <- factor(wday(xtime$V1))
xtimedf <- as.data.frame(table(xtime$hour))

p <- ggplot(xtimedf,aes(x=Var1,y=Freq))
p+geom_bar(stat='identity',aes(fill=Freq))+
    theme(legend.position='none')+
    labs(x='时刻',y='频数')
ggsave('10.png')

