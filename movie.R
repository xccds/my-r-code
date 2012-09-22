# 在R中使用正则表达式的例子
# http://xccds1977.blogspot.com/2012/04/r_12.html

word <- c('abc noboby@stat.berkeley.edu','text with no email','first me@mything.com also you@yourspace.com')
pattern <- '[-A-Za-z0-9_.%]+@[-A-Za-z0-9_.%]+\\.[A-Za-z]+'
(gregout <- gregexpr(pattern,word))
substr(word[1],gregout[[1]],gregout[[1]]+attr(gregout[[1]],'match.length')-1)
getcontent <- function(s,g) {
    substring(s,g,g+attr(g,'match.length')-1)
}
getcontent(word[1],gregout[[1]])


url <-'http://movie.douban.com/top250?format=text'
# 获取网页原代码，以行的形式存放在web变量中
web <- readLines(url,encoding="UTF-8")
# 找到包含电影名称的行编号
name <- web[grep('<td headers="m_name">',web)+1]
# 用正则表达式来提取电影名
gregout <- gregexpr('>\\w+',name)
movie.names = 0
for (i in 1:250) {
    movie.names[i] <-getcontent(name[i],gregout[[i]])
}
movie.names <- sub('>','',movie.names)
# 找到包含电影发行年份的行编号并进行提取
year <- web[grep('<span class="year">',web)] 
movie.year <- substr(year,36,39)
# 找到包含电影评分的行编号并进行提取
score <- web[grep('<td headers="m_rating_score">',web)+1]
movie.score <- substr(score,21,23)
# 找到包含电影评价数量的行编号并进行提取
rating <- web[grep('<td headers="m_rating_num">',web)+1]
movie.rating <- sub(' *','',rating)
# 合成为数据框
movie <- data.frame(names=movie.names,year=as.numeric(movie.year),
                    score=as.numeric(movie.score),rate=as.numeric(movie.rating))
# 绘散点图
library(ggplot2)
p <- ggplot(data=movie,aes(x=year,y=score))
p+geom_point(aes(size=rate),colour='lightskyblue4',
             position="jitter",alpha=0.8)+
  geom_point(aes(x=1997,y=8.9),colour='red',size=4)

