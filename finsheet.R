# 如何来抓财务报表数据

library(quantmod)
# 财务报表分析
setSymbolLookup(myname=list(name="601857.SS", src="yahoo"))
getSymbols("601857.SS")


getFin('MSFT') # returns AAPL.f to "env"
test <-viewFin(paste('MSFT','f',sep='.'), "IS", "Q") # Quarterly Income Statement
viewFin(MSFT.f, "IS", "Q") # Annual Cash Flows
str(AAPL.f)

names(MSFT.f)
MSFT.f$IS
names(MSFT.f$IS)
MSFT.f$IS$A
str(MSFT.f$IS$A)
class(MSFT.f$IS$A)
# 最近的年报，其中利润表的净利润
MSFT.f$IS$A['Net Income','2011-06-30']

# 中国股票的代码表
http://www.txsec.com/inc1/gpdm.asp

# 纽约市场的代码表
 读symbol http://www.findata.co.nz/markets/NYSE/symbols/A.htm
# http://www.nyse.com/about/listed/lc_ny_industry_2.html?ListedComp=All&supersector=13&sector=106

聚类问题
根据2011.12的财务指标，能否预测未来的股票价格走势？股票的收益率和方差，这是两个因变量，财务指标做为自变量，分类问题

getFin('AZO')
viewFin(AZO.f, "IS", "A") # Quarterly Income Statement
AZO.f$IS
names(MSFT.f$IS)
MSFT.f$IS$A
str(MSFT.f$IS$A)
class(MSFT.f$IS$A)
# 最近的年报，其中利润表的净利润
AZO.f$IS$A
dim(AZO.f$IS$A)
colnames(AZO.f$IS$A)[1]

time <-as.character (colnames(AZO.f$IS$A)[1]) # 最近年报公布时间
AZO.f$IS$A['Net Income',time] # 获得年报的利润表中的最近一期净利润
AZO.f$CF$A[,time] # 获得年报的现金流量表
rownames(AZO.f$IS$A)

# 下面的有希望用，但是速度太慢了，还是要学python啊
name <- NULL
# 生成代码表字符串
library(XML)
abc <- LETTERS[seq(from = 1, to = 26 )]
for ( i in abc) {
    url <- paste('http://www.findata.co.nz/markets/NYSE/symbols/',i,'.htm',sep='')
    tables <- readHTMLTable(url,stringsAsFactors = FALSE)
    raw <- tables[[2]]
    stockname <- raw[,1]
    name <- c(name,stockname)
}

name <- name[!(grepl('[-.]',name))]

# 直接去抓 google.finance的利润表的函数
getprofit <- function(name) {
    web <- paste('http://www.google.com/finance?q=NYSE:',name,'&fstype=ii',sep='')
    doc <- htmlParse(web,encoding='UTF-8')
    tables <- readHTMLTable(doc,stringsAsFactors = FALSE)
    if (length(tables)==1|| is.null(tables[[3]])) {
      revenue <- 'f'
    } else {
      revenue <- tables[[3]][18,2]}
    return(revenue) # 提取净利润,字符串形式，要将,改过来
}

# profit <- sapply(name,getprofit)

# 循环得到A字头公司利润
temp <- rep(0,length(name))
for (i in 1:length(name)) {
  temp[i] <- getprofit(name[i])
}

name <- scan('d:/stockname.txt',what='character',skip=1)
profit <- scan('d:/profit.txt',what='character',skip=1)

profit[profit=='f'] <-  NA
profit <- as.numeric(gsub(',','',profit))
class(profit)
head(profit)
data <- data.frame(name,profit)
data[order(profit,decreasing=T),][1:10,]

# 找到这支股票的行业
url <- 'http://www.google.com/finance?q=NYSE:CHL'
doc <- readLines(url,encoding='UTF-8')
lnumber <- doc[grep('Industry',doc)]

指标选择
中美两市的一维指标比较
聚类比较
分类（报表公布后一月内的股票收益率和波动情况）