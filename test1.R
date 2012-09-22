#一个oracle题目关于正则表达式的解答

rm(list=ls())
# 读取文档
txt <- read.delim("d:\\message_data.txt",head=F,encoding='UTF-8',colClasses="character")
# 获取文档长度
len <- dim(txt)[1]
# 确定正则表达式
pattern  <- '[ ,:.!"\\{\\}\\?\t|:\\(\\)#\\]|, |\\[|\\]'
res <- data.frame(id=0, word='',Freq=0)
# 用循环来处理每句话
for (i in 1:len) {
    word <- strsplit(txt$V2[i],pattern)
    temp <- as.data.frame(table(word))
    temp$id <- rep(txt$V1[i],length(temp$word))
    res <- rbind(res,temp)
    }
res <-res[-1,]
res2 <- res[res$word!='',]
res2$id[1:22]=1
# 输出成文本
write.table(res2,'d:\\out.txt',sep='\t',row.names=F)
