# 邱同学的宋词函数，拿来研究了一下

rm(list=ls())

txt=read.csv("SongPoem.csv",colClasses="character");

sentences=strsplit(txt$Sentence,"，|。|！|？|、");
sentences=unlist(sentences);
sentences=sentences[sentences!=""];
s.len=nchar(sentences);

sentences=sentences[s.len<=15];
s.len=nchar(sentences);

splitwords=function(x,x.len) substring(x,1:(x.len-1),2:x.len);

words=mapply(splitwords,sentences,s.len,SIMPLIFY=TRUE,USE.NAMES=FALSE);
words=unlist(words);
words.freq=table(words);
words.freq=sort(words.freq,decreasing=TRUE);
words.freq[1:100];

freq <- unclass(words.freq[-1])
word.frame <- data.frame(word=row.names(freq),
                         freq=freq,stringsAsFactors=F)
row.names(word.frame) <- 1:dim(word.frame)[1]

topword <- word.frame[1:200,]
rword <- sample(x=topword$word,size=12,
                replace=F,prob=topword$freq/200)

paste(rword)