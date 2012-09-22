# XML包的使用和网页数据抓取
# http://xccds1977.blogspot.com/2012/09/blog-post.html

library(XML)
xml.url <- "http://www.w3schools.com/xml/plant_catalog.xml"
# 解析xml页面
xmlfile <- xmlTreeParse(xml.url)
# 观察对象属性
class(xmlfile)
# 获取根结点
xmltop <- xmlRoot(xmlfile)
# 用xmlValue函数获取叶结点处的值
xmlValue(xmltop[[1]][[1]])
xmlValue(xmltop[['PLANT']][['COMMON']])
# xmlSApply类似于sapply函数，取出第一个子结点中的所有叶结点值
xmlSApply(xmltop[[1]],xmlValue)
# 进一步可以取出所有子结点中的叶结点值
plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
# 将数据转为数据框
plantcat_df <- data.frame(t(plantcat),row.names=NULL)
plantcat_df[1:5,1:4]


# 豆瓣的例子
url <- 'http://api.douban.com/movie/subject/imdb/tt0365748'
doc <- xmlTreeParse(url,useInternal=TRUE) 
top <- xmlRoot(doc)
xmlName(top)

# 找出合乎条件的参数节点
(node <- getNodeSet(top,"//db:attribute[@name='writer']"))
xmlValue(node[[1]])
lapply(node, function(x) xmlSApply(x, xmlValue))
xpathApply(top,  "//db:tag", xmlGetAttr,'name')

xmlValue(top[['summary']])

htmlParse(doc, asText = TRUE)
