# 用R来处理图片
# http://xccds1977.blogspot.com/2012/06/r_27.html

library(fpc)
library(jpeg) # 可以读而且可以写文件
library(biOps) #有许多图形处理方法
library(ReadImages)  #ggplot2兼容
library(bigmemory)

#读取图像文件，观察到此图像是399*399像素的rgb图片
x <-  readJpeg("d:\\xccds.jpg")
print(x)
# 先尝试缩放操作，后面的参数是缩放的比例，可以采用四种方式进行插值，这里用的是最近邻法
plot(imgScale(x,1.3,1.3,interpolation='nearestneighbor'))
#之后进行柔化降噪，此处采用的是中位数滤镜
plot(imgBlockMedianFilter(x,5))
# 勾勒边缘
plot(imgCanny(x,0.7))
plot(x)
# 很方便进行图片裁剪，先设定好左上角的坐标像素，再设定好图片的宽度和高度即可
plot(imgCrop(x,0,0,399,200))
# 修改后的结果可以存到本地磁盘中
y <- imgCrop(x,0,0,399,200)
writeJpeg(filename='d:\\test.jpg',imgdata=y)
# 另外一些有用的处理函数
# 图像聚类
plot(imgKMeans(x,8))
# 翻转图像
plot(imgHorizontalMirroring(x))
# 负片效果
plot(imgNegative(x))
# 转灰度效果
plot(imgRGB2Grey(x))
# 旋转图像
plot(imgRotate(x,45))
# 最后来个复杂点的例子，给图片换底
# rgb图像文件本质上是一个三维数组，它是由红、绿、兰三个矩阵所构成。将这三个矩阵的数值进行修改即是修改图片的颜色
y <- unclass(x)
y1 <- y[,,1]
y2 <- y[,,2]
y3 <- y[,,3]
z <- y1>220&y2>220&y3>220 
y1[z] <- y2[z] <- y3[z]<- 77
# 修改后的矩阵再重新组合为图片格式
y <- imagedata(array(data=c(y1,y2,y3),dim=c(399,399,3)))
plot(y)



# X <- big.matrix(A,ncol=3)
# run the k-means algorithm on this data

# stat.kmean <- function(k) {
#     result <- kmeans(X,k)
#     stats <- cluster.stats(dist(X), result$cluster)
#     return(stats$avg.silwidth)
# }
# model2 <- dbscan(X,eps=0.6,MinPts=4)

X.kmeans <- kmeans(X, centers=10)
centroids <- X.kmeans$centers
idx <- X.kmeans$cluster
X.compressed <- centroids[idx,]
X.compressed <- array(X.compressed, dim=dim(A))
writeJPEG(target='d:\\compress.jpg',image=X.compressed)

par(mfrow=c(1,2))
plot(x, main="Original Image")
plot(X.compressed, main="Compressed Image")