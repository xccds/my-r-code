# 用R来解euler问题
# euler 1 -----------------------------------------------------------------


# 
# 预备知识
1:10
10:1
x <- 1:10
print(x)
sum(x)
x > 5
x[x > 5]
x > 5 & x < 8
x > 8 | x < 3
10 %% 3
9 %% 3
x %% 3
x %% 3 == 0
x[x %% 3 == 0]

# Project Euler 1
# 找到1000以下，所有能被3或5整除的数，将它们相加
x <- 1:999
sum(x[x %% 3 == 0 | x %% 5 == 0 ])


# euler 2 -----------------------------------------------------------------


# By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
# 预备练习，while循环和if判断
x <- 1:10
print(x)
print(x[10])
print(x[-10])

i <- 1
while (i <= 10) {
  print(x[i])
  i <- i + 1
}

i <- 1
while (TRUE) {
  print(x[i])
  i <- i + 1
  if (i > 10) break
}
# 用循环和条件来完成前篇文章中的问题
# 可以体会到R语言向量化计算的特点。
x <- 1
sumx <- 0
while (x < 1000) {
  if (x %% 3 == 0 | x %% 5 == 0) {
    sumx <- sumx + x
    }
  x <- x + 1
}
print(sumx)

# Project Euler 2
# 找到4000000以下的斐波纳契数列
# 将其中的偶数进行求和
i <- 2
x <- 1:2
while (x[i] < 4e6) {
  x[i+1] <- x[i-1] + x[i]
  i <- i + 1
}
x <- x[-i]
sum(x[x %% 2 == 0])

# 3
# What is the largest prime factor of the number 600851475143 ?

# 预备练习，学习for循环、建立自定义函数和其它一些函数

for (n in 1:10) {
    print(sqrt(n))
}

x <- c('hello','world','I','love','R')
for (n in x) {
    print(n)
}

x <- seq(from=1,to=10,by=1)
print(x)
x <- seq(from=1,to=10,by=2)
print(x)
x <- seq(from=1,to=2,length.out=10)
print(x)
round(x) 
x > 1.5
all(x>1.5)
any(x>1.5)

# 如何自定义一个求圆面积的函数
myfunc <- function(r) {
    area <- pi*r^2
    return(area)
}
print(myfunc(4))

# 同时求四个不同半径圆的面积
r <- c(2,2,4,3)
sapply(X=r,FUN=myfunc)


# euler 3 -----------------------------------------------------------------


# 找到600851475143这个数的最大质因子
# 先建立一个函数以判断某个数是否为质数

findprime  <- function(x) {
    if (x %in% c(2,3,5,7)) return(TRUE)
    if (x%%2 == 0 | x==1) return(FALSE)
    xsqrt <- round(sqrt(x))
    xseq <- seq(from=3,to=xsqrt,by=2)
    if (all(x %% xseq !=0)) return(TRUE)
    else return(FALSE)
}
# 列出1到100的质数，看函数对不对
x = 1:100
x[sapply(x,findprime)]

# 寻找最大的质因子
n <- 600851475143
for (i in seq(from=3, to=round(sqrt(n)), by=2)) {
  if (findprime(i) & n %% i == 0) {
      n <- n / i
      prime.factor <- i       
      if (i >= n)
        break
    }
}
print(prime.factor)




# euler 4 -----------------------------------------------------------------


# 预备练习
x <- y <- 1:9
data <- expand.grid(x=x,y=y)
print(data)
z <- data$x * data$y
# 乘法表
z <- matrix(z,ncol=9)

set.seed(1)
x <- round(runif(10),2)
print(x)
order(x)
x[order(x)[1]]
which.min(x)
x[which.min(x)]
x[order(x)]

y <- 1:10
data <- data.frame(x,y)
class(data)
head(data)
data[1,]
data[,1]
data$x
data[order(data$x),]

# 
x <- y <- 999:100
data <- expand.grid(x=x,y=y)
data$prod <- data$x * data$y
data <- data[order(data$prod,decreasing=T),]
head(data)

# value = data$prod
# value = as.character(value)
# value = strsplit(value,split='')
# for (i in 1:length(value)) {
#   isequal <- (value[[i]] == rev(value[[i]]))
#   if (all(isequal)==TRUE) {
#     result = value[i]
#     break
#     }
# }
# print(data[i,])

reverse <- function(n) {
    reversed <- 0
    while (n > 0) {
        reversed <- 10 * reversed + n %% 10
        n <- n%/%10
    }
    return(reversed)
}

value <- data$prod
for (i in 1:length(value)) {
    isequal <- (value[i] == reverse(value[i]))
    if (isequal) {
        print(data[i,])
        break
    }
}


# euler 5 -----------------------------------------------------------------


# 5 What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

# 预备练习
mat <- matrix(1:12,ncol=4)
print(mat)
t(mat)
colnames(mat) <- c('one','two','three','four')
rownames(mat) <- c('a','b','c')
print(mat)
apply(mat,1,sum)
apply(mat,2,sum)
sum(apply(mat,2,sum))
prod(apply(mat,2,sum))

# 判断是否为质数的函数
findprime  <- function(x) {
  if (x %in% c(2,3,5,7)) return(TRUE)
  if (x%%2 == 0 | x==1) return(FALSE)
  xsqrt <- round(sqrt(x))
  xseq <- seq(from=3,to=xsqrt,by=2)
  if (all(x %% xseq !=0)) return(TRUE)
  else return(FALSE)
}
x = 1:20
prime <- x[sapply(x,findprime)]

# 建立分解质因子的函数
primefactor <- function(x,prime) {
  m <- length(prime)
  fac.count <- numeric(m)
  names(fac.count) <- prime
  for (i in 1:m) {
    prime.num <- prime[i]
    while (x %% prime.num == 0 & x !=1 ) {
      fac.count[i] <- fac.count[i] + 1
      x = x / prime.num
    }  
  }
  return(fac.count)
}

# 上面的函数是对一个20以下的数字将其分解为多个质数之积
# 返回每个质因子对应的幂
primefactor(18,prime)

# 对1到20每个数进行质因子分解，形成一个表格
result <- t(sapply(1:20,primefactor,prime))
# 求每列的极大值
prime.power <- apply(result,2,max)
prod(prime^prime.power)


# small.composite <- function(n) {
#   composite <- 20
#   for (i in 1:n) {
#     remain <- (composite %% i)
#     if (remain != 0) {
#       if (i %% remain == 0) {
#         composite <- composite * i / remain
#       }
#       else {
#         composite <- composite * i
#       }
#     }
#   }
#   return(composite)
# }
# cat("The result is:", small.composite(20), "\n")


# euler 6 -----------------------------------------------------------------


# 6 what is the difference between the sum of the squares and the square of the sums?
x <- 1:100
sumofsq <- sum(x^2)
sqofsum <- sum(x)^2
sqofsum-sumofsq

# What is the 10001st prime number?
n <- 0
i <- 1
m <- rep(0,10001)
while (n <10001) {
    if (findprime(i)) {
        n <- n +1 
        m[n] <- i}
    i <- i + 1
}
m[10001]

# 预备练习，熟悉一些字符串操作函数
text <- c('hello','world','I','love','code')
gsub('o',' ',text)
gsub('o','*',text)
gsub('o','',text)

(temp1 <-paste(text,collapse=' '))
paste(text,collapse='*')
paste(text,collapse='')

(temp2 <- strsplit(temp1," "))
class(temp2)
(temp3 <- unlist(strsplit(temp1," ")))
class(temp3)


# euler 8 -----------------------------------------------------------------


# 8 Find the greatest product of five consecutive digits in the 1000-digit number.
web <- 'http://projecteuler.net/problem=8'
raw <- readLines(web)
raw <- raw[54:73]
data <- gsub('<br />','',raw)

# data <- scan('clipboard',what='character')
num <- paste(data,collapse='')
temp <- as.numeric(unlist(strsplit(num,'')))

p <- numeric()
for ( i in 1:(1000-4)) {
    p[i] <- prod(temp[i:(i+4)])
}
max(p)


# euler 9 -----------------------------------------------------------------


# 9 Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.

for (a in 1:500) {
    for (b in 1:500) {
        if (2*a*b-2000*(a+b)+1000^2==0) {
        print(c(a,b,1000-a-b))
        print(a*b*(1000-a-b))
        break
        }
    }
}



# euler 10 ----------------------------------------------------------------


# 10 Find the sum of all the primes below two million.
n <- 1
sumn <- 0
while (n <2e6) {
    if (findprime(n)) {
        sumn <- sumn + n
    }
    n <- n + 1
}
print(sumn)


# 使用筛法来得到质数
PrimeSieve <- function(n) {
    if (n <= 1) {
        primes <- numeric(0)
    }
    if (n == 2 | n == 3) {
        primes <- 2:n
    }
    else {
        numbers <- 2:n
        sieve <- rep(TRUE, times = n - 1)  # let all flags to be TRUE
        cross.limit <- floor(sqrt(n))
        count <- 1   
        p <- numbers[sieve][count]  # let p be the first sieve number
        while (p <= cross.limit) {
            sieve[p * (2:floor(n / p)) - 1] <- FALSE
            count <- count + 1
            p <- numbers[sieve][count]
        }   
        primes <- numbers[sieve]
    }
    return(primes)
}
result <- sum(as.numeric(PrimeSieve(2e6)))
cat("The result is:", result, "\n")


# euler 11 ----------------------------------------------------------------


raw <- scan('clipboard')
data <- matrix(raw,ncol=20,byrow=T)
final <- matrix(ncol=4)
for (x in 1:17) {
    for ( y in 1:17) {
        submatrix <- data[x:(x+3),y:(y+3)]
        final <- rbind(final,
                       submatrix,
                       t(submatrix),
                       diag(submatrix),
                       diag(apply(submatrix,1,rev)))
    }
}
max(apply(final,1,prod),na.rm=T)

# 这个解法更为精妙
prod.vertical <- mat[1:17, ] * mat[2:18, ] * mat[3:19, ] * mat[4:20, ]
prod.horizontal <- mat[, 1:17] * mat[, 2:18] * mat[, 3:19] * mat[, 4:20]
prod.diag1 <- mat[1:17, 1:17] * mat[2:18, 2:18] * mat[3:19, 3:19] * mat[4:20, 4:20]
prod.diag2 <- mat[4:20, 1:17] * mat[3:19, 2:18] * mat[2:18, 3:19] * mat[1:17, 4:20]
result <- max(prod.vertical, prod.horizontal, prod.diag1, prod.diag2)
cat("The result is :", result, "\n")



# euler 12 ----------------------------------------------------------------


x = 1:20
triangle = x*(x+1)/2

factorf <- function(x) {
  m <- 0
  for (n in 1:round(x/2)) {
    if (x%%n==0) m <- m + 1
  }
  return(m)
}

x <- 7
y <- x*(x+1)/2
m <- factorf(y)
while (m <= 50) {
  y <- x*(x+1)/2
  m <- factorf(y)
  x <- x + 1
}
print(y)


# helper funtion for factorization  	   
PrimeFactor <- function(x, prime = prime) {
  m <- length(prime)
  fac.count <- numeric(m)
  names(fac.count) <- prime
  # actually, a primality check could insert here
  for (i in 1:m) {
    prime.num <- prime[i]
    while (x %% prime.num == 0) {
      fac.count[i] <- fac.count[i] + 1
      x = x / prime.num
    }  
    while (x == 1) break
  }
  return(fac.count)
}

prime <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 
           67, 71, 73, 79, 83, 89, 97)

# generate triangle numbers and count prime factors
i <- 1
div.count <- 0
while (div.count <= 500) {
  triangle <- i * (i + 1) / 2
  fac <- PrimeFactor(triangle, prime)
  div.count <- prod(fac + 1) 
  i <- i + 1
}
cat("The result is", i-1, "th triangle number:", triangle, "\n")



# euler 13 ----------------------------------------------------------------


# 一般方法
web <- 'http://projecteuler.net/problem=13'
raw <- readLines(web)
raw <- raw[54:153]
data <- gsub('<br />','',raw)
data <- as.numeric(data)
sumx <- sum(data)
y <- numeric()
num <- as.numeric(paste('1e',51:42,sep=''))
for ( i in num) {
    y <- c(y,sumx %/% i)
    sumx <- sumx %% i
}
print(y)

# R 的简单方法
options(digits = 10)  
digits <- scan("clipboard", what = 0)
result <- sum(digits)

# euler 14 ----------------------------------------------------------------



func <- function(x) {
  n = 1
  raw <- x
  while (x > 1) {
    x <- ifelse(x%%2==0,x/2,3*x+1)
    n = n + 1
  }
  return(c(raw,n))
}


# 用多核心来计算
library(snow)
system.time({
x <- 1:1e6
cl <- makeCluster(4,type='SOCK')
results <- clusterApply(cl,x,fun=func)
res.df <- do.call('rbind',results)
})
stopCluster(cl)
res.df[which.max(res.df[,2]),1]

837799