# 计算金融中的copula如何计算

library(copula)
# 二元椭圆copula对象
mycop.norm <- ellipCopula(family='normal',dim=3,
                          dispstr='ex',param=0.4)

mycop.t <- ellipCopula(family='t',dim=3,
                       dispstr='toep',param=c(0.8,0.5),df=8)
# 阿基米德copula
mycop.clayton <- archmCopula(family = "clayton", dim = 3, param = 2)

# 多元分布对象
mymvd <- mvdc(copula=mycop.t,margins=c('norm','norm','norm'),
              paramMargins=list(list(mean=0,sd=2),
                               list(mean=0,sd=1),
                               list(mean=0,sd=2)))

# pcopula dcopula rcopula pmvdc dmvdc rmvdc
# 根据T copula生成随机数
set.seed(1)
u <- rcopula(mycop.t,4)
# 根据实际数据和copula计算密度和分布
cbind(dcopula(mycop.t,u),pcopula(mycop.t,u))
# 生成随机多元分布
x <- rmvdc(mymvd,4)
# 多元密度和分布
cbind(dmvdc(mymvd,x),pmvdc(mymvd,x))

par(mfrow = c(1, 2), mar = c(2, 2, 1, 1), oma = c(1, 1, 0, 0),
    mgp = c(2, 1, 0))
u <- rcopula(mycop.norm, 200)
# 观察随机copula值
library(scatterplot3d)
scatterplot3d(u)
v <- rcopula(mycop.norm, 200)
scatterplot3d(v)

# 生成二元分布后显示contour图
myMvd1 <- mvdc(copula = archmCopula(family = "clayton", param = 2),
                margins = c("norm", "norm"), paramMargins = list(list(mean = 0,
                sd = 1), list(mean = 0, sd = 1)))
myMvd2 <- mvdc(copula = archmCopula(family = "frank", param = 5.736),
                  margins = c("norm", "norm"), paramMargins = list(list(mean = 0,
                   sd = 1), list(mean = 0, sd = 1)))
myMvd3 <- mvdc(copula = archmCopula(family = "gumbel", param = 2),
                 margins = c("norm", "norm"), paramMargins = list(list(mean = 0,
                 sd = 1), list(mean = 0, sd = 1)))
par(mfrow = c(1, 3), mar = c(2, 2, 1, 1), oma = c(1, 1, 0, 0),
        mgp = c(2, 1, 0))
contour(myMvd1, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))
contour(myMvd2, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))
contour(myMvd3, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))

persp(myMvd1, dmvdc, xlim = c(-3, 3), ylim = c(-3, 3))

# 生成数据
myMvd <- mvdc(copula = ellipCopula(family = "normal", param = 0.5),
            margins = c("gamma", "gamma"), paramMargins = list(list(shape = 2,
            scale = 1), list(shape = 3, scale = 2)))
n <- 200
dat <- rmvdc(myMvd, n)

# 拟合多元分布
mm <- apply(dat, 2, mean)
vv <- apply(dat, 2, var)
# 标准化
b1.0 <- c(mm[1]^2/vv[1], vv[1]/mm[1])
b2.0 <- c(mm[2]^2/vv[2], vv[2]/mm[2])
# 根据相关系数求omega
a.0 <- sin(cor(dat[, 1], dat[, 2], method = "kendall") * pi/2)
# 作为求解初始值
start <- c(b1.0, b2.0, a.0)
# 估计参数
fit <- fitMvdc(dat, myMvd, start = start,
      optim.control = list(trace = TRUE, maxit = 2000))



# 定义tcopula函数，生成椭圆copula对象
cop.dim3 <- tCopula(c(-.6,.75,0),dim=3,dispstr='un',df=1)
set.seed(5640)
# 随机生成tcopula分布函数
rand.cop =rcopula(cop.dim3,500)
pairs(rand.cop)
cor(rand.cop)
# 生成正态copula对象
cop_normal_dim3 = normalCopula(c(-.6,.75,0), dim = 3, dispstr = "un")
# 根据copula和边缘分布生成多元分布对象
mvdc_normal <- mvdc(cop_normal_dim3, c("exp", "exp","exp"),
                    list(list(rate=2), list(rate = 3), list(rate=4)) )
set.seed(5640)
# 从多元分布中随机抽样1000个
rand_mvdc = rmvdc(mvdc_normal,1000)
pairs(rand_mvdc)
par(mfrow=c(2,2))
plot(density(rand_mvdc[,3]))
plot(density(rand_mvdc[,1]))
plot(density(rand_mvdc[,2]))


library(Ecdat) # need for the data
library(copula) # for copula functions
library(fGarch) # need for standardized t density
library(MASS) # need for fitdistr and kde2d
library(fCopulae) # additional copula functions (pempiricalCopula
# and ellipticalCopulaFit)
data(CRSPday,package="Ecdat")
ibm = CRSPday[,5]
crsp = CRSPday[,7]
# 拟合一元T分布，得到拟合参数三个，中心，离散，自由度
est.ibm = as.numeric(fitdistr(ibm,"t")$estimate)
est.crsp = as.numeric(fitdistr(crsp,"t")$estimate)
est.ibm[2] = est.ibm[2]*sqrt(est.ibm[3]/(est.ibm[3]-2))
est.crsp[2] = est.crsp[2]*sqrt(est.crsp[3]/(est.crsp[3]-2))
# 估计相关参数
cor_tau = cor(ibm,crsp,method="kendall")
omega <- sin(cor_tau*pi/2) # 公式8.27
# 建立t copula对象
cop_t_dim2 = tCopula(omega, dim = 2, dispstr = "un", df = 4)

# 两种方法来变换原数据
n = length(ibm)
data1 = cbind(pstd(ibm,mean=est.ibm[1],sd=est.ibm[2],nu=est.ibm[3]),
              pstd(crsp,mean=est.crsp[1],sd=est.crsp[2],nu=est.crsp[3]))
data2 = cbind(rank(ibm)/(n+1), rank(crsp)/(n+1))
# 根据多元数据拟合copula函数
ft1 = fitCopula(cop_t_dim2, data=data1,
                start=c(omega,5))
ft2 = fitCopula(cop_t_dim2, method="mpl", data=data2,
                start=c(omega,5),lower=c(0,2.5),upper=c(.5,15) )