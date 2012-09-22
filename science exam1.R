#Introduction to Scientific Programming and 
# Simulation Using R这本书上第三章的一个练习
rm(list=ls())
sim1 <- function() { #模拟此游戏的主函数
    x <- sum(ceiling(6*runif(2))) #模拟第一次扔骰子
    if (x==7|x==11) {
        profit <- 1 #win
    } else {
        record <<- x #未赢则记录上一次的点数
        profit <- sim2() #并进入第一次未赢后的模拟函数
    }
    return(profit)
}

        
sim2 <- function(){  #第一次未赢后的模拟函数
           x <- sum(ceiling(6*runif(2))) #模拟第二次骰子
            if (x==record) { # 如果第二次点数与第一次一致
                profit <- 1 # win
            } else if (x==7|x==11) { #第二次扔出7or11
                profit <- -1 #loss
            } else {  #其它情况则进入迭代
                profit <- sim2()
            }
            return(profit)
}

#将模拟函数运行1000次观察其输赢比例
table(replicate(1000,sim1()))
 