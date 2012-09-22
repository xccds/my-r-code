rm(list=ls())
neighbours <- function(A,i,j,n) {
    left <- ifelse(j ==  1,n,j-1)
    right <- ifelse(j == n, 1, j+1)
    up <- ifelse(i == 1, n, i-1)
    down <- ifelse(i == n, 1, i+1)
        nbrs <- sum(A[up,left] == 1,A[up,right] == 1,A[up,j] == 1,A[i,left] == 1,
                A[i,right] == 1,A[down,left] == 1,A[down,right] == 1,A[down,j] == 1)
    return(nbrs)
    }

n <- 50
A <- matrix(round(runif(n^2)),n,n)
record <- rep(0,1000)
finished <- FALSE

while (!finished) {
    plot(c(1,n),c(1,n),type='n',xlab='',ylab='')
    for(i in 1:n) {
        for (j in 1:n) {
            if (A[i,j]==1) {
                points(i,j,pch=16,col='red')
            }
        }
    }
    
    B <- A
    for (i in 1:n) {
        for (j in 1:n) {
            nbrs <- neighbours(A,i,j,n)
            if (A[i,j]==1) {
                if ((nbrs ==2) | (nbrs==3)){
                    B[i,j] <- 1
                } else {
                    B[i,j] <- 0
                }
            } else {
                if (nbrs ==3) {
                B[i,j] <- 1
                } else {
                    B[i,j] <- 0
                }
            }
        }
}

A <- B


# input <- readline('stop?')
# if (input =='y') finished <- T
}          

        
