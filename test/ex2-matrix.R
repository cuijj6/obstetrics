#y <- matrix(c(1,2,3,4),nrow = 2,ncol = 2)
y <- matrix(c(1,2,3,4),nrow = 2)
#y[,2]
y


y <- matrix(nrow = 2,ncol = 2)
y[1, 1] <- 1
y[2, 1] <- 2
y[1, 2] <- 3
y[2, 2] <- 4
y


m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE) #按行排列
n <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = FALSE) #按列排列
n
n[-2, ] #剔除第二行


y %*% y # mathematical matrix multiplication
m %*% n #
y + y
y * 2


x <- matrix(nrow = 3, ncol = 3)
y <- matrix(c(4, 5, 2, 3), nrow = 2)
y
x[2:3, 2:3] <- y
x


x <- c(1,5,22,67)
x <- c(x[1:3], 168, x[4]) #insert 168 before 67
x
length(x)


first1 <- function(x){
    for (i in length(x)) {
         if (x[i]==1) break
    }
    return(i)
}


for (n in x)
x <- c()
x
length(x)
1:length(x)

m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow=TRUE)
n <- m + c(1, 2)
n
m + 10:13

y[1]
y[2:3]
y <- vector(length = 3)
y[2:3]
x <- c(5, 2, 1)
x

x <- c(1, 2, 4, 1, 2) + c(6, 0, 9, 20, 22)
x




