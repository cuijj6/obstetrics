#假设LMP:2018-12-30
id <- c(1, 1, 2)
date_mid <- c("2019-03-02", "2019-05-13", "2019-07-31")
#date_mid <- c("2019-03-02", "2019-05-13", "2019-07-31")
d1 <- data.frame(id, date_mid)
d1


date_early <- c("2019-01-01", "2019-02-01", "2019-03-01")
id <- c(1, 1, 1)
d2 <- data.frame(id, date_early)


d2

merge(d1, d2)

merge(d1, d2, by="id")
info <- match2(d1$id, d2$id)
print(info)
res <- cbind(d1[info$I_x, ],d2[info$I_y, ])
index <- c(2,3,4,5, 6,8,10)
x[index]
