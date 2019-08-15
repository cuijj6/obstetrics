source('test/main_pkgs.R')
library(Ipaper)

df1  <- read.xlsx("INPUT/中大附一GDM复发-20190810.xlsx") %>%
    data.table()
df2  <- read.xlsx("INPUT/d_检查指标.xlsx") %>%
    data.table()

# print(df1)
# print(df2)
merge(df1, df2, by="id")
info <- match2(df1$id, df2$id)
print(info)

res <- cbind(df1[info$I_x, ], df2[info$I_y, ])
write.xlsx(res,"cuijj6alldata.xlsx")


