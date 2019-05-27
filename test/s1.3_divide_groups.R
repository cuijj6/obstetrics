source('test/main_pkgs.R')

lst <- read_xlsx2list("INPUT/中大附一GDM复发.xlsx")

## 1. previous
df <- lst$previous

judge_diabete <- function(df){
    ill_diabete <- c("妊娠期发生的糖尿病", "糖耐量异常", "妊娠期并发糖尿病", "妊娠期糖尿病",
                      "妊娠伴糖尿病", "葡萄糖耐量试验异常",
                     "妊娠期并发糖尿病")
    ill <- df$out_judge %>%  str_extract("[\u4e00-\u9fa50-9]*糖[\u4e00-\u9fa50-9]*")
    info <- table(ill) %>% as.data.frame() %>% data.table()

    I_diabete <- match2(ill, ill_diabete)$I_x

    df$is_diabete <- 0
    df$is_diabete[I_diabete] <- 1
    listk(df, info)
}

## 2. Current
# df <- lst$current
res <- map(lst, judge_diabete) %>% purrr::transpose()
lst2 <- res$df
# write_list2xlsx(res$df, "INPUT/中大附一GDM复发2.xlsx")

per_diabeta <- lst2 %>% map(~sum(.$is_diabete/nrow(.)))

d <- data.table(x = lst2$previous$is_diabete, y = lst2$current$is_diabete)
table(d)
