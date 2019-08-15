source('test/main_pkgs.R')
library(Ipaper)
library(ropls)

# Dongdong Kong, 2019-08-15
# -------------------------
# 1. Previous数据无用，无法在ALL中匹配上
# 2. Current与ALL完全匹配
headcols <- . %>% .[, 1:10]

df_exam  <- read.xlsx("INPUT/d_检查指标.xlsx") %>% data.table()
lst  <- read_xlsx2list("INPUT/中大附一GDM复发-20190810.xlsx") %>% map(data.table)
# d_part <- df_all[which(ill_diabete == "妊娠期并发糖尿病"), ]
# df2  <- read.xlsx("INPUT/d_检查指标.xlsx") %>%
#     data.table()
# df_all <- lst$ALL

#' two group: 1: diat; 0: non-diat
group_diab <- function(d){
    ill_diabete <- d$out_judge %>% str_extract("[\u4e00-\u9fa50-9]*糖尿病")
    info_diab <- table(ill_diabete) %>% as.data.table()

    ill_diab_sel <- c("妊娠期发生的糖尿病", "妊娠期糖尿病")
    d$is_diab <- (ill_diabete %in% ill_diab_sel) %>% as.numeric()
    d
}

yinyang <- function(str, to.num = FALSE){
    levels <- c("阴性(-)", "弱阳性(+/-)", "阳性(+)", "阳性(1+)", "阳性(2+)",  "阳性(3+)",  "阳性(4+)")
    str %<>% gsub(" ", "", .) %>%
        gsub("―", "-", .) %>%
        gsub("弱阳\\(\\+/-\\)", "弱阳性(\\+/-)", .)
    ans <- factor(str, levels)
    if (to.num) ans <- as.numeric(ans) - 1
    return(ans)
}


df_current <- lst$current[, .(id_full, age, weight_before, weight, height, out_judge_diabetes)] %>% group_diab
{
    df <- merge(df_exam, df_current, c("id_full"))
    df[,  `:=`(y = as.factor(is_diab),
               bmi        = weight/(height/100)^2,
               bmi_before = weight_before/(height/100)^2)]
    df <- df[, -(1:12)] %>% reorder_name(c("is_diab", "bmi", "bmi_before"))

    d <- df[, .(y, bmi, bmi_before,
                `总胆固醇CHOL.x`, `甘油三酯TG.x`, `高密度胆固醇HDL-c.x`, `低密度胆固醇LDL-c.x`,
                `尿糖(干化学)`, `尿蛋白(干化学)`, `葡萄糖GLU` = as.numeric(`葡萄糖GLU`))]
    # d[, 5:7] %<>% map(table)
    d$`尿糖(干化学)` %<>% yinyang(to.num = TRUE)
    d$`尿蛋白(干化学)` %<>% yinyang(to.num = TRUE)

    # d <- df[, .(y = as.factor(`is_diab`), bmi, bmi_before,
    #             `OGTT空腹血糖.x`, `OGTT血糖.1.h.x`, `OGTT血糖.2.h.x`)]
    d
}
# d[, y2 := as.numeric(`OGTT空腹血糖.x` >= 5.1 | `OGTT血糖.1.h.x` >= 10 | `OGTT血糖.2.h.x` >= 8.5)]

m <- opls(d[, -1], d[, y], predI = 1, orthoI = NA)
tbl <- table(fit = fitted(m), ref = d[, y])
precision(tbl)

# recall: 预测正确的样本数
# precision:

info <- map(lst[2:3], group_diab) %>% map("is_diab") %>% as.data.table()
tbl  <- table(info)

# pvalue < 0.05, 则x-y相互非独立
r <- chisq.test(tbl) # "spearman


## 1. --------------------------------------------------------------------------


match2(df2$id_full, lst$current$id_full)

