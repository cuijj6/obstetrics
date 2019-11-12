source('test/main_pkgs.R')

## ni shijie
select_ill_items <- function(file) {
    d <- read_xlsx(file) %>% data.table()
    # names(d)[1] <- "I"
    d_ill <- get_ills(d)
    ills <- c("妊娠伴糖尿病", "妊娠期并发糖尿病", "妊娠期发生的糖尿病", "妊娠期糖尿病")
    # d_ill[out_judge_diabetes %in% ills, ]
    ind <- which(d_ill$out_judge_diabetes %in% ills)
    d <- cbind(d, d_ill)

    d$is_select = FALSE
    d$is_select[ind] <- TRUE
    r <- d[ind, .(I, id_full = `住院流水号`, id = `住院号`)]
    list(all = d, selected = r)
}

##
l1 <- select_ill_items("INPUT/条件2之前每次数据（20190314）.xlsx")
l2 <- select_ill_items("INPUT/02-01.xlsx")

## hebing
{
    varnames <- intersect(colnames(l1$all), colnames(l2$all))
    I_rm <- grep("出院诊断|既往史", varnames)
    # varnames <- varnames[-I_rm]

    info <- match2(l1$selected$id_full, l2$selected$id_full)
    ids <- l1$selected[-info$I_x, I]

    d_m1 <- l1$all[I %in% ids, ..varnames]
    df <- rbind(d_m1, l2$all[is_select == TRUE, ..varnames])

    df$分娩日期 %<>% ymd()
    df2 <- df[`分娩日期` >= ymd("2011-01-01")]
    ## 增加了18条记录
}
write_list2xlsx(df2, "第一次分娩469例糖尿病.xlsx")


# ids <- r1[-info$I_x, I]
# write_list2xlsx(list(newsupply = d1[I %in% ids, ]), "补充数据.xlsx")
