extract_period <- function(x){
    G <- str_extract(x, "(?<=孕|G)\\d{1}") %>% as.numeric()
    P <- str_extract(x, "(?<=产|P)\\d{1}") %>% as.numeric()

    period <- str_extract(x, "(?<=妊娠)\\d{1,2}\\+?\\d(?=周)")
    week <- str_extract(period, "^\\d{1,2}") %>% as.numeric()
    day  <- str_extract(period, "(?<=\\+)\\d{1,2}$") %>% as.numeric()

    method <- str_extract(x, "顺产|剖宫产")
    data.table(P, G, week, day, method)
}

extract_BMI <- function(x){
    # x <- d$体格检查[106]
    x %<>% gsub("：|:| ", "", .)
    pattern_yunqian <- "孕前体重|孕前体|孕前|前体重|孕前体重Kg"
    weight_before <- str_extract(x, "(?<=孕前体重|孕前体|孕前|前体重|孕前体重Kg)\\d{2,3}(\\.\\d{1,})?") %>% as.numeric()
    weight        <- str_extract(x, "(?<!孕前体重|孕前体|孕前|前体重|孕前体重Kg)(?<=体重|体重值|体)\\d{2,3}(\\.\\d{1,})?") %>% as.numeric()
    height        <- str_extract(x, "(?<=身高|身长|身)\\d{1,3}(\\.\\d{1,})?") %>% as.numeric()
    height[which(height < 3)] %<>% multiply_by(100)

    # BMI <- str_extract(x, "(?<=BMI)\\d{2,3}(\\.\\d{1,})?(?==Kg|Kg)") %>% as.numeric()
    data.table(weight_before, weight, height)
}

# 既往史
extract_ill <- function(x){
    his <- str_extract(x, "[\u4e00-\u9fa50-9]*糖尿病")
    I_not <- grep("否|无", his)
    his[I_not] <- "-"
    his
}

#' extractBasicInfo
#'
#' @export
extractBasicInfo <- function(d){
    # 出院诊断
    out_mat <- d[, .SD, .SDcols = grep("出院诊断", colnames(d))] %>% as.matrix()
    out_mat[is.na(out_mat)] <- ""
    out_judge <- apply(out_mat, 1, function(x){ paste0(x, collapse = "; ") })

    his_diabetes <- d$既往史 %>% extract_ill()
    out_judge_diabetes <- out_judge %>% extract_ill()

    info_period <- d$孕次产次孕周 %>% extract_period()
    info_BMI    <- d$体格检查 %>% extract_BMI()
    d$年龄 %<>% gsub("^(Y|y)", "", .) %>% as.numeric()
    
    names <- names(d)
    headvars <- c("住院号", "上次住院号", "本次住院号") %>% intersect(names)

    info <- cbind(d[, .SD, .SDcols = c(headvars, "患者姓名", "出生日期", "年龄", "分娩日期",
                   "婴儿1性别", "婴儿1体重", "婴儿1呼吸", "婴儿1转归",
                   "婴儿2性别", "婴儿2体重", "婴儿2呼吸", "婴儿2转归")],
             info_period, info_BMI, out_judge, out_judge_diabetes, his_diabetes)
    info
}

# summary(info_BMI)
# info_BMI[, which(is.na(weight))]

# na_num <- as.matrix(info_BMI) %>% is.na() %>% rowSums(.)
# I_bad <- which(na_num >= 1 & na_num <= 2)
# xs <- d$体格检查[I_bad] %>% gsub(" ", "", .)

# info_bad <- info_BMI[I_bad, ] %>% cbind(I_bad, ., xs)
# file.show("check.csv")
