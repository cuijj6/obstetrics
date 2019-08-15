dcast2 <- function (d, by, value.var = "value", ...)
{
    vars_left <- setdiff(colnames(d), c(by, value.var)) %>% paste(collapse = "+")
    vars_right <- by %>% paste(collapse = "+")
    formula <- as.formula(sprintf("%s~%s", vars_left, vars_right))
    dcast(d, formula, value.var = value.var, ...)
}

varnames <- c("血红蛋白", "血红蛋白A1c(HBA1c)", "白细胞",
              "OGTT空腹血糖", "OGTT血糖 1 h", "OGTT血糖 2 h", "总胆固醇CHOL", "甘油三脂TG", "高密度胆固醇HDL-c", "低密度胆固醇LDL-c",  # [5]
              "空腹胰岛素", "胰岛素 1 h", "胰岛素 2 h")

## FUNCTIONS
rename_items <- function(d){
    d$item %<>% factor(varnames)
    d$date_examine %<>% substr(1, 10)
    d <- unique(d)
    d$value %<>% as.numeric()

    mat <- dcast2(d, "item", "value", fun.aggregate = mean, na.rm = TRUE)
    mat
    # x <- mat[, 5:12]
    # I_bad <- apply(x > 1, 1, any) %>% which()
}
