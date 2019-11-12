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


match2 <- function (x, y) 
{
    class_x = class(x)[1]
    class_y = class(y)[1]
    if (class_x != class_y) {
        stop("class of x and y is different!")
    } else if (is.data.frame(x)) {
        x = do.call(paste, x)
        y = do.call(paste, y)    
    }

    I <- match(x, y)
    I_x <- which.notna(I)
    I_y <- I[I_x]
    d <- data.table(x = x[I_x], y = y[I_y], I_x, I_y, grp = cumsum(c(TRUE, 
        diff(I_y) != 1)))
    d
}
