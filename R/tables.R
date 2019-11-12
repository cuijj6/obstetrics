#' basinInfo
#'
#' @param data data.table obj
#'
#' @export
basinInfo <- function(data, indicator = "age2", response = "GDM", digit = 2){
    types = c("Recurrence", "No Recurrence")
    fmt2 = sprintf("%%.%df ± %%.%df", digit, digit)
    fmt3 = sprintf("%%.%df [%%.%df, %%.%df]", digit, digit, digit)

    formula <- sprintf("%s~GDM", indicator) %>% as.formula()
    d <- data[, .SD, .SDcols = c(indicator, "GDM")] %>% set_names(c("x", "GDM"))

    ## 判断数据类型

    if ( is.numeric( d[[1]][1] ) ){
        m <- aov(formula, data = data)
        s <- summary(m)

        stat    = s[[1]][1, 4:5] %>% unlist() %>% set_names(c("statistic", "pval"))
        d_tukey <- TukeyHSD(m)$GDM
        diff    <- data.table(d_tukey)[, sprintf(fmt3, diff, lwr, upr)]

        info <- d %>% .[, as.list(stat_sd(x)), .(GDM)]
        str_mean <- info[, sprintf(fmt2, y, sd)] %>% set_names(types)

        # d_tukey[1, ][4],
        c(c(str_mean, diff = diff) %>% as.list(), c(stat) %>% as.list()) %>% 
            as.data.table() %>% cbind(type = "", .)
        # "variales"      "Recurrence"    "No Recurrence" "diff"          "F value"       "Pr(>F)"
    } else {
        tbl <- table(d)
        if (ncol(tbl) == 4) tbl = tbl[, 4:3]

        nrow <- nrow(tbl)
        tbl2 = foreach (i = 1:nrow, .combine = rbind) %do% {
            row    = tbl[i, ]
            row[3] = row[1] - row[2]
            sprintf("%d (%.1f%%)", row, row/sum(row[1:2])*100)
        } %>% set_colnames(c("Recurrence", "No Recurrence", "diff"))

        t <- chisq.test(tbl)
        statistic <- t$statistic[[1]]
        pval      <- t$p.value

        ans <- data.table(type = rownames(tbl), tbl2, statistic, pval) 

        ans[nrow, 5:6] <- NA
        ans
    }
}

tidy_his <- function(x){
    ans <- rep(0, length(x))
    I <- which(x != "0")
    ans[I] <- 1
    factor(ans, 0:1, c("No", "Yes"))
}

# 生产方式
tidy_mode <- function(x){
    x[x != "顺产"] = "刨宫产及其他"
    factor(x)
}
