varnames <- c("血红蛋白A1c(HBA1c)", "白细胞",
    "OGTT空腹血糖", "OGTT血糖 1 h", "OGTT血糖 2 h", "总胆固醇CHOL", "甘油三酯TG", "高密度胆固醇HDL-c", "低密度胆固醇LDL-c",  # [5]
    "空腹胰岛素", "胰岛素 1 h", "胰岛素 2 h")

vars_left <- c("住院流水号", "住院号", "患者姓名", "检验日期", "项目名称", "结果") # , "申请日期", "标本号"
vars_name <- c("id_full", "id", "name", "date_examine", "item", "value")

#' adjust date
#' @note Only suit for third data
merge_date <- function(x) {
    x$date_examine %<>% substr(1, 10) %>% ymd()
    # x[, year := year(date_examine)]

    diff <- diff(ymd(x$date_examine))
    grp  <- cumsum(c(0, diff > 120))
    date <- x$date_examine[!duplicated(grp)]
    x$date_examine <- factor(grp, labels = date) %>% as.factor()

    x %>% reorder_name(headvars = c("id_full", "id", "name", "date_examine", "year"))
}

df_merge_date <- function(df){
    df <- df[order(id_full, date_examine), ]
    ddply(df, .(id_full), merge_date)
}

add_id <- function(d){
    d$i <- 1:nrow(d)
    d %>% reorder_name("i")
}

add_year <- function(d, n = 1){
    d$year %<>% add(n)
    d
}

# 首先假设名字没有重复, 不成立

#' @param d matched d
merge_df <- function(d1, d2, dm = NULL, n = 0){
    previous <- dm
    if (!is.null(dm)){
        dd1 <- d1 %>% as.data.frame() %>% .[-dm$i.x, ] %>% data.table()
        dd2 <- d2 %>% as.data.frame() %>% .[-dm$i.y, ] %>% data.table()
    } else {
        dd1 <- d1
        dd2 <- d2
    }
    dm <- merge(add_year(dd1, n), dd2, by = c("id_full", "id", "year"), all.y = TRUE)
    # browser()
    rbind(previous, dm)[order(i.y), ]
}

self <- function(x, ...) x
check_date_df <- function(d){
    names <- names(d)
    vars <- intersect(c("date_examine", "date_delivery"), names)

    foreach(var = vars) %do% {
        d[[var]] %<>% ymd()
    }
    d
}


#' @param by plyr style by
match_dt <- function(d1, d2, by){
    if (missing(by)) {
        by <- intersect(names(d1), names(d2)) %>% set_names(., .)
    }
    l1 <- dlply(d1, by, self)
    l2 <- dlply(d2, by, self)

    info <- match2(names(l1), names(l2))
    res  <- foreach(x = l1[info$I_x], y = l2[info$I_y], i = icount()) %do% {
        merge <- merge(x, y, by = names(by))
        listk(x, y, merge)
    }
    res
}


merge <- function(x, y, by, ...){
    if (missing(by)) {
        by <- intersect(names(x), names(y)) %>% set_names(., .)
    }
    if (class(by) == "quoted") {
        by <- names(by)
    }
    data.table:::merge.data.table(x, y, by, ...)
}
