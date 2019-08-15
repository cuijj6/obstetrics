d <- d_early
info <- d[, .(i, dup = rep(1, .N) * .N > 1), .(id, name)]

I_1 <- info[dup == FALSE, i]
I_2 <- info[dup == TRUE, i]

d_1 <- d[I_1, ]
d_2 <- d[I_2, ]

r  <- merge(d, d_all, by = .(name, id, id_full) %>% names())[, 1:12]
# r2 <- match_dt(d_2, d_all)
# map(r2, tidy_match)

tidy_df <- function(df, diff = FALSE){
    vars_sel <- .(id, id_full, name, date_birth, date_examine, date_delivery, `婴儿1体重`) %>% names()
    vars <- intersect(vars_sel, names(df))
    res  <- df[, ..vars]
    if (diff) res[, diff := as.numeric(difftime(date_delivery, date_examine, units = "days"))]
    res
}

tidy_match <- function(l) {
    x <- tidy_df(l$x)
    y <- tidy_df(l$y)
    merge <- tidy_df(l$merge)
    listk(x, y, merge)
}


# r2[[1]]$merge %>% .[, ]

d_temp <- merge(d_in, d_early, by = .(name, id, id_full), all.x = TRUE)
# 5076
d_exam <- merge(d_temp, d_after, by = .(name, id, id_full), all.x = TRUE) %>%
    merge(d_all[, .(name, id, id_full, date_delivery)])

names_org <- c('date_examine.y', 'date_examine.x', 'date_examine')
names_new <- c('date_exam_early', 'date_exam_mid', 'date_exam_after')
names(d_exam)[match(names_org, names(d_exam))] <- names_new

d_exam %<>% mutate(
    diff1 = as.numeric(difftime(date_exam_mid, date_exam_early, units = "days")),
    diff2 = as.numeric(difftime(date_exam_after, date_exam_mid, units = "days")))
d_merge <- d_exam[-which( diff1 < 0 | diff2 < 0 | diff1 > 300 | diff2 > 300 |
                            (date_delivery - date_exam_mid < 0) | (date_exam_after - date_delivery < 0) |
                            (date_delivery - date_exam_mid > 300) | (date_exam_after - date_delivery > 300) )] # 3605
d_merge %<>% reorder_name(.(name, id, id_full, i.y, i.x, i, date_exam_early, date_exam_mid, date_delivery, date_exam_after, diff1, diff2))
I_cols <- grep("year", colnames(d_merge)) %>% setdiff(1:ncol(d_merge), .)
d_merge %<>% .[, ..I_cols]

d_merge2 <- ddply(d_merge, .(id), function(d){
    n <- nrow(d)
    if (n == 1) {
        d
    } else if (n > 1) {
        if (length(unique(d$date_delivery)) == 1) {
            d[which.max(date_exam_early), ]
        }
    }
})

res <- listk(value = d_merge2)
write_list2xlsx(res, "d_检查指标.xlsx")

# d_exam[is.na(diff1) & is.na(diff2)]
## merge ALL -------------------------------------------------------------------
# 81 - 56
