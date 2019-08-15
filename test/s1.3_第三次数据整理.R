source('test/main_pkgs.R')
library(lubridate)

devtools::load_all(".")
# ------------------------------------------------------------------------------
indir <- "INPUT/third/"
files <- dir(indir, "^[^~].*.xlsx$", full.names = TRUE) %>%
    set_names(basename(.))

l <- map(files, read_xlsx) %>% map(data.table)

items1 <- table(l[[2]]$项目名称) %>% .[order(names(.))]
items2 <- table(l[[4]]$项目名称) %>% .[order(names(.))]

filter_item <- function(d){
    d <- d[, ..vars_left] %>% set_names(vars_name)
    d[item == "甘油三脂TG", item := "甘油三酯TG"]
    d[item %in% varnames, ]
}

{
    # "单位"       "参考范围"   "高低标志"   "危急标志"
    ## 孕期, 产后
    d_in    <- l[[4]] %>% filter_item() %>%
        df_merge_date() %>% rename_items() %>% add_id()
    d_after <- l[[2]] %>% filter_item() %>%
        df_merge_date() %>% rename_items() %>% add_id()

    ## 2. 早孕
    varnames_2 <- c("葡萄糖GLU", "尿糖(干化学)", "尿蛋白(干化学)")
    d <- l[[3]] %>% .[`项目名称` %in% varnames_2, ..vars_left] %>% set_names(vars_name)
    d <- unique(d[order(id_full, date_examine)])
    d_fix <- df_merge_date(d)
    d_early <- dcast2(d_fix, "item", "value", fun.aggregate = dplyr::first, default = NA_character_) %>% add_id()
    # d_in <- merge(d_early, d_in)
    # write_list2xlsx(list("早孕" = d_early, "孕期" = d_in, "孕后" = d_after), "INPUT/cuijj6_OBS_third.xlsx")
}
d_all <- Ipaper::read_xlsx('INPUT/中大附一GDM复发-20190810.xlsx') %>% check_date_df()

d_early %<>% check_date_df()
d_in    %<>% check_date_df()
d_after %<>% check_date_df()

save(d_early, d_in, d_after, d_all, file = "third.rda")
