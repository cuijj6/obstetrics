source('test/main_pkgs.R')

indir <- "INPUT/raw-data/2th-20190604-Jane/"
files <- dir(indir, "*.xlsx", full.names = TRUE)

lst <- map(files, ~read.xlsx(.) %>% data.table())
d   <- lst[[1]]

lst2 <- map(lst[1:3], extractBasicInfo) %>% set_names(c("ALL", "previous", "current"))

I_match <- match2(lst2$current$上次住院号 %>% as.numeric(), lst2$previous$住院号 %>% as.numeric())
lst2$current  %<>%  .[I_match$I_x, ]
lst2$previous %<>%  .[I_match$I_y, ]

lst2$previous[, `住院流水号` := `本次住院号`]

write_list2xlsx(lst2, "INPUT/中大附一GDM复发-20190810.xlsx")

# fwrite2(info, "baseInfo.csv")
## 检查列名变化
# varnames <- map(lst, colnames)
# names <- unique(unlist(varnames))
# df <- do.call(rbind, .)
# I_du <- duplicated(Ids) %>% which()
# map(varnames, ~setdiff(names, .))

# map(lst, nrow)
# map_int(lst, nrow)

# info_user <- d[, .(`患者姓名`, `出生日期`, `住院号`)] %>% unique()
# d[, .N, .(`患者姓名`, `出生日期`)][N >= 2]
