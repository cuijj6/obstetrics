source('test/main_pkgs.R')

file_raw <- "data/sysu_1th_obstetrics.rda"

if (!file.exists(file_raw)) {
    files <- dir("data/raw-data/", "*.xlsx$", full.names = TRUE)
    names <- basename(files) %>% gsub(".xlsx$", "", .)

    l <- llply(files, function(file) {
        read.xlsx(file) %>% data.table
    }, .progress = "text")

    l %<>% set_names(names)
    d <-l[4:6] %>% do.call(rbind, .)
    l <- c(list(`04－住院检验` =d), l[-(4:6)])
    I <- order(names(l))
    save(l, file = file_raw)
} else {
    load(file_raw)
}


## tidy data
get_info_Inspect <- function(d, nmin=100, outfile) {
    info <- table(d[, .(`套餐名`, `项目名称`)]) %>% as.data.frame() %>% as.data.table()
    info <- info[order(-Freq)][Freq>=100]
    if (!missing(outfile)) {
        write.xlsx(info, outfile)
    }
    info
}

## 1. 初次产检
# rm: 送检日期,
