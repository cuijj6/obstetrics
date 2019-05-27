fwrite2 <- function(x, file){
    write.table(x, file, sep = ",", row.names = FALSE, fileEncoding = "gbk")
}

#' read_xlsx
#' 
#' @importFrom openxlsx read.xlsx
#' @export
read_xlsx <- function(file, sheet = 1, ...){
    read.xlsx(file, sheet, ...) %>% data.table()
}
