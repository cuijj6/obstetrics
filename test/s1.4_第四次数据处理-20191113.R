source('test/main_pkgs.R')

# df <- Ipaper::read_xlsx("INPUT/中大附一GDM复发-20190810.xlsx", 1)
df.cui <- Ipaper::read_xlsx("INPUT/data/GDM复发最终数据-20191112.xlsx", 2)
# d <- Ipaper::read_xlsx("INPUT/data/姓名匹配.xlsx")
#
# # d[, .(name, date_birth)] %>% duplicated() %>% which()
#
# info1 = match2(df[, .(name)], d[, .(name)])
# info2 = match2(df[, .(name, date_birth)], d[, .(name, date_birth)])
# info  = match2(df[, .(name, date_delivery1)], d[, .(name, date_delivery1)])
# x = df[-info$I_x, ]
# y = d[-unique(info$I_y), ]


## 第四次数据处理: 2019-11-13
names = c("ALL", "previous", "current") %>% set_names(., .)
lst <- map(names, ~Ipaper::read_xlsx("INPUT/中大附一GDM复发-20190810.xlsx", .x))

diabetes_target = c("妊娠期糖尿病", "妊娠伴糖尿病", "妊娠期并发糖尿病", "妊娠期发生的糖尿病")
lst$previous[out_judge_diabetes %in% diabetes_target & date_delivery >= "2010-01-01", ]

c("first", "second", "group", "admin", "serinum", "name", "date_birth",
    "age2", "date_delivery2",
    "OGTT2_0_d", "OGTT2_1_d", "OGTT2_2_d", "OGTT2_0_a", "OGTT2_1_a", "OGTT2_2_a",
    "famhis2", "height2", "diagn2", "hist2", "HBA2_d",

    "CHOL2_d", "TG2_d", "HDL2_d", "LDL2_d",
    "TSH", "FT3", "FT4", "GLU2_d", "HBA2_a",
    "CHOL2_a", "TG2_a", "HDL2_a", "LDL2_a", "insu2_0_a", "uglu2_1_a", "uglu2_2_a",

    "age1", "date_delivery1", "gender1_1", "weight1_1", "brth1_1", "out1_1",
    "gender1_2", "weight1_2", "brth1_2", "out1_2", "insulin1", "hypoins1",
    "ptimes1", "dtimes1", "pweek1", "pday1", "mode1", "wght_b1", "wght_d1",
    "height1", "diagn1", "pdiagn1", "hist1", "HBA1_d",

    "OGTT1_0_d", "OGTT1_1_d", "OGTT1_2_d",
    "CHOL1_d", "TG1_d", "HDL1_d", "LDL1_d")

# "OGTT1_3_d",