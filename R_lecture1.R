###################
###Lecture 1#######
###Data handling###
###################
##Before we get started...
##Connecting GitHub and Rstudio
install.packages("usethis") #to creat a readme file
install.packages("rmarkdown")
usethis::use_readme_rmd()
###################
###Data cleaning###
###################
getwd() 
setwd("/cloud/project")
library(readxl)
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
##Datacleaning: sheet1
od1 <- read_excel("original_data.xlsx",
                  na = c("", "NA", "999","."),
                  sheet = "入契肺炎",
                  skip = 2)
od1 %>% glimpse
#od1 %>% as.data.frame -> od1
#str(od1)
colnames(od1)
od1 <- od1 %>% 
  rename(id = 患者番号,
         age = 入院時年齢,
         steroid = ステロイド使用,
         intubation = 気管内挿管,
         starts = 入院年月日,
         prognosis = 退院時転帰)
append <- od1 %>% select(ends_with("(入院時)"))
od1 <- od1 %>% 
  mutate(adl = rowSums(append), #Don't use "na.rm = TRUE"
         hospitalterm = interval(od1$退院年月日, od1$starts))
od1 <- od1 %>% 
  mutate(steroid = ifelse(is.na(steroid), "0", "1"))  #Don't do "steroid == NA".
od1 <- od1 %>% 
  select(id, starts, age, steroid, intubation, prognosis, adl, hospitalterm) 
##Datacleaning: sheet2
od2 <- read_excel("original_data.xlsx",
                  sheet = "バイタル",
                  skip = 0)
od2 <- od2 %>% 
  rename(id = 患者番号,
         bp = 血圧,
         hr = 脈拍,
         rr = 呼吸数)
od2_sub <- str_split(od2$bp, "/", simplify=T) %>% 
  as_tibble() %>% 
  rename(sbp = V1,
         dbp= V2)
od2_2_sub <- bind_cols(od2, od2_sub) 
#Connecting od1 and od2_sub
od12 <- inner_join(od1, od2_2_sub, by = "id")
##Data cleaning: od3
od3 <- read_excel("original_data3.xlsx")
od3 %>% glimpse()
od3$患者名[od3$患者名 %in% 
          c("司馬", "シバ")] <- "司馬"
od3 <- od3 %>% 
  rename(id = 患者番号,
         name = 患者名,
         wbc = ハッケッキュウ,
         eo = コウサンキュウスウ,
         starts = 入院年月日,
         testday = 検査日)
od3$starts <- ymd(od3$starts)
od3$testday <- ymd(od3$testday)
range(c(od3$eo))
od3 <- od3 %>% 
  mutate(eo_ab = round(wbc * eo/100, digits = 0)) %>% #round≠四捨五入
  select(-name, -wbc, -eo)
od3_wide <- od3 %>% 
  spread(var, number)
od3_wide <- od3_wide %>% 
  arrange(id, testday) %>% 
  group_by(id, starts) %>% 
  filter((testday==starts|testday==starts+1) & (!bun == "ソクテイフノウ") & !is.na(bun)) %>% 
  filter(row_number()==1)
#long format
#long <- od3_wide %>% gather(., key = "var",
#                            value = "number",
#                            bun)
#connecting data: od12 and od3_wide 
install.packages("tidylog")
library(tidylog)
od4 <- left_join(od12, od3_wide, by=c("id", "starts"))
#picking up the patients with no lab data
od_plus <- anti_join(od3_wide, od12, by=c("id", "starts"))
##Saving data
save(od1, od2_2_sub, od12, od3, od3_wide, od4, file = "after_cleaning.rda", compress = "gz")
#Reload RDA files
load("/cloud/project/after_cleaning.rda")
#od4 %>% write_csv(path="cleaned_original_data.csv")
#write_rds(df, "R_lecture_dataset/after_cleaning.rds", compress = "gz")
#install.packages("openxlsx")
#library(openxlsx)
#write.xlsx(df, "after_cleaning.xlsx")
###Connecting many data in the same folder
getwd()
dir.create("R_lecture_dataset")
dir.create("R_lecture_dataset/cleaned_datasets",
           recursive = TRUE)
setwd("/cloud/project/R_lecture_dataset/cleaned_datasets")
filelist <- list.files("/cloud/project/R_lecture_dataset/cleaned_datasets",
                       pattern = "\\csv$")
final_df <- map_df(filelist, ~{
  path <- file.path("/cloud/project/R_lecture_dataset/cleaned_datasets", .)
  df <- read_csv(path, locale = locale(encoding = "SHIFT-JIS"))
  df <- df %>% mutate_all(.funs = as.character)
  })
final_df <- final_df %>% 
  rowid_to_column(., "ID") 
############################
###Advanced data cleaning###
############################
##For Shiroshtia's study
rp1 <- read_excel("original_data_relapse.xlsx", 
                  sheet = "入契肺炎")
dim(rp1)
rp1 <- rp1 %>% 
  rename(id = 患者番号,
         age = 入院時年齢,
         sex = 性別,
         start = 入院年月日,
         end = 退院年月日)
#Check the number of relapse patients
rp1_grouped <- rp1 %>% 
  group_by(id) %>% 
  filter(n() >= 2)
length(unique(rp1_grouped$id)) #Not use "rp1_grouped[id]".
#rp1_grouped %>% ungroup()
rp2 <- rp1_grouped %>% 
  mutate(tag = start - lag(end)) %>% 
  filter(tag <= 365)
##manipulating strings and substrings
##regular expression
su <- read_excel("original_summary.xlsx")
su <- su %>% 
  rename(id = 患者番号,
         start = 入院年月日,
         comment = 考察)
which(str_detect(su$comment, "HOT"))
su <- su %>% 
  mutate(hot = str_detect(su$comment, "HOT"))
str_subset(su$comment, "COPD.?")
su <- su %>% 
  mutate(o2 = str_extract(su$comment, "......酸素.........")) 
str_view(su$o2, "(?<=投与)\\d+")
str_view(su$o2, "\\d+(?=(L|l))")
str_detect(su$o2, "[1][0-5]") #\\d: 0-9
str_replace_all(su$comment, "COPD", "慢性閉塞性肺疾患")

