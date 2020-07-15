###################
###Lecture1########
###Data handling###
###################
##Before we get started...
##Connecting GitHub and Rstudio
install.packages("usethis") #to creat readme file
install.packages("rmarkdown")
usethis::use_readme_rmd()
###################
###Data cleaning###
###################
getwd() #setwd("~/project")
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
  filter((starts==testday|starts==testday+1) & !bun == "ソクテイフノウ")  
#long format
#long <- od3_wide %>% gather(., key = "var", value = "number",
#                            -id, -starts, -testday, -eo_ab)
#connecting data: od12 and od3_wide 
install.packages("tidylog")
library(tidylog)
od4 <- left_join(od12, od3_wide, by=c("id", "starts"))
#picking up the patients with no lab data
od_plus <- anti_join(od3_wide, od12, by=c("id", "starts"))
##Saving data
save(od1, od2_2_sub, od12, od3, od3_wide, od4, file = "after_cleaning.rda")
od4 %>% write_csv(path="cleaned_original_data.csv")
#Reload RDA files
load("/cloud/project/after_cleaning.rda")
#Connecting data

############################
###Advanced data cleaning###
############################
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
