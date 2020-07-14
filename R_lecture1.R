###Lecture1#######
###Data handling##
##################
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
od1 <- read_excel("original_data.xlsx",
                  sheet = "入契肺炎",
                  skip = 2)
str(od1)
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
#Datacleaning: sheet1
colnames(od1)
od1 <- od1 %>% 
  rename(id = 患者番号,
         age = 入院時年齢,
         steroid = ステロイド使用,
         intubation = 気管内挿管,
         prognosis = 退院時転帰)
od1 <- od1 %>% 
  mutate(adl = apply(od1[,16:25], 1, sum),
         hospitalterm = interval(od1$退院年月日, od1$入院年月日)) 
od1 <- od1 %>% 
  select(id, age, steroid, intubation, prognosis, adl) 
#Datacleaning: sheet2
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
#Connecting od1 and od3
od12 <- inner_join(od1, od2_2_sub, by = "id")
#Saving data
save(od1, od2_2_sub, od12, file = "after_cleaning.rda")
od12 %>% write_csv(path="cleaned_original_data.csv")
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
