###Lecture1#######
###Data handling##
##################
##Before we get started...
##Connecting GitHub and Rstudio
install.packages("usethis") #to creat readme file
install.packages("rmarkdown")
usethis::use_readme_rmd()
###Data cleaning############
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
od1 <- od1 %>% 
  rename(id = 患者番号,
         age = 入院時年齢,
         steroid = ステロイド使用,
         intubation = 気管内挿管,
         prognosis = 退院時転帰)
od1 <- od1 %>% 
  mutate(adl = apply(od1[,16:25], 1, sum),
         hospitalterm = interval(original_data$退院年月日, original_data$入院年月日)) 
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
od13 <- inner_join(od1, od2_2_sub, by = "id")  
