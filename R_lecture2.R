###Read in the csv file
library(tidyverse)
library(lubridate)
df <- read_csv("../project/pneumocopd_analysis_complete.csv",
               locale = locale(encoding = "CP932"),
               col_types = cols(.default = col_double()))
install.packages("naniar")
install.packages("visdat")
library(naniar)
library(visdat)
vis_miss(df)
miss_var_summary(df)
###Summarizing data
install.packages("tableone")
library(tableone)
#Baseline characterisrics
factorVars <- c("gender", "steroid", "ics", "laba", "lama", "ams", "death")
vars <- c("age", "steroid", "gender", "hospitalterm", "ics", "laba", "lama", "rr", "ams", "hr","death", "bun")
table1 <- CreateTableOne(vars = vars, data = df, includeNA = TRUE, factorVars = factorVars)
table1 %>% 
  print(nonnormal = c("hospitalterm")) %>% write.csv(file = "table1.csv")
#Baseline characteristics by treatment 
table2 <- CreateTableOne(vars = c("age", "gender", "hospitalterm", "ics", "laba", "lama", "rr", "ams", "hr", "death"),
                         strata = "steroid", factorVars = c("gender","ics", "laba", "lama", "ams", "death"), data = df)
table2 %>% 
  print(exact = "MMT",
        nonnormal = c("hospitalterm"),
        smd = TRUE) %>% 
  write.csv(file = "table2.csv")