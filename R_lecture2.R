########################
###Lecture 2############
###Summarizing data#####
########################
###Read in the csv file
library(tidyverse)
library(lubridate)
df <- read_csv("stats_data.csv", 
               locale = locale(encoding = "SHIFT-JIS"), 
               col_types = cols(
                 id = col_double(),
                 age = col_double(),
                 gender = col_double(),
                 steroid = col_double(),
                 intubation = col_double(),
                 hospitalterm = col_double(),
                 wbc = col_double(),
                 eosi_p = col_double(),
                 bun = col_double(),
                 ics = col_factor(),
                 laba = col_factor(),
                 lama = col_factor(),
                 rr = col_double(),
                 ams = col_factor(),
                 hr = col_double(),
                 hot = col_factor(),
                 wheeze = col_factor(),
                 delirium = col_factor(),
                 stability = col_factor(),
                 time_to_stability = col_double(),
                 death = col_factor(),
                 cencode = col_factor(),
                 discharge = col_factor(),
                 insulin = col_factor(),
                 adl = col_factor(),
                 hospital = col_factor()
               ),
               guess_max = 1500, #default: 1000
               na = c("NA", "NULL", ".", "999", "#VALUE!", "MA", "ND", "NE"))
#spec(df)
#cols_condense(spec(df))
install.packages("naniar")
install.packages("visdat")
library(naniar)
library(visdat)
vis_miss(df)
miss_var_summary(df)
#Don't use df %in% c(NA, 999)
###Visualizing data "simply"
qplot(df$age)
qplot(df$hospital)
df %>% qplot(x=age, y=hospitalterm, color=hospital, data=.) 
###Visualizing data for the manuscript
###ggplot + geom(aes) + labs + scale + theme
ggplot(data = df) +
  geom_bar(mapping = aes(x = hospital))
ggplot(data = df) +
  geom_histogram(mapping = aes(x=age, fill = hospital)) #bar, hist: fill
ggplot(data = df) +
  geom_count(mapping = aes(x=hospital, y= death)) +
  labs(title = "Number of death in each hospital",
       x = "Hospital", y= "Death")
ggplot(data = df) +
  geom_boxplot(mapping = aes(x=hospital, y= age)) + 
  theme_classic() #bw: black&white
ggplot(data = df) +
  geom_point(mapping = aes(x=age, y= hospitalterm,
             color = hospital)) + #point, line: color
  scale_color_discrete(name = "Hospital", 
                       breaks = c(1,2,3,4,5),
                       labels = c("KMC", "AMA", "AWA", "SAI", "ICHI"))
  #scale_color_discrete(guide=FALSE)
  #"scale_fill_discrete"
#theme_classic()+  ##before adjusting x-labels
#    theme(axis.text.x=element_text(angle=45, hjust=1))
###Summarizing data
install.packages("tableone")
library(tableone)
###Baseline characterisrics
#Overall baseline
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
###Omitting not available data
df_c <- df %>%
  drop_na()