library("tidyverse")
df <- read_csv("../project/pneumocopd_analysis_complete.csv",
               col_types = cols(.default = col_double()))
install.packages("mice")
library("mice")
