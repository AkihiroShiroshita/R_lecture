#############################
###Lecture 3#################
###Dealing with Big Data#####
#############################
##R is limited bu the amount of memory on the computer and cores (~2^31)
library(tidyverse)
suppressPackageStartupMessages(library(ff))
df_ff <- read.csv.ffdf(file="stats_data.csv")
class(df_ff)
df_ff[[2]][1]
df_ff$age
#Converting new columns to ff class
df_ff$id <- ff(1:nrow(df_ff))
#
map(df_ff, )
