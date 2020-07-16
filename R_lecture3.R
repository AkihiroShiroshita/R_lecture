#############################
###Lecture 3#################
###Dealing with Big Data#####
#############################
##
df <- read_csv("stats_data.csv",
               col_types=cols(.default="c"))
##R is limited bu the amount of memory on the computer and cores (~2^31)
library(tidyverse)
suppressPackageStartupMessages(library(ff))
df_ff <- read.csv.ffdf(file="stats_data.csv")
class(df_ff)
#Converting new columns to ff class
df_ff$newCol <- ff(1:nrow(df_ff))
#Visualizing 
hist(df_ff[,2])
