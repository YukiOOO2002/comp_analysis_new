library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)

rm(list=ls())
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ") 
path <-  "015/data/merge_long_c0.csv"
df <- read_csv(path, show_col_types = FALSE)

df2 <- df %>%
  filter(str_detect(`participant.label`, "^03"))

df2 <- df2 %>% 
  arrange(id_subsession,round) %>% 
  select(participant.label,round,contribute,payoff,id_subsession,max_contribute,,max_payoff,max_c_and_p)

df3 <- df2 %>% 
  filter(round==1)

df4 <- df3 %>% 
  filter(max_contribute==1 & max_payoff==1)

df5 <- df3 %>% 
  filter(max_contribute==1 & max_payoff==0)

df6 <- df3 %>% 
  filter(max_contribute==0 & max_payoff==1)

df7 <- df3 %>% 
  filter(max_contribute==0 & max_payoff==0)


df22 <- df %>%
  filter(str_detect(`participant.label`, "^015"))

df22 <- df22 %>% 
  arrange(id_subsession,round) %>% 
  select(participant.label,round,contribute,payoff,id_subsession,max_contribute,,max_payoff,max_c_and_p)

df3 <- df22 %>% 
  filter(round==1)

df4 <- df3 %>% 
  filter(max_contribute==1 & max_payoff==1)

df5 <- df3 %>% 
  filter(max_contribute==0 & max_payoff==1)

df6 <- df3 %>% 
  filter(max_contribute==1 & max_payoff==0)

df7 <- df3 %>% 
  filter(max_contribute==0 & max_payoff==0)

