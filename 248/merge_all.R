rm(list = ls())
library(tidyverse)
library(readr) 
library(dplyr)
library(stringr)


#データ読み込み（メイン＋質問コントロール＋）
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")

path_base <- "015/cleaning/merge_base.csv"
df_base <- read_csv(path_base, show_col_types = FALSE)

path_015 <- "015/cleaning/merge_015.csv"
df_015 <- read_csv(path_015, show_col_types = FALSE)

path_03 <- "015/cleaning/merge_03.csv"
df_03 <- read_csv(path_03, show_col_types = FALSE)

path_045 <- "015/cleaning/merge_045.csv"
df_045 <- read_csv(path_045, show_col_types = FALSE)

df <- bind_rows(df_base,df_015,df_03,df_045)

df <- df %>% 
  mutate(
    player.age = case_when(
      player.age == 222 ~ 22,
      player.age == 223 ~ 23,
      player.age == 199 ~19,
      TRUE ~ player.age
    ) 
  ) %>% 
  mutate(
    player.gender = case_when(
      player.gender == "男性" ~ 0,
      player.gender == "女性" ~ 1,
      player.gender == "どちらでもない" ~ 2
    )
  ) %>% 
  rename(age = player.age) %>% 
  rename(gender = player.gender) %>% 
  rename(affiliation = player.affiliation) 



write.csv(
  df, 
  file = "015/data/merge_final.csv",
  row.names = FALSE,   
  quote = FALSE       
)
