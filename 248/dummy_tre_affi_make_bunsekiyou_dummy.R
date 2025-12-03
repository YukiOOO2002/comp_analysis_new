rm(list = ls())
library(tidyverse) 
library(lme4)
library(stargazer)
library(dplyr)
library(ggplot2) 
library(estimatr)
library(broom)
library(gt)
library(modelsummary)
library(plm)
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/merge_long.csv" 
df <- read_csv(path, show_col_types = FALSE) 

df2 <- df %>% 
  mutate(
    contribute_full = if_else(contribute == 10,1,0),
    contribute_non = if_else(contribute == 0,1,0),
  )


df2 <- df2 %>%
  mutate(
    science = case_when(
      (affiliation >= 5 & affiliation <= 11) | 
        (affiliation >= 17 & affiliation <= 23) | 
        affiliation %in% c(26, 27) ~ 1,
      .default = 0
    )
  ) %>% 
  mutate(
    graduate = case_when(
      affiliation >=13 ~1,
      .default = 0
    )
  )

write.csv(
  df2, 
  file = "015/data/bunsekiyou_dummy.csv",
  row.names = FALSE,   
  quote = FALSE       
)


