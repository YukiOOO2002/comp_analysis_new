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
path <- "015/data/bunsekiyou_dummy.csv" 
df <- read_csv(path, show_col_types = FALSE) 
df_social_alpha <- df %>% 
  filter(alpha_med< -0.0215|alpha_med>0.024)
df_social_beta <- df%>% 
  filter(beta_med< -0.0715|beta_med>0.0555)
summary(df)

df_alpha <- df_social_alpha %>% 
  filter(alpha_med>=-2) %>% 
  filter(alpha_med<=2) %>% 
  filter(beta_med>=-2) %>% 
  filter(beta_med<=2) 

df_beta <- df_social_beta %>% 
  filter(alpha_med>=-2) %>% 
  filter(alpha_med<=2) %>% 
  filter(beta_med>=-2) %>% 
  filter(beta_med<=2) 


df_agg_ind <- df_alpha %>% 
  group_by(participant.label, group_type, alpha_med) %>% 
  summarise(
    mean_contribute=mean(contribute,na.rm=TRUE),
    .groups = "drop"
  )

df_summary_alpha <- df_agg_ind %>% 
  count(group_type,alpha_med,mean_contribute, name = "count_alpha") 

plot_alpha_agg <- ggplot(
  data=df_summary_alpha,
  aes(x=alpha_med,
      y = mean_contribute,
      size = count_alpha)
)+
  geom_point()+
  labs(title = "Relation Between Alpha and Contribution")+
  facet_wrap(~group_type)
print(plot_alpha_agg)

df_agg_ind_beta <- df_beta %>% 
  group_by(participant.label,group_type,beta_med) %>% 
  summarise(
    mean_contribute = mean(contribute, na.rm = TRUE),
    .groups = "drop"
  )

df_summary_beta <- df_agg_ind_beta %>% 
  count(group_type,beta_med,mean_contribute, name = "count_alpha") 

plot_beta_agg <- ggplot(
  data=df_summary_beta,
  aes(x=beta_med,
      y = mean_contribute,
      size = count_alpha)
)+
  geom_point()+
  labs(title = "Relation Between Alpha and Contribution")+
  facet_wrap(~group_type)
print(plot_beta_agg)
