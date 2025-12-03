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

df_social <- df %>% 
  filter(alpha_med>=-2) %>% 
  filter(alpha_med<=2) %>% 
  filter(beta_med>=-2) %>% 
  filter(beta_med<=2) 


df_agg_ind <- df_social %>% 
  group_by(participant.label, group_type, alpha_med) %>% 
  summarise(
    mean_contribute=mean(contribute,na.rm=TRUE),
    .groups = "drop"
  )

df_summary_alpha <- df_agg_ind %>% 
  count(group_type,alpha_med,mean_contribute, name = "count_alpha") 

df_summary_alpha$group_type <- factor(
  df_summary_alpha$group_type,
  levels = c("T0", "TL", "TM", "TH")
)
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

df_agg_ind_beta <- df_social %>% 
  group_by(participant.label,group_type,beta_med) %>% 
  summarise(
    mean_contribute = mean(contribute, na.rm = TRUE),
    .groups = "drop"
  )

df_summary_beta <- df_agg_ind_beta %>% 
  count(group_type,beta_med,mean_contribute, name = "count_alpha") 
df_summary_beta$group_type <- factor(
  df_summary_beta$group_type,
  levels = c("T0", "TL", "TM", "TH")
)
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

save_dir <- "015/analysis/"
all_trend <- paste0(save_dir, "relation_alpha_contribution.png")
ggsave(
  filename = all_trend,  
  plot = plot_alpha_agg, 
  device = "png", 
  width = 8, 
  height = 5, 
  units = "in"
)

all_trend1 <- paste0(save_dir, "relation_beta_contribution.png")
ggsave(
  filename = all_trend1,  
  plot = plot_beta_agg, 
  device = "png", 
  width = 8, 
  height = 5, 
  units = "in"
)
