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
setwd("././././.")
path <- "015/data/merge_long.csv"
df <- read_csv(path, show_col_types = FALSE) 
df <- read_csv(path, show_col_types = FALSE) 
df_fs <- df %>% 
  filter(alpha_med>=-2) %>% 
  filter(alpha_med<=2) %>% 
  filter(beta_med>=-2) %>% 
  filter(beta_med<=2) 

df_fs$group_type <- factor(
  df_fs$group_type,
  levels = c("T0", "TL", "TM", "TH")
)

df_m <- df_fs %>% 
  filter(gender==0)

df_w <- df_fs %>% 
  filter(gender==1)

df_summary <- df_fs %>% 
  count(group_type,alpha_med,contribute, name = "count_alpha") 

df_agg_ind <- df_fs %>% 
  group_by(participant.label, group_type, alpha_med,beta_med) %>% 
  summarise(
    mean_contribute=mean(contribute,na.rm=TRUE),
    .groups = "drop"
  )

plot_alpha <- ggplot(
  data = df_agg_ind,
  aes(x =  alpha_med,
      y=,mean_contribute,))+
  geom_point()+
  labs(title = "Relation Between Alpha and Contribution m")+
  facet_wrap(~group_type)
print(plot_alpha)

df_summary_alpha <- df_agg_ind %>% 
  count(group_type,alpha_med,mean_contribute, name = "count_alpha") 

plot_alpha_agg <- ggplot(
  data=df_summary_alpha,
  aes(x=alpha_med,
      y = mean_contribute,
      size = count_alpha)
)+
  geom_point()+
  labs(title = "Relation Between Alpha and Contribution m")+
  facet_wrap(~group_type)
print(plot_alpha_agg)

df_agg_ind_beta <- df_fs %>% 
  group_by(participant.label, group_type, beta_med) %>% 
  summarise(
    mean_contribute=mean(contribute,na.rm=TRUE),
    .groups = "drop"
  )

plot_beta <- ggplot(
  data = df_agg_ind,
  aes(x =  beta_med,
      y=,mean_contribute,))+
  geom_point()+
  labs(title = "Relation Between Beta and Contribution m")+
  facet_wrap(~group_type)
print(plot_beta)

df_summary_beta <- df_agg_ind %>% 
  count(group_type,beta_med,mean_contribute, name = "count_beta") 

plot_beta_agg <- ggplot(
  data=df_summary_beta,
  aes(x=beta_med,
      y = mean_contribute,
      size = count_beta)
)+
  geom_point()+
  labs(title = "Relation Between Beta and Contribution m")+
  facet_wrap(~group_type)
print(plot_beta_agg)

save_dir <- "015/analysis/"
all_trend <- paste0(save_dir, "relation_alpha_contribution_m.png")
ggsave(
  filename = all_trend,  
  plot = plot_alpha_agg, 
  device = "png", 
  width = 8, 
  height = 5, 
  units = "in"
)

all_trend1 <- paste0(save_dir, "relation_beta_contribution_m.png")
ggsave(
  filename = all_trend1,  
  plot = plot_beta_agg, 
  device = "png", 
  width = 8, 
  height = 5, 
  units = "in"
)


df_summaryw <- df_w %>% 
  count(group_type,alpha_med,contribute, name = "count_alpha") 

df_agg_indw <- df_w %>% 
  group_by(participant.label, group_type, alpha_med,beta_med) %>% 
  summarise(
    mean_contribute=mean(contribute,na.rm=TRUE),
    .groups = "drop"
  )

df_summary_alphaw <- df_agg_indw %>% 
  count(group_type,alpha_med,mean_contribute, name = "count_alpha") 

plot_alpha_aggw <- ggplot(
  data=df_summary_alphaw,
  aes(x=alpha_med,
      y = mean_contribute,
      size = count_alpha)
)+
  geom_point()+
  labs(title = "Relation Between Alpha and Contribution w")+
  facet_wrap(~group_type)
print(plot_alpha_aggw)

df_agg_ind_betaw <- df_w %>% 
  group_by(participant.label, group_type, beta_med) %>% 
  summarise(
    mean_contribute=mean(contribute,na.rm=TRUE),
    .groups = "drop"
  )


df_summary_betaw <- df_agg_indw %>% 
  count(group_type,beta_med,mean_contribute, name = "count_beta") 

plot_beta_aggw <- ggplot(
  data=df_summary_betaw,
  aes(x=beta_med,
      y = mean_contribute,
      size = count_beta)
)+
  geom_point()+
  labs(title = "Relation Between Beta and Contribution w")+
  facet_wrap(~group_type)
print(plot_beta_aggw)

save_dir <- "015/analysis/"
all_trendw <- paste0(save_dir, "relation_alpha_contribution_w.png")
ggsave(
  filename = all_trendw,  
  plot = plot_alpha_aggw, 
  device = "png", 
  width = 8, 
  height = 5, 
  units = "in"
)

all_trendw1 <- paste0(save_dir, "relation_beta_contribution_w.png")
ggsave(
  filename = all_trendw1,  
  plot = plot_beta_aggw, 
  device = "png", 
  width = 8, 
  height = 5, 
  units = "in"
)
# group_factor <- as.factor(df_agg_ind$group_type)
# group_colors <- rainbow(nlevels(group_factor))[unclass(group_factor)]
# library(rgl)
# plot3d(
#   x = df_agg_ind$alpha_med,
#   y = df_agg_ind$beta_med,
#   z = df_agg_ind$mean_contribute,
#   xlab = "Alpha",
#   ylab = "Beta",
#   zlab = "Mean Contribute ",
#   col = group_colors,
#   size = 5,     # 点のサイズ
#   type = "p",   # 描画タイプを点(point)に指定
#   main = "3D Scatter Plot: Alpha, Beta, and Mean Contribution"
# )
