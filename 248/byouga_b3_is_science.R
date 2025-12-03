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

df1<- df %>%
  group_by(alpha_med, beta_med) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  )

df <- df %>% 
  left_join(df1 %>% 
              select("alpha_med","beta_med","count"))
help("left_join")

scatter_plot_all <- df %>%
  ggplot(aes(x = alpha_med, y = beta_med)) +
  geom_point(aes(color = factor(science), size = 3), alpha = 0.2) +
  scale_x_continuous(breaks = seq(-0.4,0.4,0.1)) +
  scale_y_continuous(breaks = scales::breaks_width(1)) +
  guides(size = "none") +
  coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(-7, 2)) +
  labs(
    x = expression(alpha[med]),
    y = expression(beta[med]),
    title = "alpha/beta Distribution (All)" # タイトル変更
  ) +
  scale_size_continuous(range = c(2, 10)) +
  theme_gray() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray")

# # ファイル名と保存
save_dir <- "015/analysis/"
all_filename <- paste0("alpha_beta_distribution_", "allsci11", ".png")
all_save_path <- paste0(save_dir, all_filename)

ggsave(
  filename = all_save_path,
  plot = scatter_plot_all,
  device = "png",
  width = 8,
  height = 5,
  units = "in"
)

scatter_plot_all <- df %>%
  ggplot(aes(x = alpha_med, y = beta_med)) +
  geom_point(aes(color = factor(science), size = count), alpha = 0.5) +
  scale_x_continuous(breaks = seq(-10,10,1)) +
  scale_y_continuous(breaks = scales::breaks_width(1)) +
  guides(size = "none") +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-7, 2)) +
  labs(
    x = expression(alpha[med]),
    y = expression(beta[med]),
    title = "alpha/beta Distribution (All)" # タイトル変更
  ) +
  scale_size_continuous(range = c(2, 10)) +
  theme_gray() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray")

# # ファイル名と保存
save_dir <- "015/analysis/"
all_filename <- paste0("alpha_beta_distribution10_", "allsci", ".png")
all_save_path <- paste0(save_dir, all_filename)

ggsave(
  filename = all_save_path,
  plot = scatter_plot_all,
  device = "png",
  width = 8,
  height = 5,
  units = "in"
)

