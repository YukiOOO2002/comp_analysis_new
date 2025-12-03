rm(list = ls())
library(tidyverse)
library(readr) 
library(dplyr)
library(stringr)

#データ読み込み（メイン＋質問コントロール＋）
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path_1st <- "015/Nov05_1030_03_1st/comp_demo_03_2025-11-05_1030.csv"
df_1st <- read_csv(path_1st, show_col_types = FALSE)
path_2nd <- "015/Nov05_1330_03_2nd/comp_demo_03_2025-11-05_1330.csv"
df_2nd <- read_csv(path_2nd, show_col_types = FALSE)
df_2nd <- subset(df_2nd, participant._current_page_name == "final")
pathq1 <- "015/Nov05_1030_03_1st/q_2025-11-05_1030.csv"
df_q1 <- read_csv(pathq1, show_col_types = FALSE)
pathq2 <- "015/Nov05_1330_03_2nd/q_2025-11-05_1330.csv"
df_q2 <- read_csv(pathq2, show_col_types = FALSE)
df_q2 <- subset(df_q2, participant._current_page_name == "final")
pathb3 <- "015/cleaning/b3syori_merge_03.csv"
df_b3 <- read_csv(pathb3, show_col_types = FALSE)

#処置群で２セッション目のものに被験者番号+24
df_2nd <- df_2nd %>%
  mutate(
    label_num_char = str_extract(participant.label, "\\d+"),
    new_num = as.numeric(label_num_char) + 24,
    new_label = paste0("SUB", str_pad(new_num, width = 2, pad = "0")),
    participant.label = new_label
  ) %>%
  
  select(-label_num_char, -new_num, -new_label)

df_2nd <- df_2nd %>%
  mutate(
    label_num_char1 = str_extract(group.id_in_subsession, "\\d+"),
    new_num1 = as.numeric(label_num_char1) + 8,
    group.id_in_subsession = new_num1
  ) %>%
  
  select(-label_num_char1, -new_num1)

df_q2 <- df_q2 %>%
  mutate(
    label_num_char = str_extract(participant.label, "\\d+"),
    new_num = as.numeric(label_num_char) + 24,
    new_label = paste0("SUB", str_pad(new_num, width = 2, pad = "0")),
    participant.label = new_label
  ) %>%
  
  select(-label_num_char, -new_num, -new_label)

#２セッション分のデータをマージ＋後で他処置群とマージするので判別記号付与
df_combined <- bind_rows(df_1st, df_2nd)

df_combined <- df_combined %>%
  
  mutate(
    participant.label = paste0("TM", participant.label)
  )

#不必要な列をドロップ
cols_to_drop <- c(
  "participant._is_bot",
  "participant._index_in_pages",
  "participant._max_page_index",
  "participant._current_app_name",
  "participant._current_page_name",
  "participant.time_started_utc",
  "participant.visited",
  "participant.mturk_worker_id",
  "participant.mturk_assignment_id",
  "player.role",
  "session.code",
  "session.label",
  "session.mturk_HITId",
  "session.mturk_HITGroupId",
  "session.comment",
  "session.is_demo",
  "player.payoff",
  "player.round_data",
  "group.individual_share"
)

#列を被験者番号順に
df_combined2 <- df_combined %>% 
  arrange(participant.label)

#質問も同様にマージ＆並べ替え
df_q_combined <- bind_rows(df_q1,df_q2)

df_q_combined <- df_q_combined %>% 
  arrange(participant.label) %>% 
  mutate(
    participant.label = paste0("TM", participant.label)
  )



#列名分かりやすく
df_wide <- df_combined2 %>%
  rename(
    contribute = player.individual_choice, 
    mpcr = player.assigned_mpcr,
    payoff = player.round_payoff,
    total_contribution = group.total_contribution,
    id_subsession = group.id_in_subsession
  ) %>%
  pivot_wider(
    id_cols = c(participant.label), 
    names_from = subsession.round_number,
    values_from = c(contribute, mpcr, payoff, total_contribution, id_subsession),
    names_glue = "round {subsession.round_number}_{.value}"
  )

#質問とマージ
df_wide2 <- df_wide %>% 
  left_join(
    df_b3 %>% 
      select(participant.label,alpha_med,alpha_positive, beta_med, beta_positive),
    by = "participant.label"
  ) %>% 
  left_join(
    df_q_combined %>% 
      select(participant.label,player.gender, player.age,player.affiliation),
    by = "participant.label"
  ) 

write.csv(
  df_wide2, 
  file = "015/cleaning/merge_03.csv",
  row.names = FALSE,   
  quote = FALSE       
)

