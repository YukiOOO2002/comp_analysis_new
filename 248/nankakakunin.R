# 必要なパッケージをロード
# install.packages(c("dplyr", "stringr")) が必要です
library(dplyr)
library(stringr)

rm(list = ls()) 
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ") 
path <- "015/data/merge_long_c0.csv"
df <- read_csv(path, show_col_types = FALSE)

groups <- c( "03", "045")
rounds <- 1:10

results_list <- list()

for (group in groups) {
  for (r in rounds) {
    temp_df <- df %>%
      filter(round == r) %>%
      filter(str_detect(participant.label, paste0("^", group)))
    filter_keys <- temp_df %>%
      filter(max_contribute == 1 & max_c_and_p == 0) %>%
      select(id_subsession) %>%
      distinct()
    extracted_data <- temp_df %>%
      semi_join(filter_keys, by = "id_subsession")
    results_list[[paste0(group, "_R", r)]] <- extracted_data
  }
}

# 4. 全ての抽出結果のデータフレームを縦に結合
final_df <- bind_rows(results_list)

cat("--- 最終的に結合されたデータフレームの最初の6行 ---\n")
print(head(final_df))

cat("\n--- 最終データフレームの次元 ---\n")
print(paste("行数:", nrow(final_df), "、列数:", ncol(final_df)))

df3 <- final_df %>% 
  filter(str_detect(participant.label, paste0("^", 03)))
                    