rm(list = ls())
library(tidyverse) 
library(lme4)
library(stargazer)
library(dplyr)
library(ggplot2) 
library(estimatr)
library(modelsummary)
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/merge_long.csv" 
df <- read_csv(path, show_col_types = FALSE) 

df2 <- df %>% 
  mutate(
    contribute_full = if_else(contribute == 10,1,0),
    contribute_non = if_else(contribute == 0,1,0),
  )

df3 <- df2 %>%
  mutate(
    is_base = as.integer(grepl("base", group_type, ignore.case = TRUE)),
    a015 = as.integer(grepl("015", group_type)),
    a03 = as.integer(grepl("03", group_type)),
    a045 = as.integer(grepl("045", group_type))
  )
modelglm_norm <- glm(
  formula = contribute_full~
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)


modelglm_arr11 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    a015*beta_positive + a03*beta_positive + a045*beta_positive+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)

modelglm_arr <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)

modelglm_arrnon <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)

save_dir <- "015/regression/" 
file_name <- "resultsprobit_tukauyou.html"
full_path <- paste0(save_dir, file_name)

stargazer(modelglm_arr, modelglm_arrnon,
          type = "html",
          out = full_path, 
          title = "Probit Model Results ",
          omit = c("age", "gender", "affiliation"))


stargazer(modelglm_arr,modelglm_arr11, type = "html",
          out = "resultsotameshi.html", title = "Probit Model Results")

models <- list("(1)" = modelglm_arr_non,
               "(2)" = modelglm_arr)

modelsummary(models, 
             title = "Probit Regression",
             stars = TRUE, 
             output = "Probit_modelsummary.html",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))


modelglm_norm_non <- glm(
  formula = contribute_non~
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
modelglm_arr_non <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
modelglm_arr11non <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    a015*beta_positive + a03*beta_positive + a045*beta_positive+
    a015*alpha_positive + a03*alpha_positive + a045*alpha_positive+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)

modelglm_arr <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
modelglm_arr11 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    a015*beta_positive + a03*beta_positive + a045*beta_positive+
    a015*alpha_positive + a03*alpha_positive + a045*alpha_positive+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)

stargazer(modelglm_arr_non,modelglm_arr11non,modelglm_arr,modelglm_arr11, type = "html", 
          out = "resultsotameshinon.html",title = "Probit Model Results (contribute_non)")

models <- list("(1)" = modelglm_arr_non, "(2)" = modelglm_arr11non,
               "(3)" = modelglm_arr, "(4)" = modelglm_arr11)

modelsummary(models, 
             title = "Probit Regression",
             stars = TRUE, 
             output = "Probit_modelsummary.html",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))

modelglm_paradum <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
modelglm_paradum_multi <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive*a045 +
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
stargazer(modelglm_paradum,modelglm_paradum_multi, type = "text", title = "Probit Model Results (contribute_non)")


modelglm_paradum_non <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
modelglm_paradum_multi_non <- glm(
  formula = contribute_non~
    alpha_positive + 
    a015 + alpha_positive*a015 + 
    a03 + alpha_positive*a03 + 
    a045+ alpha_positive*a045 + 
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
stargazer(modelglm_paradum,modelglm_paradum_multi,modelglm_paradum_non,modelglm_paradum_multi_non, type = "html",out ="probit.html",  title = "Probit Model Results ")


summary(modelglm_paradum) 
summary(modelglm_paradum_multi)
summary(modelglm_paradum_non)
summary(modelglm_paradum_multi_non)

model_lm <- lm_robust(
  formula = contribute~
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr <- lm_robust(
  formula = contribute~
    alpha_med + beta_med +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3
)


models <- list("treatment only" = model_lm, "add parameter" = model_lm_arr)

model_lm_dum <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr_multi <- lm_robust(
  formula = contribute~
  alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df3
)

models <- list("treatment only" = model_lm_dum, "add parameter" = model_lm_arr_multi)


modelsummary(models, 
             title = "Robust Regression",
             stars = TRUE, 
             output = "markdown",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))

model_lm_duma <- lm_robust(
  formula = contribute~
    alpha_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr_multia <- lm_robust(
  formula = contribute~
    alpha_positive +
    a015 + alpha_positive*a015 +
    a03 + alpha_positive*a03 +
    a045+ alpha_positive*a045 +
    age + gender + affiliation,
  data = df3
)

models <- list("treatment only" = model_lm_duma, "add parameter" = model_lm_arr_multia)


modelsummary(models, 
             title = "Robust Regression",
             stars = TRUE, 
             output = "markdown",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))

library(lme4)
model_mix <- lmer(
  formula = contribute~
    a015 + a03 + a045+
    age + gender + affiliation +
    (1|participant.label),
  data = df3,
)

model_mix_arr <- lmer(
  formula = contribute~
    alpha_med + beta_med +
    a015 + a03 + a045+
    age + gender + affiliation +
    (1|participant.label),
  data = df3
)
stargazer(model_mix,model_mix_arr, type = "text", title = "Mixed Effect Model Results")

print(table)
library(margins)
dif_glm <- margins(modelglm)
summary(dif_glm)
library(modelsummary)
modelsummary(
  dif_glm, 
  title = "プロビット回帰の平均限界効果 (AME)",
  output = "markdown" 
)

modelglm2 <- glm(
  formula = contribute_non~
    alpha_med + beta_med +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") # プロビットリンクを指定
)

summary(modelglm2)
stargazer(modelglm2, type = "text", title = "Probit Model Results (contribute_non)")


# --- PDF描画のコード (プロットオブジェクトを作成し、最後に配置) ---

# 1. 線形予測子 (eta = x*beta) の計算
eta <- predict(modelglm, type = "link")

# 2. 描画用データの準備
plot_data <- data.frame(eta = eta)

# 標準正規分布のPDFを描画するためのデータの作成
x_range <- seq(min(eta) - 0.5, max(eta) + 0.5, length.out = 500)
pdf_data <- data.frame(
  x = x_range,
  density = dnorm(x_range) # 標準正規分布のPDF
)

# 3. グラフオブジェクトの作成
probit_pdf_plot <- ggplot(plot_data, aes(x = eta)) +
  # 線形予測子のヒストグラム (密度スケール)
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, fill = "salmon", color = "darkred", alpha = 0.6) +
  
  # 標準正規分布のPDF曲線 (phi(z))
  geom_line(data = pdf_data, aes(x = x, y = density), 
            color = "blue", linewidth = 1) +
  
  # 平均線形予測子の位置を垂直線で示す
  geom_vline(xintercept = mean(eta), 
             linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  
  # ラベルとタイトル
  labs(
    title = "プロビット回帰の線形予測子 (eta) 分布と標準正規分布PDF",
    x = "線形予測子 (eta)",
    y = "密度 (Density)"
  ) +
  
  # テーマ設定
  theme_minimal()

# 4. プロットの描画
# Rスクリプトやコンソールで実行された場合、この行によって図が描画されます
print(probit_pdf_plot)

x_range_full <- seq(-3, 3, length.out = 500)
cdf_data_full <- data.frame(
  x = x_range_full,
  probability = pnorm(x_range_full) # 標準正規分布のCDF (Phi(z))
)

# 3. CDFグラフオブジェクトの作成
probit_cdf_plot_full <- ggplot(cdf_data_full, aes(x = x, y = probability)) +
  # CDF曲線 (シグモイド曲線) を描画
  geom_line(color = "blue", linewidth = 1) +
  
  # 平均線形予測子の位置を垂直線で示す (データ範囲は維持)
  geom_vline(xintercept = mean(eta), 
             linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  
  # ゼロ点 (0, 0.5) を示す点を描画
  geom_point(aes(x = 0, y = 0.5), color = "red", size = 2) +
  
  # ラベルとタイトル
  labs(
    title = "プロビット回帰の予測確率 (CDF/シグモイド曲線) - 全体描画",
    x = "線形予測子 (eta)",
    y = "予測確率 P(y=1 | x) = Φ(eta)"
  ) +
  
  # Y軸の範囲を0から1に明示的に設定
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  
  # X軸の範囲を -3 から 3 に設定
  scale_x_continuous(limits = c(-3, 3)) +
  
  # テーマ設定
  theme_minimal()

# 4. プロットの描画
print(probit_cdf_plot_full)

