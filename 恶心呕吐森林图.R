# 加载必要包 ----------------------------------------------------------------
library(metafor)
library(gemtc)
library(readxl)
library(dplyr)
library(coda)

# 数据读取与预处理 ----------------------------------------------------------
ponv_data <- read_excel("恶心呕吐.xlsx")

# 检查列名是否匹配
required_cols <- c("Study", "Treatment", "N", "PONV")
if (!all(required_cols %in% names(ponv_data))) {
  stop("列名不匹配，请确保包含Study, Treatment, N, PONV")
}

# 数据清洗与格式转换
data_clean <- ponv_data %>%
  rename(
    study = Study,
    treatment = Treatment,
    responders = PONV,  # 事件发生数
    sampleSize = N
  ) %>%
  mutate(treatment = toupper(treatment)) %>%
  select(study, treatment, responders, sampleSize)

# 创建网络结构 --------------------------------------------------------------
network_ponv <- mtc.network(
  data.ab = data_clean,  # 二分类臂水平数据
  treatments = data.frame(
    id = c("OSA", "OBA", "OFA"),
    description = c("Opioid-Sparing Analgesia", 
                    "Opioid-Based Analgesia",
                    "Opioid-Free Analgesia"))
)

# 模型参数设置 --------------------------------------------------------------
model_ponv <- mtc.model(network_ponv,
                        type = "consistency",
                        likelihood = "binom",  # 二项分布
                        link = "logit",        # logit链接函数
                        linearModel = "random")

# 运行模型 --------------------------------------------------------------
results_ponv <- mtc.run(model_ponv,
                        n.adapt = 20000,
                        n.iter = 100000,
                        thin = 20)

# 结果提取函数 ------------------------------------------------------------
get_or_contrasts <- function(results, base) {
  eff <- relative.effect(results, t1 = base)
  smry <- summary(eff)
  data.frame(
    Comparison = paste0(rownames(smry$summaries$statistics), " vs ", base),
    LogOR = smry$summaries$statistics[, "Mean"],
    Lower = exp(smry$summaries$quantiles[, "2.5%"]),  # 转换为OR
    Upper = exp(smry$summaries$quantiles[, "97.5%"])
  ) %>% filter(!grepl("sd.d", Comparison))
}

# 获取关键对比结果 --------------------------------------------------------
contrasts_oba <- get_or_contrasts(results_ponv, "OBA")
contrasts_osa <- get_or_contrasts(results_ponv, "OSA")

# 合并并格式化结果
forest_df <- rbind(contrasts_oba, contrasts_osa) %>%
  filter(Comparison %in% c("d.OBA.OSA vs OBA", 
                           "d.OBA.OFA vs OBA",
                           "d.OSA.OFA vs OSA")) %>%
  mutate(
    Comparison = case_when(
      Comparison == "d.OBA.OSA vs OBA" ~ "OSA vs OBA",
      Comparison == "d.OBA.OFA vs OBA" ~ "OFA vs OBA",
      Comparison == "d.OSA.OFA vs OSA" ~ "OFA vs OSA"
    ),
    OR = exp(LogOR),  # 将log(OR)转换为OR
    OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, Lower, Upper)
  )

# 绘制森林图 --------------------------------------------------------------
metafor::forest(
  x = log(forest_df$OR),       # 使用log(OR)作为效应量
  ci.lb = log(forest_df$Lower),
  ci.ub = log(forest_df$Upper),
  slab = forest_df$Comparison,
  xlab = "Odds Ratio (95% CrI)",
  main = "PONV",
  refline = 0,                 # OR=1的参考线
  header = "Treatment Comparison",
  cex = 0.8,
  digits = 2,
  alim = c(-3, 3),            # log(OR)范围
  at = log(c(0.1, 0.5, 1, 2, 5)),  # 显示OR刻度
  atransf = exp,               # 显示原始OR值
  ilab = forest_df$OR_CI,      # 显示格式化后的OR值
  ilab.xpos = -4
)

# 添加自定义图例
text(-1.5, nrow(forest_df)+1, "OR (95% CrI)", font=2, cex=0.8)

