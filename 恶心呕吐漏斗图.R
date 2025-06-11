library(readxl)
library(dplyr)
library(metafor)
library(ggplot2)

# 读取数据 ----------------------------------------------------------------
ponv_data <- read_excel("恶心呕吐.xlsx") 

# 预处理数据 --------------------------------------------------------------
# 生成两两比较对（兼容性版本）
data_ponv <- ponv_data %>%
  group_by(Study) %>%
  filter(n_distinct(Treatment) >= 2) %>%  # 保留至少有两个组的试验
  do({
    study_data <- .
    treatments <- unique(study_data$Treatment)
    if (length(treatments) < 2) return(NULL)  # 安全检查
    
    # 生成所有治疗对组合
    comparisons <- combn(treatments, 2, simplify = FALSE)
    lapply(comparisons, function(pair) {
      # 提取治疗组数据
      group1 <- study_data %>% filter(Treatment == pair[1])
      group2 <- study_data %>% filter(Treatment == pair[2])
      
      # 跳过无效组合
      if (nrow(group1) == 0 | nrow(group2) == 0) return(NULL)
      
      data.frame(
        Study = unique(study_data$Study),
        Treat1 = pair[1],
        Treat2 = pair[2],
        event1 = group1$PONV,
        n1 = group1$N,
        event2 = group2$PONV,
        n2 = group2$N
      )
    }) %>%
      bind_rows()  # 使用dplyr的bind_rows代替map_dfr
  }) %>%
  ungroup() %>%
  filter(!is.na(event1) & !is.na(event2))  # 最终过滤

# 计算OR效应量 -----------------------------------------------------------
data_meta <- escalc(
  measure = "OR",
  ai = event1, n1i = n1,
  ci = event2, n2i = n2,
  data = data_ponv,
  slab = paste(Study, Treat1, "vs", Treat2)
)

# 统一比较方向（以OBA为参照）--------------------------------------------
data_meta <- data_meta %>%
  mutate(
    Comparison = case_when(
      Treat1 == "OBA" ~ paste(Treat1, "vs", Treat2),
      Treat2 == "OBA" ~ paste(Treat2, "vs", Treat1),
      TRUE ~ paste(Treat1, "vs", Treat2)  # 保留原始比较名称
    )
  )
# 调整后的颜色映射
color_palette <- c("OBA vs OSA" = "#F8766D", 
                   "OBA vs OFA" = "#00BA38",
                   "OFA vs OSA" = "#619CFF")
# 运行随机效应模型 --------------------------------------------------------
meta_model <- rma(yi = yi, sei = sqrt(vi), 
                  data = data_meta, method = "REML")

# 绘制漏斗图 -------------------------------------------------------------
funnel(meta_model, main = "",
       col = color_palette[data_meta$Comparison],
       xlab = "Log Odds Ratio",
       level = c(90, 95),
       shade = c("white", "gray65"))

# 添加图例和Egger检验
legend("topright", legend = names(color_palette),
       fill = color_palette, bty = "n")
egger_test <- regtest(meta_model)
print(egger_test)
