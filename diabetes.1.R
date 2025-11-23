# 加载所需的包：dplyr 用于数据操作
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(knitr)) install.packages("knitr")
library(knitr)

# 假设 diabetes.csv 文件已在 R Markdown 运行环境中加载
diabetes <- read.csv("D:/10226/Pima_Diabetes_Project/diabetes.csv") # 假设文件在当前路径下

# 将不合理的 0 值替换为 NA
cols_check <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
for(col in cols_check){
  diabetes[[col]][diabetes[[col]] == 0] <- NA
}

# 使用中位数对缺失值进行填充 (NA -> Median)
for(col in cols_check){
  diabetes[[col]][is.na(diabetes[[col]])] <- median(diabetes[[col]], na.rm = TRUE)
}

cat("--- 清洗后数据摘要 --- \n")
summary(diabetes)
# 加载所需的包：caret 用于数据分区
if(!require(caret)) install.packages("caret")
library(caret)

set.seed(123)
train_index <- createDataPartition(diabetes$Outcome, p = 0.7, list = FALSE)

train_data <- diabetes[train_index, ]
test_data  <- diabetes[-train_index, ]

cat("训练集样本量:", nrow(train_data), "\n")
cat("测试集样本量:", nrow(test_data), "\n")
# 1. 先计算均值和中位数
mean_tbl <- aggregate(. ~ Outcome, diabetes, mean)
median_tbl <- aggregate(. ~ Outcome, diabetes, median)

# 2. Outcome 转中文
mean_tbl$Outcome <- ifelse(mean_tbl$Outcome == 0, "未患病", "患病")
median_tbl$Outcome <- ifelse(median_tbl$Outcome == 0, "未患病", "患病")

# 设置行名
rownames(mean_tbl) <- mean_tbl$Outcome
rownames(median_tbl) <- median_tbl$Outcome

# 删除 Outcome 列
mean_tbl$Outcome <- NULL
median_tbl$Outcome <- NULL

#  3. 合并为 “均值(中位数)” 形式
final_tbl <- data.frame(
  Map(function(mn, md) paste0(round(mn, 2), " (", round(md, 2), ")"),
      mean_tbl, median_tbl)
)

# 行名
rownames(final_tbl) <- rownames(mean_tbl)

# 4. 列名改成中文
cn_names <- c(
  "Pregnancies"    = "怀孕次数",
  "Glucose"        = "血糖",
  "BloodPressure"  = "血压",
  "SkinThickness"  = "皮褶厚度",
  "Insulin"        = "胰岛素",
  "BMI"            = "体质指数",
  "DiabetesPedigreeFunction" = "家族遗传系数",
  "Age"            = "年龄"
)

# 匹配并替换列名
current_names <- colnames(final_tbl)
new_names <- cn_names[current_names]
colnames(final_tbl) <- ifelse(is.na(new_names), current_names, new_names)

# 显示最终表格 (使用 kable 美化)
kable(final_tbl, caption = "表 1: 各组生理指标的均值 (中位数) 对比表")
# 加载所需的包：ggplot2 用于绘图，patchwork 用于拼图
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(patchwork)) install.packages("patchwork")
library(ggplot2)
library(patchwork)

# 定义加固版绘图函数
plot_hist_density <- function(data, var, breaks_seq, title) {
  data <- as.data.frame(data)
  df <- data.frame(x = data[[var]])
  df <- df[!is.na(df$x), , drop = FALSE]
  if(nrow(df) == 0) return(NULL)
  
  p_temp <- ggplot(df, aes(x)) +
    stat_bin(breaks = breaks_seq, aes(y = after_stat(count / sum(count) * 100))) +
    stat_density(aes(y = after_stat(density)), geom = "line")
  
  built <- ggplot_build(p_temp)
  hist_max <- tryCatch(max(built$data[[1]]$y, na.rm = TRUE), warning = function(w) 0)
  dens_max <- tryCatch(max(built$data[[2]]$y, na.rm = TRUE), warning = function(w) 0)
  
  if(dens_max == 0) scale_factor <- 1 else scale_factor <- (hist_max * 0.7) / dens_max
  
  ggplot(df, aes(x)) +
    geom_histogram(
      breaks = breaks_seq,
      aes(y = after_stat(count / sum(count) * 100)),
      fill = "steelblue", color = "black", alpha = 0.75
    ) +
    geom_density(
      aes(y = after_stat(density * scale_factor)),
      color = "hotpink", linewidth = 1.2
    ) +
    labs(title = title, x = title, y = "百分比(%)") +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), panel.grid.minor = element_blank())
}

# 设定 Breaks
breaks_bmi   <- seq(0, 70, 5)       
breaks_glu   <- seq(0, 210, 15)     
breaks_ins   <- seq(0, 900, 50)     
breaks_bp    <- seq(0, 130, 10)     
breaks_skin  <- seq(0, 100, 10)     
breaks_pedi  <- seq(0, 2.5, 0.2)   

p1 <- plot_hist_density(diabetes, "BMI", breaks_bmi, "BMI")
p2 <- plot_hist_density(diabetes, "Glucose", breaks_glu, "血糖")
p3 <- plot_hist_density(diabetes, "Insulin", breaks_ins, "胰岛素")
p4 <- plot_hist_density(diabetes, "BloodPressure", breaks_bp, "血压")
p5 <- plot_hist_density(diabetes, "SkinThickness", breaks_skin, "皮褶厚度")
p6 <- plot_hist_density(diabetes, "DiabetesPedigreeFunction", breaks_pedi, "家族遗传系数")

(p1 | p2 | p3) / (p4 | p5 | p6)
# 加载所需的包：tidyr 用于数据重塑
if(!require(tidyr)) install.packages("tidyr")
library(tidyr)

# 定义中文映射 (确保 dplyr, ggplot2 已加载)
chinese_labels <- c(
  Pregnancies = "怀孕次数", Glucose = "血糖", BloodPressure = "血压", 
  SkinThickness = "皮褶厚度", Insulin = "胰岛素", BMI = "BMI", 
  DiabetesPedigreeFunction = "家族遗传系数", Age = "年龄", 
  Outcome = "诊断结果" 
)
feature_labels <- chinese_labels[1:8]

diabetes_long <- diabetes %>%
  pivot_longer(cols = -Outcome, names_to = "Feature", values_to = "Value") %>%
  mutate(Outcome = factor(Outcome), 
         Feature = factor(Feature, levels = names(feature_labels)))

ggplot(diabetes_long, aes(x = Outcome, y = Value, fill = Outcome)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1.5) +
  facet_wrap(~Feature, scales = "free", ncol = 3, labeller = labeller(Feature = feature_labels)) + 
  scale_fill_manual(values = c("steelblue", "hotpink"), labels = c("健康", "糖尿病")) +
  labs(title = "各生理指标在健康与患病人群中的分布差异", x = "诊断结果", y = "数值", fill = "组别") +
  theme_bw() +
  theme(legend.position = "top", strip.text = element_text(size = 12, face = "bold"))
# 加载所需的包：reshape2 用于相关矩阵重塑
if(!require(reshape2)) install.packages("reshape2")
library(reshape2)

cor_matrix <- cor(diabetes, use = "complete.obs")
melted_cor <- melt(cor_matrix)

ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) + 
  scale_x_discrete(labels = chinese_labels, name = "变量") +
  scale_y_discrete(labels = chinese_labels, name = "变量") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = "皮尔逊\n相关系数") +
  labs(title = "变量间相关性热力图") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1), axis.text.y = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  coord_fixed()
# 准备数据：结局变量转换为因子
train_data_clean <- train_data %>% mutate(Outcome = factor(Outcome, levels = c(0, 1)))
test_data_clean <- test_data %>% mutate(Outcome = factor(Outcome, levels = c(0, 1)))

# 模型训练
model_lr <- glm(Outcome ~ ., data = train_data_clean, family = binomial)

# 打印模型摘要
print(summary(model_lr))
# 加载所需的包：caret 用于混淆矩阵
if(!require(caret)) install.packages("caret")
library(caret)

# 预测概率
test_probabilities <- predict(model_lr, newdata = test_data_clean, type = "response")

# 转换为分类预测 (阈值 0.5)
test_predictions_class <- ifelse(test_probabilities > 0.5, "1", "0")
test_predictions_factor <- factor(test_predictions_class, levels = c("0", "1"))

# 创建混淆矩阵
conf_matrix <- confusionMatrix(
  data = test_predictions_factor, 
  reference = test_data_clean$Outcome,  
  positive = "1" 
)
print(conf_matrix)
# 加载所需的包：pROC 用于 ROC 曲线和 AUC 计算
if(!require(pROC)) install.packages("pROC")
library(pROC)

# 绘制 ROC 曲线并计算 AUC
roc_curve <- roc(response = test_data_clean$Outcome, predictor = test_probabilities)
auc_value <- auc(roc_curve)

# 绘制 ROC 曲线图
plot(roc_curve, 
     main = paste("ROC 曲线 (AUC:", round(auc_value, 4), ")"),
     col = "blue", 
     lwd = 2,
     print.auc = TRUE)
legend("bottomright", 
       legend = paste("AUC =", round(auc_value, 4)), 
       col = "blue", lwd = 2)

