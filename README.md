# Pima-Diabetes-Prediction
## 项目简介
记录我的第一个完整数据分析项目：从数据清洗的小白到能构建预测模型的探索者。

项目使用的是经典的 Pima Indians Diabetes 数据集。

我的目标是练习一个完整工作流，并尝试建立一个简单的糖尿病预测模型。

## 项目内容
1. **pima.Rmd**：核心分析过程，包括缺失值处理、可视化、建模、评估等  [查看](https://github.com/chenhouhui44/Pima-Diabetes-Prediction/blob/main/pima.Rmd)
2. **diabetes**：存放数据 [查看](https://github.com/chenhouhui44/Pima-Diabetes-Prediction/blob/main/diabetes.csv) 
3. **diabetes.R**：R脚本全部内容[查看](https://github.com/chenhouhui44/Pima-Diabetes-Prediction/blob/main/diabetes.1.R)  
4. **README.md**：记录我的学习过程，即本文档

## 分析流程概要
1.**数据的读取与清洗**

2.**探索性数据分析DA**

3.**建模**

4.**模型评估**

## 项目结果
-模型准确率约 78%

-AUC 达到 0.82（示例）

-最强的风险因素包括：Glucose、BMI、Age、Pregnancies

## 如何运行

安装 R 和 RStudio

安装所需 R 包：tidyverse, ggplot2, pROC, caret 等

打开 pima.Rmd 并点击 Knit 即可运行。
