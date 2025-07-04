install.packages(c("dplyr","readr", "ggpubr", "psych", "FSelectorRcpp"), dependencies = TRUE)

library(dplyr)
library(readr)

adult_data <- read.csv("C:/Users/user/Pictures/New folder (2)/adult.csv")

head(adult_data)

library(ggpubr)
library(psych)

adult_data <- adult_data %>%
  mutate(income_binary = ifelse(income == "<=50K", 0, 1))

anova_result <- aov(age ~ income, data = adult_data)
summary(anova_result)

cor.test(adult_data$age, adult_data$income_binary, method = "kendall")

anova_fnlwgt <- aov(fnlwgt ~ income, data = adult_data)
summary(anova_fnlwgt)

cor.test(adult_data$fnlwgt, adult_data$income_binary, method = "kendall")

anova_loss <- aov(capital.loss ~ income, data = adult_data)
summary(anova_loss)

cor.test(adult_data$capital.loss, adult_data$income_binary, method = "kendall")

anova_hours <- aov(hours.per.week ~ income, data = adult_data)
summary(anova_hours)

cor.test(adult_data$hours.per.week, adult_data$income_binary, method = "kendall")



library(FSelectorRcpp)
tbl_workclass <- table(adult_data$workclass, adult_data$income)
chisq.test(tbl_workclass, simulate.p.value = TRUE, B = 10000)

adult_data$workclass <- as.factor(adult_data$workclass)
adult_data$income <- as.factor(adult_data$income)

information_gain(income ~ workclass, data = adult_data)

tbl_education <- table(adult_data$education, adult_data$income)
chisq.test(tbl_education)

adult_data$education <- as.factor(adult_data$education)
adult_data$income <- as.factor(adult_data$income)

information_gain(income ~ education, data = adult_data)

tbl_marital <- table(adult_data$marital.status, adult_data$income)
chisq.test(tbl_marital)

adult_data$marital.status <- as.factor(adult_data$marital.status)
adult_data$income <- as.factor(adult_data$income)

information_gain(income ~ marital.status, data = adult_data)

tbl_relationship <- table(adult_data$relationship, adult_data$income)
chisq.test(tbl_relationship)

adult_data$relationship <- as.factor(adult_data$relationship)
adult_data$income <- as.factor(adult_data$income)

information_gain(income ~ relationship, data = adult_data)

tbl_country <- table(adult_data$`native.country`, adult_data$income)
chisq.test(tbl_country, simulate.p.value = TRUE, B = 10000)

adult_data$`native.country` <- as.factor(adult_data$`native.country`)
adult_data$income <- as.factor(adult_data$income)

information_gain(income ~ `native.country`, data = adult_data)
