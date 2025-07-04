# Install required packages
install.packages(c("dplyr","readr", "ggplot2"), dependencies = TRUE)

library(dplyr)
library(ggplot2)
library(readr)

adult_data <- read.csv("C:/Users/user/Pictures/New folder (2)/adult.csv")

head(adult_data)

ggplot(adult_data, aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

ggplot(adult_data, aes(x = age, y = hours.per.week, color = income)) +
  geom_point(alpha = 0.4) +
  labs(title = "Scatterplot: Age vs Hours per Week by Income",
       x = "Age", y = "Hours per Week")

ggplot(adult_data, aes(x = income, y = age, fill = income)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot of Age by Income", x = "Income", y = "Age")

ggplot(adult_data, aes(x = fnlwgt)) +
  geom_histogram(binwidth = 10000, fill = "darkorange", color = "black") +
  labs(title = "Histogram of fnlwgt", x = "fnlwgt", y = "Frequency")

ggplot(adult_data, aes(x = fnlwgt, y = age, color = income)) +
  geom_point(alpha = 0.4) +
  labs(title = "Scatterplot: fnlwgt vs Age by Income",
       x = "fnlwgt", y = "Age")
ggplot(adult_data, aes(x = income, y = fnlwgt, fill = income)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot of fnlwgt by Income", x = "Income", y = "fnlwgt")


ggplot(adult_data, aes(x = education.num, y = age, color = income)) +
  geom_jitter(width = 0.3, alpha = 0.4) +
  labs(title = "Scatterplot: Education Level vs Age by Income", x = "Education Level (Num)", y = "Age")

ggplot(adult_data, aes(x = capital.gain, y = age, color = income)) +
  geom_point(alpha = 0.4) +
  labs(title = "Scatterplot: Capital Gain vs Age by Income",
       x = "Capital Gain", y = "Age")

ggplot(adult_data, aes(x = income, y = capital.gain, fill = income)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot of Capital Gain by Income", x = "Income", y = "Capital Gain")

ggplot(adult_data, aes(x = capital.loss)) +
  geom_histogram(binwidth = 200, fill = "purple", color = "black") +
  labs(title = "Histogram of Capital Loss", x = "Capital Loss", y = "Frequency")

ggplot(adult_data, aes(x = capital.loss, y = age, color = income)) +
  geom_point(alpha = 0.4) +
  labs(title = "Scatterplot: Capital Loss vs Age by Income",
       x = "Capital Loss", y = "Age")

ggplot(adult_data, aes(x = income, y = capital.loss, fill = income)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot of Capital Loss by Income", x = "Income", y = "Capital Loss")

ggplot(adult_data, aes(x = hours.per.week)) +
  geom_histogram(binwidth = 2, fill = "darkcyan", color = "black") +
  labs(title = "Histogram of Hours Worked Per Week", x = "Hours per Week", y = "Frequency")

ggplot(adult_data, aes(x = hours.per.week, y = age, color = income)) +
  geom_point(alpha = 0.4) +
  labs(title = "Scatterplot: Hours per Week vs Age by Income",
       x = "Hours per Week", y = "Age")

ggplot(adult_data, aes(x = income, y = hours.per.week, fill = income)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot of Hours per Week by Income", x = "Income", y = "Hours per Week")

ggplot(adult_data, aes(x = workclass, fill = workclass)) +
  geom_bar() +
  labs(title = "Distribution of Workclass", x = "Workclass", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(adult_data, aes(x = education, fill = education)) +
  geom_bar() +
  labs(title = "Distribution of Education Levels", x = "Education", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(adult_data, aes(x = marital.status, fill = marital.status)) +
  geom_bar() +
  labs(title = "Distribution of Marital Status", x = "Marital Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(adult_data, aes(x = occupation, fill = occupation)) +
  geom_bar() +
  labs(title = "Distribution of Occupation", x = "Occupation", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(adult_data, aes(x = relationship, fill = relationship)) +
  geom_bar() +
  labs(title = "Distribution of Relationship Status", x = "Relationship", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(adult_data, aes(x = race, fill = race)) +
  geom_bar() +
  labs(title = "Distribution of Race", x = "Race", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(adult_data, aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(title = "Distribution of Sex", x = "Sex", y = "Count") +
  theme_minimal()

ggplot(adult_data, aes(x = `native.country`, fill = `native.country`)) +
  geom_bar() +
  labs(title = "Distribution of Native Countries (All)", x = "Native Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))

ggplot(adult_data, aes(x = income, fill = income)) +
  geom_bar() +
  labs(title = "Income Class Distribution", x = "Income", y = "Count") +
  theme_minimal()
