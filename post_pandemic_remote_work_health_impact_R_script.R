# load libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)
library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)

#load dataset
data <- read_csv("C:/Users/nhhlo/OneDrive/Documents/post_pandemic_remote_work_health_impact_2025.csv")

# preview data
glimpse(data)    # Check column names and data types
head(data)       # View the first 6 rows
summary(data)    # Summary of each column

cat("Rows:", nrow(data), "\nColumns:", ncol(data), "\n")
glimpse(data)

# data cleaning 
data <- data %>%
  mutate

data <- data %>%
  mutate(
    Gender = str_to_title(str_trim(Gender)),
    Region = str_to_title(str_trim(Region)),
    Industry = str_to_title(str_trim(Industry)),
    Job_Role = str_to_title(str_trim(Job_Role)),
    Work_Arrangement = str_to_title(str_trim(Work_Arrangement)),
    Mental_Health_Status = str_to_title(str_trim(Mental_Health_Status)),
    Burnout_Level = factor(Burnout_Level, levels = c("Low", "Medium", "High"))
  )
glimpse(data)

health_issues <- data %>%
  separate_rows(Physical_Health_Issues, sep = ";") %>%
  mutate(Physical_Health_Issues = str_trim(Physical_Health_Issues))

health_issues %>%
  count(Physical_Health_Issues, sort = TRUE)


## Descriptive Stats

# categorical counts
cat_counts <- function(df, column) {
  df %>% count({{column}}, sort = TRUE)
}

cat_counts(data, Gender)
cat_counts(data, Work_Arrangement)
cat_counts(data, Burnout_Level)
cat_counts(data, Mental_Health_Status)
cat_counts(data, Industry)

# numeric summaries
numeric_summary <- data %>%
  select(Age, Hours_Per_Week, Work_Life_Balance_Score, Social_Isolation_Score)

summary(numeric_summary)


# EDA Visualisation

# Age distribution by gender
ggplot(data, aes(x = Age, fill = Gender)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Age Distribution by Gender", x = "Age", y = "Count")

# Hours per week distribution
ggplot(data, aes(x = Hours_Per_Week, fill = Work_Arrangement)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Weekly Work Hours by Work Arrangement", x = "Hours Per Week", y = "Count")

# Boxplots for relationships
# Burnout vs Hours Per Week
ggplot(data, aes(x = Burnout_Level, y = Hours_Per_Week, fill = Burnout_Level)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Burnout Level vs Weekly Hours", x = "Burnout Level", y = "Hours Per Week")

# Work-life balance score by work arrangement
ggplot(data, aes(x = Work_Arrangement, y = Work_Life_Balance_Score, fill = Work_Arrangement)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Work-Life Balance by Work Arrangement", x = "Work Arrangement", y = "Work-Life Balance Score")

# Bar plots for categorical counts
# Gender distribution
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

# Work arrangement distribution by region
ggplot(data, aes(x = Region, fill = Work_Arrangement)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Work Arrangement by Region", x = "Region", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# heatmap for physical Health Issues

top_issues <- health_issues %>%
  count(Physical_Health_Issues, sort = TRUE) %>%
  top_n(10)

ggplot(top_issues, aes(x = reorder(Physical_Health_Issues, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Physical Health Issues", x = "Health Issue", y = "Count")

## Correlation
 # correlation matrix
numeric_data <- data %>%
  select(Age, Hours_Per_Week, Work_Life_Balance_Score, Social_Isolation_Score)

cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix", mar = c(0,0,1,0))

# Burnout Risk
ggplot(data, aes(x = Work_Arrangement, fill = Burnout_Level)) +
  geom_bar(position = "fill", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Burnout Levels by Work Arrangement", x = "Work Arrangement", y = "Proportion") +
  scale_y_continuous(labels = percent)

# Mental health vs working hours
ggplot(data, aes(x = Hours_Per_Week, y = Work_Life_Balance_Score, color = Mental_Health_Status)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Work Hours vs Work-Life Balance by Mental Health Status", 
       x = "Hours Per Week", 
       y = "Work-Life Balance Score")


ggplot(data, aes(x = Age, fill = Gender)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Age Distribution by Gender", x = "Age", y = "Count")

ggplot(data, aes(x = Burnout_Level, y = Hours_Per_Week, fill = Burnout_Level)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Burnout Level vs Weekly Hours", x = "Burnout Level", y = "Hours Per Week")

# Logistic Regression 

# Create binary burnout
data <- data %>%
  mutate(Burnout_High = ifelse(Burnout_Level == "High", 1, 0))

# further analysis 

data <- data %>%
  mutate(Burnout_High = ifelse(Burnout_Level == "High", 1, 0))

# Logistic regression
logit_model <- glm(Burnout_Level ~ Hours_Per_Week + Work_Arrangement + Age + Social_Isolation_Score,
                   data = data, family = binomial)
summary(logit_model)

ggplot(data, aes(x = Hours_Per_Week, y = Work_Life_Balance_Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal()

data$predicted_prob <- predict(logit_model, type = "response")
ggplot(data, aes(x = Hours_Per_Week, y = predicted_prob, color = Work_Arrangement)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal()


## Further Regression Analysis


# 1 Linear Regression

lm_model <- lm(Work_Life_Balance_Score ~ Hours_Per_Week + Age + Social_Isolation_Score + Work_Arrangement, 
               data = data)
summary(lm_model)

# Visualize linear regression
ggplot(data, aes(x = Hours_Per_Week, y = Work_Life_Balance_Score, color = Work_Arrangement)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Linear Regression: Work-Life Balance vs Hours Per Week",
       x = "Hours Per Week",
       y = "Work-Life Balance Score")


# 2 Binary Logistic Regression

# Create binary outcome: High Burnout vs Low/Medium
data <- data %>%
  mutate(Burnout_High = ifelse(Burnout_Level == "High", 1, 0))

logit_model <- glm(Burnout_High ~ Hours_Per_Week + Age + Social_Isolation_Score + Work_Arrangement,
                   data = data, family = binomial)
summary(logit_model)

# Odds ratios
exp(coef(logit_model))

# Visualize predicted probabilities
data$predicted_prob <- predict(logit_model, type = "response")
ggplot(data, aes(x = Hours_Per_Week, y = predicted_prob, color = Work_Arrangement)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(title = "Predicted Probability of High Burnout",
       x = "Hours Per Week",
       y = "Probability")


# 3 Multinomial Logistic Regression

library(nnet)
multi_model <- multinom(Burnout_Level ~ Hours_Per_Week + Age + Social_Isolation_Score + Work_Arrangement,
                        data = data)
summary(multi_model)

# Calculate odds ratios
exp(coef(multi_model))

# Predicted probabilities for each burnout level
data$predicted_multi <- predict(multi_model, type = "probs")
head(data$predicted_multi)












