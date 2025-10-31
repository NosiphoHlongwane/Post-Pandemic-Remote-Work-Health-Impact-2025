# Load Libraries 
library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)
library(corrplot)
library(nnet)

# Load Dataset 
data <- read_csv("C:/Users/nhhlo/OneDrive/Documents/post_pandemic_remote_work_health_impact_2025.csv")

# Data Overview 
glimpse(data)
head(data)
summary(data)

cat("Rows:", nrow(data), "\nColumns:", ncol(data), "\n")

# --- Data Cleaning ---
# Clean and standardise text columns
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

# --- Physical Health Issues Cleaning ---
health_issues <- data %>%
  separate_rows(Physical_Health_Issues, sep = ";") %>%
  mutate(Physical_Health_Issues = str_trim(Physical_Health_Issues))

health_issues %>%
  count(Physical_Health_Issues, sort = TRUE)

# --- Descriptive Statistics ---
# Function to count categorical variables
cat_counts <- function(df, column) {
  df %>% count({{column}}, sort = TRUE)
}

cat_counts(data, Gender)
cat_counts(data, Work_Arrangement)
cat_counts(data, Burnout_Level)
cat_counts(data, Mental_Health_Status)
cat_counts(data, Industry)

# Numeric summaries
numeric_summary <- data %>%
  select(Age, Hours_Per_Week, Work_Life_Balance_Score, Social_Isolation_Score)
summary(numeric_summary)

# --- Exploratory Data Analysis ---

# Age distribution by gender
ggplot(data, aes(x = Age, fill = Gender)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Age Distribution by Gender", x = "Age", y = "Count")

# Weekly work hours by work arrangement
ggplot(data, aes(x = Hours_Per_Week, fill = Work_Arrangement)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(title = "Weekly Work Hours by Work Arrangement", x = "Hours Per Week", y = "Count")

# Burnout vs Hours Per Week
ggplot(data, aes(x = Burnout_Level, y = Hours_Per_Week, fill = Burnout_Level)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Burnout Level vs Weekly Hours", x = "Burnout Level", y = "Hours Per Week")

# Work-life balance by work arrangement
ggplot(data, aes(x = Work_Arrangement, y = Work_Life_Balance_Score, fill = Work_Arrangement)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Work-Life Balance by Work Arrangement", x = "Work Arrangement", y = "Work-Life Balance Score")

# Gender distribution
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(alpha = 0.8) +
  theme_minimal() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

# Work arrangement by region
ggplot(data, aes(x = Region, fill = Work_Arrangement)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Work Arrangement by Region", x = "Region", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Top Physical Health Issues ---
top_issues <- health_issues %>%
  count(Physical_Health_Issues, sort = TRUE) %>%
  slice_max(n, n = 10)

ggplot(top_issues, aes(x = reorder(Physical_Health_Issues, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Physical Health Issues", x = "Health Issue", y = "Count")

# --- Correlation Analysis ---
numeric_data <- data %>%
  select(Age, Hours_Per_Week, Work_Life_Balance_Score, Social_Isolation_Score) %>%
  mutate(across(everything(), as.numeric))

cor_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix", mar = c(0,0,1,0))

# --- Burnout Risk Visualisation ---
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
       x = "Hours Per Week", y = "Work-Life Balance Score")

# --- Regression Models ---

## Linear Regression
lm_model <- lm(Work_Life_Balance_Score ~ Hours_Per_Week + Age + Social_Isolation_Score + Work_Arrangement, 
               data = data)
summary(lm_model)

ggplot(data, aes(x = Hours_Per_Week, y = Work_Life_Balance_Score, color = Work_Arrangement)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Linear Regression: Work-Life Balance vs Hours Per Week",
       x = "Hours Per Week", y = "Work-Life Balance Score")

## Binary Logistic Regression
# Create binary variable for high burnout
data <- data %>%
  mutate(Burnout_High = ifelse(Burnout_Level == "High", 1, 0))

logit_model <- glm(Burnout_High ~ Hours_Per_Week + Age + Social_Isolation_Score + Work_Arrangement,
                   data = data, family = binomial)
summary(logit_model)

# Odds ratios
exp(coef(logit_model))

# Predicted probabilities
data$predicted_prob <- predict(logit_model, type = "response")

ggplot(data, aes(x = Hours_Per_Week, y = predicted_prob, color = Work_Arrangement)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(title = "Predicted Probability of High Burnout",
       x = "Hours Per Week", y = "Probability")

## Multinomial Logistic Regression
multi_model <- multinom(Burnout_Level ~ Hours_Per_Week + Age + Social_Isolation_Score + Work_Arrangement,
                        data = data)
summary(multi_model)

# Odds ratios
exp(coef(multi_model))

# Predicted probabilities (as a separate matrix)
predicted_multi <- predict(multi_model, type = "probs")
head(predicted_multi)
