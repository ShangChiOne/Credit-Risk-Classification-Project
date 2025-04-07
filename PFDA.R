# Ryan Lim Xin Kai, TP072014
# Sanjivan A/L Thiyageswaran, TP070073
# Fareez, TP077930


# Install and load required packages using pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, cluster, rpart, rpart.plot, randomForest, mice)

#To check if libraries have been loaded successfully
sessionInfo()

# Load necessary libraries and suppress message during start up
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))       
suppressPackageStartupMessages(library(cluster))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(rpart.plot))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(mice))


#work directory
print(getwd())
setwd("C:\\Users\\sanji\\Desktop\\Work\\University\\Y2\\SEM 1\\PFDA\\PFDA Assignment")


# Load the Dataset
credit_data <- read.csv("5. credit_risk_classification.csv")
print("Dataset was loaded in successfully")


# To view the Dataset
View(credit_data)


required_columns <- c("credit_amount", "class", "purpose", "age", "savings_status", "checking_status", "duration", "num_dependants")
required_columns

# Identify missing columns in the dataset
missing_columns <- setdiff(required_columns, names(credit_data))
missing_columns


# Check if there are any missing columns and handle the error
if (length(missing_columns) > 0) {
  stop(paste(
    "Error: The following required columns are missing from the dataset:",
    paste(missing_columns, collapse=", "),
    "\nPlease verify that your dataset includes these columns with exact names and correct spelling."
  ))
} else {
  message("All columns that are required are present.")
}


# Initial Exploration
print("Now exploring the dataset's structure and providing a summary")
str(credit_data)           
summary(credit_data)       
head(credit_data)          
print("Dataset exploration completed")


# --- Data Cleaning Section Start ---

# Handle Missing Values using MICE (Multiple Imputation by Chained Equations)
print("Imputing missing values using MICE...")
imputed_data <- mice(credit_data, m = 5, method = 'rf', seed = 123)  
credit_data <- complete(imputed_data)
print("Missing values imputed successfully.")

# Remove Duplicates
credit_data <- credit_data[!duplicated(credit_data), ]
print("Duplicate rows removed.")

# Convert necessary columns to factors
credit_data$class <- as.factor(credit_data$class)       
credit_data$purpose <- as.factor(credit_data$purpose)   
credit_data$savings_status <- as.factor(credit_data$savings_status)
credit_data$checking_status <- as.factor(credit_data$checking_status)
credit_data$num_dependants <- as.factor(credit_data$num_dependants)
print("Converted 'class', 'purpose', 'savings_status', 'checking_status' and 'num_dependants' to factors.")

# Validate that the categorical variables are factors
expected_factors <- c("class", "purpose", "savings_status", "checking_status" , "num_dependants")
actual_factors <- sapply(credit_data[expected_factors], is.factor)

if (all(actual_factors)) {
  print("All required categorical variables are factors.")
} else {
  warning(paste("The following variables are not factors:",
                paste(names(actual_factors)[!actual_factors], collapse = ", ")))
}


# --- Data Cleaning Section End ---


# --- Validation of Cleaned Data ---

# Validation After Missing Value Imputation
if (any(is.na(credit_data))) {
  print("Warning: Missing values still exist after imputation.")
} else {
  print("No missing values detected after imputation.")
}

# Validate Data Types
expected_types <- list(credit_amount = "numeric", class = "factor", purpose = "factor")
actual_types <- sapply(credit_data, class)
mismatched_types <- names(expected_types)[expected_types != actual_types[names(expected_types)]]
if (length(mismatched_types) > 0) {
  print(paste("Warning: Data types mismatch for columns:", paste(mismatched_types, collapse = ", ")))
} else {
  print("All data types are correct.")
}


# Summary of Cleaned Data
print("Summary of cleaned data:")
summary(credit_data)

# Validate Column Distributions with Histogram
ggplot(credit_data, aes(x = credit_amount)) + 
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") + 
  labs(title = "Distribution of Credit Amount", x = "Credit Amount", y = "Frequency")







# --- Ryan Lim Xin Kai (TP072014) ---

# Install ggmosaic for mosaic plot
if (!require("ggmosaic")) install.packages("ggmosaic")
library(ggmosaic)


# Factorizing age, class, and purpose if needed
credit_data$class <- factor(credit_data$class)
credit_data$purpose <- factor(credit_data$purpose)


# Objective 1: Investigate the relationship between loan purpose and credit risk
# Verify variables for objective 1
required_vars <- c("purpose", "class")
missing_vars <- setdiff(required_vars, names(credit_data))

if (length(missing_vars) > 0) {
  print(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
  stop("Required variables for Ryan's objective (purpose, class) are missing in the dataset.")
} else {
  print("All required variables are present in the dataset: 'purpose' and 'class'. Proceeding with analysis.")
}


# Frequency table for Credit Risk Classes 
print("Frequency Table for Credit Risk Classes:")
freq_table <- table(credit_data$purpose, credit_data$class)
print(freq_table)



# Graph 1: Stacked Bar Plot for Proportions of Credit Risk by Loan Purpose
stacked_bar_plot <- ggplot(credit_data, aes(x = purpose, fill = class)) +
  geom_bar(position = "fill") +  # Position "fill" scales bars to proportions
  labs(
    title = "Proportions of Credit Risk (Good/Bad) by Loan Purpose",
    x = "Loan Purpose",
    y = "Proportion",
    fill = "Credit Risk"
  ) +
  scale_fill_manual(values = c("bad" = "red", "good" = "blue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(stacked_bar_plot)
ggsave("Stacked_Bar_Plot_Credit_Risk_by_Loan_Purpose.png", plot = stacked_bar_plot, width = 10, height = 6, dpi = 300)



# Graph 2: Grouped Bar Plot for Distribution of Credit Risk by Loan Purpose
grouped_bar_plot <- ggplot(credit_data, aes(x = purpose, fill = class)) +
  geom_bar(position = "dodge") +  # "dodge" places bars side by side
  labs(
    title = "Distribution of Credit Risk (Good/Bad) by Loan Purpose",
    x = "Loan Purpose",
    y = "Count",
    fill = "Credit Risk"
  ) +
  scale_fill_manual(values = c("bad" = "red", "good" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(grouped_bar_plot)
ggsave("Grouped_Bar_Plot_Credit_Risk_by_Loan_Purpose.png", plot = grouped_bar_plot, width = 10, height = 6, dpi = 300)




#---Hypothesis Testing---

# Hypothesis 1: Association Between Loan Purpose and Credit Risk (Chi-squared test)
print("Hypothesis 1: Association Between Loan Purpose and Credit Risk")
chisq_test_result <- chisq.test(table(credit_data$purpose, credit_data$class))
print(paste("Hypothesis 1 p-value:", chisq_test_result$p.value))
if (chisq_test_result$p.value < 0.05) {
  print(paste("Reject the null hypothesis: p-value =", chisq_test_result$p.value, ". There is an association between loan purpose and credit risk."))
} else {
  print(paste("Failed to reject the null hypothesis: p-value =", chisq_test_result$p.value, ". No association between loan purpose and credit risk."))
}

# Visualizing chi-square test with mozaic plot
mosaic_plot <- ggplot(data = credit_data) +
  geom_mosaic(aes(x = product(purpose), fill = class), na.rm = TRUE) +
  labs(
    title = "Mosaic Plot: Loan Purpose vs Credit Risk",
    x = "Loan Purpose",
    y = "Proportion",
    fill = "Credit Risk"
  ) +
  scale_fill_manual(values = c("bad" = "red", "good" = "blue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


chisq_test_result <- chisq.test(table(credit_data$purpose, credit_data$class))
mosaic_plot <- mosaic_plot +
  labs(
    caption = paste("Chi-squared Test p-value:", chisq_test_result$p.value)
  )

print(mosaic_plot)
ggsave("Mosaic_Plot_Loan_purpose_vs_Credit_risk.png", plot = mosaic_plot, width = 10, height = 6, dpi = 300)




# Hypothesis 2: Relationship Between Loan Purpose and Credit Risk by Loan Amount (Logistic Regression)
print("Hypothesis 2: Relationship Between Loan Purpose, Loan Amount, and Credit Risk")
logit_model <- glm(class ~ purpose + credit_amount, family = binomial(link = "logit"), data = credit_data)
summary(logit_model)

purpose_p_values <- coef(summary(logit_model))[, "Pr(>|z|)"]
print(purpose_p_values)

if (any(purpose_p_values < 0.05)) {
  print("Reject the null hypothesis: There is a significant relationship between loan purpose and credit risk.")
} else {
  print("Failed to reject the null hypothesis: No significant relationship between loan purpose and credit risk.")
}


#Visualizing logistic regression

# Calculate predicted probabilities for each purpose
credit_data$predicted_prob <- predict(logit_model, type = "response")
predicted_summary <- credit_data %>%
  group_by(purpose) %>%
  summarize(
    mean_prob_good = mean(predicted_prob),
    .groups = "drop"
  )

# Bar plot for logistic regression
predicted_prob_plot <- ggplot(predicted_summary, aes(x = reorder(purpose, mean_prob_good), y = mean_prob_good, fill = mean_prob_good)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(
    title = "Predicted Probabilities of Good Credit Risk by Loan Purpose",
    x = "Loan Purpose",
    y = "Predicted Probability of Good Credit Risk"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(predicted_prob_plot)
ggsave("Predicted_Probabilities_by_Purpose.png", plot = predicted_prob_plot, width = 10, height = 6, dpi = 300)



#Objective 2: Investigating the relationship between age and credit risk.
# Verifying variables in the dataset
required_vars <- c("class", "age")
missing_vars <- setdiff(required_vars, names(credit_data))

if (length(missing_vars) > 0) {
  print(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
  stop("Required variables for this objective are missing in the dataset.")
} else {
  print("All required variables are present in the dataset: 'class', 'age'. Proceeding with analysis.")
}


#Rounding age data
credit_data$age <- round(credit_data$age)


#Frequency table for Credit Risk Classes 
print("Frequency Table for Credit Risk Classes:")
freq_table_age <- table(credit_data$age, credit_data$class)
print(freq_table_age)



# Visualizing the frequency table
freq_table_age_df <- as.data.frame(freq_table_age)
colnames(freq_table_age_df) <- c("age", "class", "Freq")

heatmap_plot <- ggplot(freq_table_age_df, aes(x = age, y = class, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Credit Risk by Age",
    x = "Age",
    y = "Credit Risk",
    fill = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(heatmap_plot)
ggsave("Heatmap_Credit_Risk_by_Age.png", plot = heatmap_plot, width = 10, height = 6, dpi = 300)



# Graph 1 Facet grid plot for age distribution by credit risk
facet_plot <- ggplot(credit_data, aes(x = age, fill = class)) +
  geom_histogram(binwidth = 1, alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "red", "good" = "green")) +
  labs(
    title = "Age Distribution by Credit Risk (Faceted)",
    x = "Age",
    y = "Frequency"
  ) +
  facet_wrap(~ class) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "none",
    text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

print(facet_plot)
ggsave("Facet_Plot_Age_Class.png", plot = facet_plot, width = 10, height = 6, dpi = 300)



# Graph 2 Histogram with density plot for age by credit risk
histogram_plot <- ggplot(credit_data, aes(x = age, fill = class)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +
  geom_density(aes(y = ..count..), alpha = 0.7) +
  scale_fill_manual(values = c("bad" = "red", "good" = "green")) +
  labs(
    title = "Histogram with Density Overlay of Age by Credit Risk",
    x = "Age",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "top",
    text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

print(histogram_plot)
ggsave("Histogram_Density_Age_Class.png", plot = histogram_plot, width = 10, height = 6, dpi = 300)



#---Hypothesis Testing---

# Hypothesis 1: Perform independent t-test for age by credit risk
t_test_result <- t.test(age ~ class, data = credit_data)

print("T-Test Results for Age by Credit Risk:")
print(t_test_result)


# Hypothesis 2: Chi-Squared Test for Independence
print("Hypothesis 2: Association between age groups and credit risk (class).")
credit_data$age_group <- cut(credit_data$age, breaks = 5) 
chisq_test_age <- chisq.test(table(credit_data$age_group, credit_data$class))
print(chisq_test_age)
if (chisq_test_age$p.value < 0.05) {
  print("Reject the null hypothesis: There is a significant association between age groups and credit risk.")
} else {
  print("Fail to reject the null hypothesis: No significant association between age groups and credit risk.")
}










# --- Sanjivan A/L Thiyageswaran (TP070073) ---
# Objective 3: Analyze the effect of financial standing on credit risk classification.

# Section 1: Validation
required_vars_sanjivan <- c("savings_status", "checking_status", "class")
if (!all(required_vars_sanjivan %in% names(credit_data))) {
  stop("Required variables are missing:", paste(setdiff(required_vars_sanjivan, names(credit_data)), collapse = ", "))
} else {
  message("All required variables for Objective 3 are present.")
}


# Section 2: Exploratory Visualizations

# (a) Analysis 1: Proportional Distribution of Credit Risk by Saving Status
savings_status_bar <- ggplot(credit_data, aes(x = savings_status, fill = class)) +  
  geom_bar(position = "fill") +
  labs(title = "Proportion of Credit Risk by Savings Status", x = "Savings Status", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("good" = "#00c9ff", "bad" = "#ff9b00")) + 
  theme_minimal()
print(savings_status_bar)
ggsave("Savings_Status_vs_Credit_Risk_Class_Bar.png", plot = savings_status_bar, width = 8, height = 6, dpi = 300)


# (b) Analysis 2: Density Distribution of Credit Risk by Checking Account Status 
credit_data$checking_status_numeric <- as.numeric(factor(credit_data$checking_status))
checking_status_density <- ggplot(credit_data, aes(x = checking_status_numeric, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density of Checking Status by Credit Risk Class",
    x = "Checking Status (Numeric Proxy)",
    y = "Density"
  ) +
  scale_fill_manual(values = c("good" = "#00c9ff", "bad" = "#ff00bd")) +
  theme_minimal()
print(checking_status_density)
ggsave("Density_Plot_Checking_Status.png", plot = checking_status_density, width = 8, height = 6, dpi = 300)


# Section 3: Hypothesis Testing with Conclusions


# (a) Hypothesis 1: Savings Status and Its Effect on Credit Risk Classification
cat("Hypothesis 1: Savings Status and Credit Risk")

# Perform T-Test
credit_data$savings_status_numeric <- as.numeric(factor(credit_data$savings_status))
t_test_savings <- t.test(savings_status_numeric ~ class, data = credit_data)
print(t_test_savings)

if (t_test_savings$p.value < 0.05) {
  print("Conclusion: Reject the null hypothesis (H0). Savings status significantly differs by credit risk class.")
} else {
  print("Conclusion: Fail to reject the null hypothesis (H0). No significant difference in savings status by credit risk class.")
}

# Perform GLM
glm_savings <- glm(class ~ savings_status, family = binomial, data = credit_data)
print(summary(glm_savings))

if (summary(glm_savings)$coefficients[2, 4] < 0.05) {
  print("Conclusion (GLM): Reject the null hypothesis. Savings status significantly affects credit risk classification.")
} else {
  print("Conclusion (GLM): Fail to reject the null hypothesis. Savings status does not significantly affect credit risk classification.")
}


# (b) Hypothesis 2: Checking Status and Its Effect on Credit Risk Classification
cat("Hypothesis 2: Checking Status and Credit Risk")

# Perform T-Test
credit_data$checking_status_numeric <- as.numeric(factor(credit_data$checking_status))
t_test_checking <- t.test(checking_status_numeric ~ class, data = credit_data)
print(t_test_checking)

if (t_test_checking$p.value < 0.05) {
  print("Conclusion: Reject the null hypothesis (H0). Checking status significantly differs by credit risk class.")
} else {
  print("Conclusion: Fail to reject the null hypothesis (H0). No significant difference in checking status by credit risk class.")
}

# Perform GLM
glm_checking <- glm(class ~ checking_status, family = binomial, data = credit_data)
print(summary(glm_checking))

if (summary(glm_checking)$coefficients[2, 4] < 0.05) {
  print("Conclusion (GLM): Reject the null hypothesis. Checking status significantly affects credit risk classification.")
} else {
  print("Conclusion (GLM): Fail to reject the null hypothesis. Checking status does not significantly affect credit risk classification.")
}





# --- Sanjivan A/L Thiyageswaran (TP070073) ---
# Objective 4: Analyze the impact of repayment duration on credit risk classification.

# Section 1: Validation
required_vars_duration <- c("duration", "class")
if (!all(required_vars_duration %in% names(credit_data))) {
  stop("Required variables are missing:", paste(setdiff(required_vars_duration, names(credit_data)), collapse = ", "))
} else {
  message("All required variables for Objective 4 are present.")
}

# Section 2: Exploratory Visualizations

# (a) Analysis: Density Distribution of Repayment Duration by Credit Risk Class
duration_density_plot <- ggplot(credit_data, aes(x = duration, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density of Repayment Duration by Credit Risk Class",
    x = "Repayment Duration (Months)",
    y = "Density"
  ) +
  scale_fill_manual(values = c("good" = "#00c9ff", "bad" = "#ff00bd")) +
  theme_minimal()
print(duration_density_plot)
ggsave("Duration_Density_Plot.png", plot = duration_density_plot, width = 8, height = 6, dpi = 300)

# Section 3: Hypothesis Testing with Conclusions

# (a) Hypothesis 1: Duration and Credit Risk Class
cat("\n=== Hypothesis 1: Repayment Duration and Credit Risk ===\n")

# Perform T-Test
t_test_duration <- t.test(duration ~ class, data = credit_data)
print(t_test_duration)

if (t_test_duration$p.value < 0.05) {
  print("Conclusion: Reject the null hypothesis (H0). Repayment duration significantly differs by credit risk class.")
} else {
  print("Conclusion: Fail to reject the null hypothesis (H0). No significant difference in repayment duration by credit risk class.")
}

# Perform GLM
glm_duration <- glm(class ~ duration, family = binomial, data = credit_data)
print(summary(glm_duration))

if (summary(glm_duration)$coefficients[2, 4] < 0.05) {
  print("Conclusion (GLM): Reject the null hypothesis. Repayment duration significantly affects credit risk classification.")
} else {
  print("Conclusion (GLM): Fail to reject the null hypothesis. Repayment duration does not significantly affect credit risk classification.")
}









# --- Fareez Daniel bin Rozaime (TP077930) ---
# Objective 5: Examine the impact of loan amount on the likelihood of credit risk.

# Load required libraries
library(ggplot2)
library(caret)
library(dplyr)

# Verify required variables
required_vars <- c("credit_amount", "class")
if (!all(required_vars %in% names(credit_data))) {
  stop(paste("Required variables are missing:", 
             paste(setdiff(required_vars, names(credit_data)), collapse = ", ")))
} else {
  message("All required variables are present.")
}

# Section 1: Visualizations
# 1(a) Proportional Bar Chart for Loan Amount by Credit Risk Class
print("Creating proportional bar chart...\n")
loan_amount_grouped <- credit_data %>%
  group_by(class) %>%
  summarise(total_loan_amount = sum(credit_amount, na.rm = TRUE))

proportional_bar_chart <- ggplot(loan_amount_grouped, aes(x = class, y = total_loan_amount, fill = class)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 1.2) +
  geom_text(aes(label = scales::comma(total_loan_amount)), vjust = -0.4, size = 5, fontface = "bold") +
  labs(
    title = "Total Loan Amount by Credit Risk Class",
    subtitle = "Visualizing Good vs Bad Credit Risk Through Loan Amounts",
    x = "Credit Risk Class", y = "Total Loan Amount",
    caption = "Data Source: Credit Data | Visualization by Fareez Daniel",
    fill = "Credit Risk Class"
  ) +
  scale_fill_manual(values = c("good" = "#4DB6AC", "bad" = "#FF7043")) +
  scale_y_continuous(labels = scales::comma) +  # Add comma formatting for y-axis
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5, face = "italic"))
ggsave("proportional_bar_chart_fixed.png", plot = proportional_bar_chart, width = 12, height = 8, dpi = 300)
print(proportional_bar_chart)


# 1(b) Violin Plot for Loan Amount Distribution
print("Creating violin plot...\n")
violin_plot <- ggplot(credit_data, aes(x = class, y = credit_amount, fill = class)) +
  geom_violin(trim = TRUE, color = "black", size = 1.2, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "black") +
  labs(
    title = "Loan Amount Distribution by Credit Risk Class",
    subtitle = "Violin plot highlighting data density with mean markers",
    x = "Credit Risk Class", y = "Loan Amount",
    caption = "Data Source: Credit Data | Visualization by Fareez Daniel"
  ) +
  scale_fill_manual(values = c("good" = "#4DB6AC", "bad" = "#FF7043")) +
  theme_minimal(base_size = 15)
ggsave("violin_plot.png", plot = violin_plot, width = 12, height = 8, dpi = 300)
print(violin_plot)

# 1(c) Density Plot: Loan Amount Distribution by Credit Risk Class
print("Creating density plot...\n")
density_plot <- ggplot(credit_data, aes(x = credit_amount, fill = class)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Loan Amount Distribution by Credit Risk Class",
    subtitle = "Density plot (area under curve = 1) showing loan amount distribution",
    x = "Loan Amount",
    y = "Density",
    fill = "Credit Risk Class"
  ) +
  scale_fill_manual(values = c("good" = "#4DB6AC", "bad" = "#FF7043")) +
  scale_y_continuous(labels = scales::comma) +  # Updated y-axis for better readability
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top"
  )
ggsave("density_plot.png", plot = density_plot, width = 10, height = 6, dpi = 300)
print(density_plot)

# --- Section 2: Hypothesis Testing ---
# Hypothesis 1: Loan Amount and Credit Risk Class (T-Test)

print("Performing T-Test for Loan Amount by Credit Risk Class...\n")
t_test_result <- t.test(credit_amount ~ class, data = credit_data)
print("T-Test Results:")
print(t_test_result)
if (t_test_result$p.value < 0.05) {
  print("Conclusion: Reject the null hypothesis. Loan amounts significantly differ between credit risk classes.")
} else {
  print("Conclusion: Fail to reject the null hypothesis. No significant difference in loan amounts between credit risk classes.")
}
print("Creating boxplot for T-Test results...\n")
t_test_result_chart <- ggplot(credit_data, aes(x = class, y = credit_amount, fill = class)) +
  geom_boxplot(outlier.shape = 16, outlier.color = "black", size = 1.2) +  # Boxplot with outliers styled
  labs(
    title = "Boxplot for Loan Amounts by Credit Risk Class",
    subtitle = "Visualizing Loan Amount Differences Between Good and Bad Credit Risk",
    x = "Credit Risk Class",
    y = "Loan Amount",
    fill = "Credit Risk Class"
  ) +
  scale_fill_manual(values = c("good" = "#4DB6AC", "bad" = "#FF7043")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top"
  )
ggsave("t_test_result_chart.png", plot = t_test_result_chart, width = 10, height = 6, dpi = 300)
print(t_test_result_chart)

# Section 3: Logistic Regression Model
# Prepare Data for Logistic Regression
credit_data$class_binary <- ifelse(credit_data$class == "good", 0, 1)

# Split data
set.seed(123)
train_index <- createDataPartition(credit_data$class_binary, p = 0.8, list = FALSE)
train_data <- credit_data[train_index, ]
test_data <- credit_data[-train_index, ]

# Build Logistic Regression Model
print("Building logistic regression model...\n")
logit_model <- glm(class_binary ~ credit_amount, family = binomial, data = train_data)
print(summary(logit_model))

# Visualize Logistic Regression
logit_model_plot <- ggplot(test_data, aes(x = credit_amount, y = predict(logit_model, newdata = test_data, type = "response"))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  labs(
    title = "Logistic Regression: Loan Amount vs Predicted Probability",
    x = "Loan Amount",
    y = "Predicted Probability of Bad Credit Risk"
  ) +
  theme_minimal()
ggsave("logit_model_plot.png", plot = logit_model_plot, width = 10, height = 6, dpi = 300)
print(logit_model_plot)



# --- Fareez Daniel bin Rozaime (TP077930) ---
# Objective 6: Analyze the effect of the number of dependents on credit risk classification.

# Factorizing age, class, and purpose if needed
credit_data$num_dependants <- as.factor(credit_data$num_dependants)


# Load required libraries
library(ggplot2)
library(dplyr)

# Verify required variables
required_vars <- c("num_dependants", "class")
if (!all(required_vars %in% names(credit_data))) {
  stop(paste("Required variables are missing:", 
             paste(setdiff(required_vars, names(credit_data)), collapse = ", ")))
} else {
  message("All required variables are present.")
}

# Transform 'num_dependants' to binned categories
credit_data$num_dependants_grouped <- cut(
  as.numeric(credit_data$num_dependants),  # Ensure the column is numeric
  breaks = c(-Inf, 1, 3, Inf),             # Define ranges: 0-1, 2-3, 4+
  labels = c("0-1", "2-3", "4+"),          # Group labels
  right = TRUE                             # Include the upper limit
)



# Section 1: Visualizations
# 1(a) Bar Chart: Distribution of Number of Dependents by Credit Risk Class
bar_chart_dependents <- ggplot(credit_data, aes(x = num_dependants_grouped, fill = class)) +  
  geom_bar(position = "dodge", color = "black", size = 1.2) +
  labs(
    title = "Distribution of Number of Dependents by Credit Risk Class",
    subtitle = "Comparison of good vs bad credit risk based on grouped dependents",
    x = "Number of Dependents (Grouped)",
    y = "Count",
    fill = "Credit Risk Class"
  ) +
  scale_fill_manual(values = c("good" = "#4DB6AC", "bad" = "#FF7043")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    legend.position = "right"               # Adjust legend position
  )

# Save and print the updated bar chart
ggsave("bar_chart_dependents.png", plot = bar_chart_dependents, width = 12, height = 8, dpi = 300)
print(bar_chart_dependents)

# 1(b) Proportional Bar Chart: Number of Dependents Grouped by Credit Risk Class
print("Creating proportional bar chart for number of dependents grouped by credit risk class...\n")

proportional_bar_dependents <- ggplot(credit_data, aes(x = num_dependants_grouped, fill = class)) +
  geom_bar(position = "fill", color = "black", size = 1.2) +
  labs(
    title = "Proportion of Credit Risk by Number of Dependents (Grouped)",
    subtitle = "Proportional distribution of credit risk across grouped dependents",
    x = "Number of Dependents (Grouped)",
    y = "Proportion",
    fill = "Credit Risk Class"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  
  scale_fill_manual(values = c("good" = "#4DB6AC", "bad" = "#FF7043")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(size = 12, angle = 0), 
    axis.text.y = element_text(size = 12),  
    axis.title = element_text(size = 14),
    legend.position = "right"  
  )
ggsave("proportional_bar_dependents_grouped.png", plot = proportional_bar_dependents, width = 12, height = 8, dpi = 300)
print(proportional_bar_dependents)


# Section 2: Hypothesis Testing
# Hypothesis: The number of dependents significantly affects credit risk classification.

print("Performing Chi-Square Test for Number of Dependents and Credit Risk Class...\n")
if ("num_dependants" %in% names(credit_data)) {
  # Create a contingency table
  contingency_table <- table(credit_data$num_dependants, credit_data$class)
  
  # Perform Chi-Square Test
  chi_square_result <- chisq.test(contingency_table)
  
  # Print test result
  print("Chi-Square Test Results:")
  print(chi_square_result)
  
  # Interpretation
  if (chi_square_result$p.value < 0.05) {
    print("Conclusion: Reject the null hypothesis. Number of dependents is significantly associated with credit risk classification.")
  } else {
    print("Conclusion: Fail to reject the null hypothesis. Number of dependents is not significantly associated with credit risk classification.")
  }
} else {
  print("Variable 'num_dependants' is not available in the dataset. Skipping Chi-Square Test...")
}

# Section 3: Logistic Regression Model

credit_data$num_dependants_grouped <- cut(
  credit_data$num_dependants,
  breaks = c(-Inf, 1, 3, Inf),
  labels = c("0-1", "2-3", "4+")
)

print("Building logistic regression model with grouped dependents ...\n")
logit_model_dependents_grouped <- glm(class_binary ~ num_dependants_grouped, family = binomial, data = credit_data)

print(summary(logit_model_dependents_grouped))

print("Visualizing logistic regression predictions with grouped dependents ...\n")
logit_model_dependents_plot_grouped <- ggplot(credit_data, aes(x = num_dependants_grouped, y = predict(logit_model_dependents_grouped, type = "response"))) +
  geom_point(aes(color = class), alpha = 0.6, size = 3) +  
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue", size = 1.2) + 
  labs(
    title = "Logistic Regression: Grouped Dependents vs Predicted Probability",
    subtitle = "Relationship between grouped dependents and probability of bad credit risk",
    x = "Number of Dependents (Grouped)",
    y = "Predicted Probability of Bad Credit Risk",
    color = "Credit Risk Class"
  ) +
  scale_color_manual(values = c("bad" = "#FF7043", "good" = "#4DB6AC")) + 
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, face = "italic"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

ggsave("logit_model_dependents_grouped_plot.png", plot = logit_model_dependents_plot_grouped, width = 12, height = 8, dpi = 300)
print(logit_model_dependents_plot_grouped)









# --- Group Complex Hypothesis ---

# Group Hypothesis: Loan purpose, financial standing, loan amount, duration, number of dependents, and age together significantly influence the likelihood of being classified as high risk.
print("Testing Group Hypothesis: Loan purpose, financial standing, loan amount, duration, number of dependents, and age together significantly influence the likelihood of being classified as high risk.")

# Logistic regression model with variables from all objectives
logit_model_group_full <- glm(class ~ purpose + savings_status + checking_status + credit_amount + duration + num_dependants + age, 
                              family = binomial, data = credit_data)

# Display model summary
print("Logistic Regression Model Summary for Group Hypothesis:")
summary(logit_model_group_full)

# Conclusion based on model coefficients
significant_coeffs_full <- coef(summary(logit_model_group_full))[,"Pr(>|z|)"] < 0.05 
if (any(significant_coeffs_full[-1])) {  # Exclude intercept
  print("Reject the null hypothesis: Loan purpose, financial standing, loan amount, duration, number of dependents, and age together significantly influence credit risk classification.")
} else {
  print("Fail to reject the null hypothesis: No significant combined influence of loan purpose, financial standing, loan amount, duration, number of dependents, and age on credit risk classification.")
}




# Save and Document Results
write.csv(credit_data, "cleaned_credit_data.csv")
print("Cleaned dataset saved as 'cleaned_credit_data.csv'.")