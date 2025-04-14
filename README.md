# Credit-Risk-Classification-Project
Project Overview
This project is an end-to-end analysis of a credit risk dataset aimed at identifying key factors that influence credit risk classification. The analysis is implemented in R and includes extensive data cleaning, exploratory data analysis, visualization, and statistical hypothesis testing.

Key Features
Data Preparation and Cleaning

Package Management: Utilizes the pacman package to load required libraries.

Missing Value Imputation: Implements multiple imputation using the MICE package to handle missing data.

Data Validation: Checks for required columns and ensures that categorical variables are correctly formatted as factors.

Duplicate Removal: Identifies and removes duplicate rows.

Exploratory Data Analysis (EDA)

Data Summary: Provides a summary and structure of the cleaned dataset.

Visualizations:

Histograms, density plots, and boxplots to visualize the distribution of credit amount, age, and other variables.

Mosaic and bar plots to show relationships between credit risk and factors like loan purpose, savings status, and checking status.

Objective-Specific Analyses

Objective 1: Examines the relationship between loan purpose and credit risk using bar plots and logistic regression.

Objective 2: Investigates the impact of age on credit risk through histograms, heatmaps, and hypothesis tests (t-test and chi-square test).

Objective 3: Analyzes how financial standing (savings and checking status) affects credit risk using proportional plots and generalized linear models.

Objective 4: Explores the influence of repayment duration on credit risk with density plots and statistical tests.

Objective 5: Evaluates the effect of loan amount on credit risk through various visualizations (bar charts, violin plots, and density plots) and logistic regression.

Objective 6: Studies the impact of the number of dependents on credit risk classification, including grouping, bar charts, and logistic regression analysis.

Hypothesis Testing and Modeling

Uses multiple statistical tests (t-test, chi-square test) to assess the significance of each factor.

Builds logistic regression models to quantify the influence of individual and grouped predictors on the likelihood of credit risk classification.

Tests a combined model to evaluate the joint effect of loan purpose, financial standing, loan amount, repayment duration, number of dependents, and age.

Output and Documentation

All generated plots are saved as image files for reference.

The cleaned dataset is exported as a CSV file (cleaned_credit_data.csv) for further analysis or reporting.

The script includes detailed comments to explain each step of the process, making it accessible for future users or collaborators.

How to Run
Clone the Repository:

bash
Copy
git clone https://github.com/your-username/your-repo-name.git
cd your-repo-name
Install Required Packages: Ensure you have R installed. The script uses the pacman package to load all required libraries. If any packages are missing, they will be automatically installed.

Set Working Directory: Modify the working directory in the script (using setwd()) to point to your local project folder.

Run the Analysis: Execute the R script in your R environment or IDE (e.g., RStudio). The script will perform data cleaning, exploratory analysis, visualization, and hypothesis testing.

Conclusion
This project provides a comprehensive analysis of credit risk factors using a variety of statistical and machine learning techniques. It serves as a robust framework for understanding the influences on credit risk and can be further extended or adapted to similar datasets.


