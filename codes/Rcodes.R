# Load the dataset from the specified path
df <- read.csv("E:/1 PROJECT/MHIA/306 statistics/Assignment 3/Heart_Disease_Prediction.csv")

# Data cleaning
# 1. remove spaces for all column names in Excel 
# 2. rearrange all columns with continuous variable to the back in Excel 

# 3. assign unique ID to each rows
data <- data.frame(ID = seq_len(nrow(df)), df)

# 4. Check duplicates
duplicates <- data[duplicated(data), ]

# 5.Check and address OUTLIERS
# Define the variables of interest
columns_to_check <- c("BP", "Cholesterol", "MaxHR", "Stdepression", "Numberofvesselsfluro")

# Create a function to calculate % outliers
calculate_percentiles_and_percentage <- function(data, variable) {
  variable_data <- data[[variable]]
  non_missing_values <- variable_data[!is.na(variable_data)]
  
  q01 <- quantile(non_missing_values, 0.01)
  q99 <- quantile(non_missing_values, 0.99)
  
  below_percentile_01 <- sum(variable_data < q01, na.rm = TRUE)
  above_percentile_99 <- sum(variable_data > q99, na.rm = TRUE)
  total_values <- length(non_missing_values)
  
  cat("Variable:", variable, "\n")
  cat("0.01 Percentile:", q01, "\n")
  cat("0.99 Percentile:", q99, "\n")
  cat("Percentage of values below 0.01 percentile:", (below_percentile_01 / total_values) * 100, "%\n")
  cat("Percentage of values above 0.99 percentile:", (above_percentile_99 / total_values) * 100, "%\n")
  cat("\n")
}
# Calculate percentiles and percentages for each variable
for (column in columns_to_check) {
  calculate_percentiles_and_percentage(data, column)
}

# WINSORIZE outliers to 99th percentile value 
winsorize_data <- data
winsorize_data$Cholesterol[winsorize_data$Cholesterol >= 407] <- 407
winsorize_data$Stdepression[winsorize_data$Stdepression >= 4.2] <- 4.2

# BP, MaxHR had natural outliers,hence not treated. 

# 6. NORMALITY TEST
columns_to_check <- c("Age", "BP", "Cholesterol", "MaxHR", "Stdepression", "Numberofvesselsfluro")

# Perform the Kolmogorov-Smirnov normality test for each variable
for (column in columns_to_check) {
  test_result <- ks.test(winsorize_data[[column]], "pnorm", mean = mean(winsorize_data[[column]]), sd = sd(winsorize_data[[column]]))
  
  cat("Variable:", column, "\n")
  cat("KS Statistic:", test_result$statistic, "\n")
  cat("P-value:", test_result$p.value, "\n")
  
  # Check for significance (e.g., alpha = 0.05)
  if (test_result$p.value < 0.05) {
    cat("The variable is not normally distributed.\n")
  } else {
    cat("The variable appears to be normally distributed.\n")
  }
  
  cat("\n")
}

# MaxHR and Cholesterol is normally distributed
# BP, Stdepression, Numberofvesselsfluro = data skewed


# 7. Encoding (create dummy variables) 
dummy_data <- winsorize_data

dummy_data$typicalAngina <- ifelse(winsorize_data$Chestpaintype == 1, 1, 0)
dummy_data$atypicalAngina <- ifelse(winsorize_data$Chestpaintype == 2, 1, 0)
dummy_data$nonAnginaPain <- ifelse(winsorize_data$Chestpaintype == 3, 1, 0)
dummy_data$noPain <- ifelse(winsorize_data$Chestpaintype == 3, 1, 0)
dummy_data$anyPain <- ifelse(winsorize_data$Chestpain %in% c(1, 2, 3), 1, 0)

dummy_data$ST.T_EKG <- ifelse(winsorize_data$EKGresults == 1, 1, 0)
dummy_data$LVH_EKG <- ifelse(winsorize_data$EKGresults == 2, 1, 0)
dummy_data$abnormal_EKG <- ifelse(winsorize_data$EKGresults %in% c(1, 2), 1, 0)

dummy_data$upsloping_ST <- ifelse(winsorize_data$SlopeofST == 1, 1, 0)
dummy_data$flat_ST <- ifelse(winsorize_data$SlopeofST == 2, 1, 0)
dummy_data$downsloping_ST <- ifelse(winsorize_data$SlopeofST == 3, 1, 0)

dummy_data$fixed_thallium <- ifelse(winsorize_data$Thallium == 6, 1, 0)
dummy_data$reversible_thallium <- ifelse(winsorize_data$Thallium == 7, 1, 0)
dummy_data$abnormal_thallium <- ifelse(winsorize_data$Thallium %in% c(6, 7), 1, 0)

cleaned_data <- dummy_data
cleaned_data$HeartDisease <- ifelse(dummy_data$HeartDisease == "Presence", 1, 0)

# Save cleaned_data to a CSV file
#write.csv(cleaned_data, file = "E:/1 PROJECT/MHIA/306 statistics/Assignment 3/cleaned_data.csv", row.names = FALSE)

# Descriptive statistics 
heart_disease_frequency <- table(cleaned_data$HeartDisease)
cat("Frequency of HeartDisease:\n")
print(heart_disease_frequency)

sex_frequency <- table(cleaned_data$Sex)
cat("Frequency of HeartDisease:\n")
print(sex_frequency)

# Compare Continuous variable  (Group 0= Absence of Heart disease, Group 1= Presence)

# Divide the data into two groups based on 'HeartDisease'
group_0 <- cleaned_data[cleaned_data$HeartDisease == 0, ]
group_1 <- cleaned_data[cleaned_data$HeartDisease == 1, ]

variables_to_analyze <- c("Age","BP", "Cholesterol", "MaxHR", "Stdepression", "Numberofvesselsfluro")

# Create a function to calculate statistics
calculate_statistics <- function(data, variable) {
  cat("Variable:", variable, "\n")
  cat("Mean:", mean(data[[variable]]), "\n")
  cat("Standard Deviation:", sd(data[[variable]]), "\n")
  cat("Median (50th percentile):", median(data[[variable]]), "\n")
  cat("25th Percentile:", quantile(data[[variable]], 0.25), "\n")
  cat("75th Percentile:", quantile(data[[variable]], 0.75), "\n")
  cat("\n")
}

# Calculate statistics for each variable in both groups
for (variable in variables_to_analyze) {
  cat("Group 0 Statistics:\n")
  calculate_statistics(group_0, variable)
  
  cat("Group 1 Statistics:\n")
  calculate_statistics(group_1, variable)
}

library(dplyr)
# Divide the data into two groups based on 'HeartDisease'
group_0 <- cleaned_data[cleaned_data$HeartDisease == 0, ]
group_1 <- cleaned_data[cleaned_data$HeartDisease == 1, ]

# Inferentials stats
# Variables for t-tests
t_test_vars <- c("Age", "MaxHR", "Cholesterol")

# Variables for Mann-Whitney U tests
mannwhitneyu_vars <- c("BP", "Stdepression", "Numberofvesselsfluro")

# Perform independent t-tests
for (var in t_test_vars) {
  t_test_result <- t.test(group_0[[var]], group_1[[var]])
  
  cat("Independent t-test for", var, "\n")
  cat("p-value:", t_test_result$p.value, "\n")
  cat("\n")
}

# Perform Mann-Whitney U tests
for (var in mannwhitneyu_vars) {
  u_test_result <- wilcox.test(cleaned_data[[var]] ~ cleaned_data$HeartDisease, data = cleaned_data)
  cat("Mann-Whitney U test for", var, "\n")
  cat("p-value:", u_test_result$p.value, "\n")
  cat("\n")
}

# COMPARE categorical variables 
# Variables to analyze
categorical_vars <- c("Sex", "FBSover120", "Exerciseangina", "typicalAngina", "atypicalAngina", 
                      "nonAnginaPain", "noPain", "anyPain", "ST.T_EKG", "LVH_EKG", "abnormal_EKG", 
                      "upsloping_ST", "flat_ST", "downsloping_ST", "fixed_thallium", 
                      "reversible_thallium", "abnormal_thallium")

# Function to calculate frequency and percentage
calculate_frequency_percentage <- function(data, variable) {
  cat("Variable:", variable, "\n")
  
  # Frequency table
  freq_table <- table(data[[variable]])
  print(freq_table)
  
  # Percentage
  percentage <- prop.table(freq_table) * 100
  print(percentage)
  
  cat("\n")
}

# Calculate frequency and percentage for each variable in both groups
for (var in categorical_vars) {
  cat("Group 0 -", var, "\n")
  calculate_frequency_percentage(group_0, var)
  
  cat("Group 1 -", var, "\n")
  calculate_frequency_percentage(group_1, var)
}

# chi-square test
perform_chi_square_test <- function(data, variable) {
  contingency_table <- table(data$HeartDisease, data[[variable]])
  
  cat("Chi-Square Test for", variable, "\n")
  chi_square_result <- chisq.test(contingency_table)
  print(chi_square_result)
  cat("\n")
}

perform_chi_square_test <- function(data, variable) {
  contingency_table <- table(data$HeartDisease, data[[variable]])
  
  cat("Chi-Square Test for", variable, "\n")
  chi_square_result <- chisq.test(contingency_table)
  print(chi_square_result)
  cat("\n")
}

# Perform chi-square tests for each variable
for (var in categorical_vars) {
  perform_chi_square_test(cleaned_data, var)
}

# LOGISTIC REGRESSION
library(caret)

# Set the seed for reproducibility
set.seed(123)

# 1. Divide data into train:test (70:30)
splitIndex <- createDataPartition(cleaned_data$HeartDisease, p = 0.7, list = FALSE)
train_data <- cleaned_data[splitIndex, ]
test_data <- cleaned_data[-splitIndex, ]

# Variables to include in univariate logistic regression
variables_to_include <- c("Age", "Sex", "Exerciseangina", "BP", "MaxHR", "Stdepression",
                          "atypicalAngina", "nonAnginaPain", "anyPain", "LVH_EKG",
                          "abnormal_EKG", "flat_ST", "upsloping_ST", "abnormal_thallium",
                          "reversible_thallium", "Cholesterol", "Numberofvesselsfluro")

# Loop through variables and build univariate logistic regression models
for (variable in variables_to_include) {
  formula <- as.formula(paste("HeartDisease ~", variable))
  model <- glm(formula, data = train_data, family = "binomial")
  
  cat("Variable:", variable, "\n")
  cat("Summary:\n")
  print(summary(model))
  cat("\n\n")
}

# multivariate logistic regression 
library(ResourceSelection)

# Set the seed for reproducibility
set.seed(123)

# 1. Divide data into train:test (70:30)
splitIndex <- createDataPartition(cleaned_data$HeartDisease, p = 0.7, list = FALSE)
train_data <- cleaned_data[splitIndex, ]
test_data <- cleaned_data[-splitIndex, ]

# 2. Build multivariate logistic regression with all specified variables
variables_to_include <- c("Age", "Sex", "Exerciseangina", "BP", "MaxHR", "Stdepression",
                          "atypicalAngina", "nonAnginaPain", "anyPain", "LVH_EKG",
                          "abnormal_EKG", "flat_ST", "upsloping_ST", "abnormal_thallium",
                          "reversible_thallium", "Cholesterol", "Numberofvesselsfluro")

formula_multivariate <- as.formula(paste("HeartDisease ~", paste(variables_to_include, collapse = " + ")))
model_multivariate <- glm(formula_multivariate, data = train_data, family = "binomial")

# 3. Print summary of the model
summary(model_multivariate)

# RETRAIN with significant features 
# Set the seed for reproducibility
set.seed(123)

# 1. Divide data into train:test (70:30)
splitIndex <- createDataPartition(cleaned_data$HeartDisease, p = 0.7, list = FALSE)
train_data <- cleaned_data[splitIndex, ]
test_data <- cleaned_data[-splitIndex, ]

# 2. Build multivariate logistic regression with all specified variables
variables_to_include <- c("Sex", "BP", "MaxHR", "flat_ST", "reversible_thallium", "Numberofvesselsfluro")

formula_multivariate <- as.formula(paste("HeartDisease ~", paste(variables_to_include, collapse = " + ")))
model_multivariate <- glm(formula_multivariate, data = train_data, family = "binomial")

# 3. Print summary of the model
summary(model_multivariate)

# 4. Hosmer–Lemeshow test
hl_test <- hoslem.test(model_multivariate$y, fitted(model_multivariate), g = 10)
cat("Hosmer–Lemeshow test:\n")
print(hl_test)

# 5. Nagelkerke R Square
nagelkerke_r2 <- 1 - (model_multivariate$deviance / model_multivariate$null.deviance)
cat("Nagelkerke R Square:", nagelkerke_r2, "\n")

# INTERACTION between variables 
library(gplots)

# Select relevant variables
variables_for_correlation <- c("Sex", "BP", "MaxHR", "flat_ST", "reversible_thallium", "HeartDisease", "Numberofvesselsfluro")

# Subset the data with selected variables
cor_data <- cleaned_data[variables_for_correlation]

# Perform Spearman rank correlation
cor_matrix <- cor(cor_data, method = "spearman")

# Print correlation matrix
cat("Spearman Rank Correlation Matrix:\n")
print(cor_matrix)

# Create a heatmap
heatmap.2(cor_matrix,
          col = colorRampPalette(c("blue", "white", "red"))(100),
          main = "Spearman Rank Correlation Heatmap",
          xlab = "",
          ylab = "",
          trace = "none",
          density.info = "none",
          key = TRUE,
          key.title = "Correlation",
          key.title.cex = 0.6,
          key.title.position = "bottom",
          symkey = FALSE,
          cellnote = round(cor_matrix, 2),
          notecol = "black",
          cexRow = 0.7,  # Adjust the size of row labels
          cexCol = 0.7,  # Adjust the size of column labels
          margins = c(12, 12))  # Adjust the margins to fit the labels

# 2-variable mode (Age, cholesterol model) 
# Set the seed for reproducibility
set.seed(123)

# 1. Divide data into train:test (70:30)
splitIndex <- createDataPartition(cleaned_data$HeartDisease, p = 0.7, list = FALSE)
train_data <- cleaned_data[splitIndex, ]
test_data <- cleaned_data[-splitIndex, ]

# 2. Build multivariate logistic regression with all specified variables
variables_to_include <- c("Age", "Cholesterol")

formula_multivariate <- as.formula(paste("HeartDisease ~", paste(variables_to_include, collapse = " + ")))
model_multivariate <- glm(formula_multivariate, data = train_data, family = "binomial")

# 3. Print summary of the model
summary(model_multivariate)

# 4. Hosmer–Lemeshow test
hl_test <- hoslem.test(model_multivariate$y, fitted(model_multivariate), g = 10)
cat("Hosmer–Lemeshow test:\n")
print(hl_test)

# 5. Nagelkerke R Square
nagelkerke_r2 <- 1 - (model_multivariate$deviance / model_multivariate$null.deviance)
cat("Nagelkerke R Square:", nagelkerke_r2, "\n")

# 4. Make predictions on the test set
predictions <- predict(model_multivariate, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# 5. Calculate accuracy
accuracy <- mean(predicted_classes == test_data$HeartDisease)
cat("Accuracy:", accuracy, "\n")

# 7. Calculate sensitivity, specificity, PPV, NPV, and F1 score
conf_matrix <- table(Actual = test_data$HeartDisease, Predicted = predicted_classes)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
ppv <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
npv <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
f1_score <- 2 * (ppv * sensitivity) / (ppv + sensitivity)

cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("PPV:", ppv, "\n")
cat("NPV:", npv, "\n")
cat("F1 Score:", f1_score, "\n")

# Question 4: Comparing risk factors between age group, sex group 
# 1. Divide into 3 groups according to Age
cleaned_data$AgeGroup <- cut(cleaned_data$Age, breaks = c(-Inf, 40, 60, Inf), labels = c("A", "B", "C"))

group_A <- cleaned_data[cleaned_data$Age < 40, ]
group_B <- cleaned_data[cleaned_data$Age >= 40 & cleaned_data$Age < 60, ]
group_C <- cleaned_data[cleaned_data$Age >= 60, ]

AgeGroup_frequency <- table(cleaned_data$AgeGroup)
cat("Frequency of HeartDisease:\n")
print(AgeGroup_frequency)

# 2. Compare mean, standard deviation, median, 25th percentile, 75th percentile for "BP", "MaxHR", No. vessels
variables_to_compare <- c("BP", "MaxHR", "Numberofvesselsfluro")

summary_stats <- function(data, variable) {
  cat("Summary statistics for", variable, "\n")
  cat("Group Mean:", mean(data[[variable]]), "\n")
  cat("Group SD:", sd(data[[variable]]), "\n")
  cat("Group Median:", median(data[[variable]]), "\n")
  cat("Group 25th Percentile:", quantile(data[[variable]], 0.25), "\n")
  cat("Group 75th Percentile:", quantile(data[[variable]], 0.75), "\n\n")
}

for (variable in variables_to_compare) {
  summary_stats(group_A, variable)
  summary_stats(group_B, variable)
  summary_stats(group_C, variable)
}

# 3. Compare frequency and percentage for "Sex", "flat_ST", "reversible_thallium"
categorical_vars <- c("Sex", "flat_ST", "reversible_thallium")

frequency_table <- function(data, variable) {
  cat("Frequency table for", variable, "\n")
  table_data <- table(data[[variable]])
  print(table_data)
  cat("Percentage table for", variable, "\n")
  prop_table <- prop.table(table_data) * 100
  print(prop_table)
  cat("\n")
}

for (variable in categorical_vars) {
  frequency_table(group_A, variable)
  frequency_table(group_B, variable)
  frequency_table(group_C, variable)
}

# 4. Perform one-way ANOVA test for MaxHR between the groups
cat("One-way ANOVA for MaxHR between groups:\n")
anova_result <- aov(MaxHR ~ factor(AgeGroup), data = cleaned_data)
summary(anova_result)

# Tukey post hoc test for pairwise comparison of MaxHR between the groups
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# 5. Perform Kruskal–Wallis H test for BP, no, vessels between the groups
cat("Kruskal–Wallis H test for BP between groups:\n")
kruskal_result <- kruskal.test(BP ~ factor(AgeGroup), data = cleaned_data)
print(kruskal_result)

cat("Kruskal–Wallis H test for BP between groups:\n")
kruskal_result <- kruskal.test(Numberofvesselsfluro ~ factor(AgeGroup), data = cleaned_data)
print(kruskal_result)

# 6. Perform chi-square test for "Sex", "flat_ST", "reversible_thallium" between groups
cat("Chi-square test for categorical variables between groups:\n")
chi_square_test <- function(variable) {
  table_data <- table(cleaned_data[[variable]], cleaned_data$AgeGroup)
  chi_square_result <- chisq.test(table_data)
  print(chi_square_result)
}

for (variable in categorical_vars) {
  chi_square_test(variable)
}

# Comparing SEX
# 1. Divide into 2 groups according to Sex
group_male <- cleaned_data[cleaned_data$Sex == 1, ]
group_female <- cleaned_data[cleaned_data$Sex == 0, ]

# 2. Calculate summary statistics for "BP" and "MaxHR", no. vessles in each group
variables_to_compare <- c("BP", "MaxHR", "Numberofvesselsfluro")

summary_stats_by_group <- function(data, variable, group_name) {
  cat("Summary statistics for", variable, "in", group_name, "\n")
  cat("Group Mean:", mean(data[[variable]]), "\n")
  cat("Group SD:", sd(data[[variable]]), "\n")
  cat("Group Median:", median(data[[variable]]), "\n")
  cat("Group 25th Percentile:", quantile(data[[variable]], 0.25), "\n")
  cat("Group 75th Percentile:", quantile(data[[variable]], 0.75), "\n\n")
}

for (variable in variables_to_compare) {
  summary_stats_by_group(group_male, variable, "Male")
  summary_stats_by_group(group_female, variable, "Female")
}

# 3. Compare frequency and percentage for "flat_ST", "reversible_thallium"
compare_categorical_variables <- function(data, variable, group_name) {
  cat("Frequency and Percentage for", variable, "in", group_name, "\n")
  table_result <- table(data[[variable]])
  cat("Table:\n", table_result, "\n")
  prop_table <- prop.table(table_result) * 100
  cat("Percentage Table:\n", prop_table, "\n\n")
}

categorical_variables_to_compare <- c("flat_ST", "reversible_thallium")

for (variable in categorical_variables_to_compare) {
  compare_categorical_variables(cleaned_data, variable, "Total")
  compare_categorical_variables(group_male, variable, "Male")
  compare_categorical_variables(group_female, variable, "Female")
}

# 4. Perform independent t-test for MaxHR between the groups
t_test_result <- t.test(MaxHR ~ Sex, data = cleaned_data)
cat("Independent t-test for MaxHR between Male and Female:\n")
print(t_test_result)

# 5. Perform Mann-Whitney U test for BP, no. vessels between the groups
u_test_result <- wilcox.test(BP ~ Sex, data = cleaned_data)
cat("Mann-Whitney U test for BP between Male and Female:\n")
print(u_test_result)

u_test_result <- wilcox.test(Numberofvesselsfluro ~ Sex, data = cleaned_data)
cat("Mann-Whitney U test for No. diseased vessels between Male and Female:\n")
print(u_test_result)

# 6. Perform chi-square test for "flat_ST", "reversible_thallium" between the groups
chi_square_test_result_flat_ST <- chisq.test(cleaned_data$flat_ST, cleaned_data$Sex)
cat("Chi-square test for flat_ST between Male and Female:\n")
print(chi_square_test_result_flat_ST)

chi_square_test_result_reversible_thallium <- chisq.test(cleaned_data$reversible_thallium, cleaned_data$Sex)
cat("Chi-square test for reversible_thallium between Male and Female:\n")
print(chi_square_test_result_reversible_thallium)
