# Install and load necessary libraries
install.packages("glmnet")
library(glmnet)
library(tidyverse)

# Load the data
case_and_death_demographics <- read.csv(file.choose())
city_and_town_data <- read.csv(file.choose())
county_data <- read.csv(file.choose())
weekly_cases_and_deaths <- read.csv(file.choose())



# Clean the data: Convert relevant columns to numeric and replace '*' with NA
case_and_death_demographics <- case_and_death_demographics %>%
  mutate(across(c("Week.case.rate", "Week.death.rate"), ~na_if(., "*"))) %>%
  mutate(across(c("Estimated.population", "Percent.of.the.population", "Cases.during.this.week", 
                  "Week.case.rate", "Cumulative.case.rate", "Deaths.during.this.week", 
                  "Week.death.rate", "Cumulative.deaths", "Cumulative.death.rate"), 
                ~ as.numeric(gsub("[^0-9.-]", "", .))))

# Remove rows with NA values in the specified columns
case_and_death_demographics <- case_and_death_demographics %>%
  drop_na(Week.case.rate, Week.death.rate)

# Check for NA values in all columns
na_counts <- case_and_death_demographics %>%
  summarise_all(~ sum(is.na(.)))

na_counts



# Generate descriptive statistics for the entire dataset
overall_stats <- case_and_death_demographics %>%
  summarise(
    mean_population = mean(Estimated.population, na.rm = TRUE),
    sd_population = sd(Estimated.population, na.rm = TRUE),
    mean_cases = mean(Cases.during.this.week, na.rm = TRUE),
    sd_cases = sd(Cases.during.this.week, na.rm = TRUE),
    mean_deaths = mean(Deaths.during.this.week, na.rm = TRUE),
    sd_deaths = sd(Deaths.during.this.week, na.rm = TRUE),
    mean_week_case_rate = mean(Week.case.rate, na.rm = TRUE),
    sd_week_case_rate = sd(Week.case.rate, na.rm = TRUE),
    mean_week_death_rate = mean(Week.death.rate, na.rm = TRUE),
    sd_week_death_rate = sd(Week.death.rate, na.rm = TRUE)
  )

print(overall_stats)

# Group by age group and calculate descriptive statistics
age_group_stats <- case_and_death_demographics %>%
  group_by(Subgroup) %>%
  summarise(
    mean_cases = mean(Cases.during.this.week, na.rm = TRUE),
    sd_cases = sd(Cases.during.this.week, na.rm = TRUE),
    mean_deaths = mean(Deaths.during.this.week, na.rm = TRUE),
    sd_deaths = sd(Deaths.during.this.week, na.rm = TRUE),
    mean_week_case_rate = mean(Week.case.rate, na.rm = TRUE),
    sd_week_case_rate = sd(Week.case.rate, na.rm = TRUE),
    mean_week_death_rate = mean(Week.death.rate, na.rm = TRUE),
    sd_week_death_rate = sd(Week.death.rate, na.rm = TRUE)
  )

print(age_group_stats)

# Ensure date columns are properly converted
weekly_cases_and_deaths <- weekly_cases_and_deaths %>%
  mutate(
    Week.Start.Date = mdy(Week.Start.Date),
    Week.End.Date = mdy(Week.End.Date)
  )


# Create binary outcome variable for high case weeks
case_and_death_demographics <- case_and_death_demographics %>%
  mutate(high_case_week = ifelse(`Cases.during.this.week` > median(`Cases.during.this.week`, na.rm = TRUE), 1, 0))

# Create lagged variables for previous weeks' cases and deaths
case_and_death_demographics <- case_and_death_demographics %>%
  arrange(Week.Start.Date) %>%
  mutate(
    lag_cases_1 = lag(`Cases.during.this.week`, 1),
    lag_cases_2 = lag(`Cases.during.this.week`, 2),
    lag_deaths_1 = lag(`Deaths.during.this.week`, 1),
    lag_deaths_2 = lag(`Deaths.during.this.week`, 2)
  )



# Check the unique values in the Subgroup column
unique_subgroups <- unique(case_and_death_demographics$Subgroup)
print(unique_subgroups)


# Filter rows containing "Race and Hispanic ethnicity" in the Group column
race_ethnicity_data <- case_and_death_demographics %>%
  filter(Group == "Race and Hispanic ethnicity")

# Get unique subgroups
unique_subgroups <- unique(race_ethnicity_data$Subgroup)

# Create an empty data frame to store the results
summary_data <- data.frame(
  Subgroup = character(),
  Mean_Cases = double(),
  Mean_Deaths = double(),
  Mean_Case_Rate = double(),
  Mean_Death_Rate = double(),
  stringsAsFactors = FALSE
)

# Iterate over each unique subgroup
for (subgroup in unique_subgroups) {
  subgroup_data <- race_ethnicity_data %>%
    filter(Subgroup == subgroup)
  # Calculate the mean of each column for the subgroup
  mean_values <- subgroup_data %>%
    summarise(
      Subgroup = subgroup,
      Mean_Cases = mean(Cases.during.this.week, na.rm = TRUE),
      Mean_Deaths = mean(Deaths.during.this.week, na.rm = TRUE),
      Mean_Case_Rate = mean(Week.case.rate, na.rm = TRUE),
      Mean_Death_Rate = mean(Week.death.rate, na.rm = TRUE)
    )
  
  # Add the mean values to the summary data frame
  summary_data <- rbind(summary_data, mean_values)
}

# Print the summary data as a table
output_table <- knitr::kable(summary_data, format = "markdown")
output_table

# Filter rows containing "Reported sex or gender" in the Group column
sex_gender_data <- case_and_death_demographics %>%
  filter(Group == "Reported sex or gender")

# Get unique subgroups
unique_subgroups_sex_gender <- unique(sex_gender_data$Subgroup)

# Create an empty data frame to store the results
summary_data_sex_gender <- data.frame(
  Subgroup = character(),
  Mean_Cases = double(),
  Mean_Deaths = double(),
  Mean_Case_Rate = double(),
  Mean_Death_Rate = double(),
  stringsAsFactors = FALSE
)

# Iterate over each unique subgroup
for (subgroup in unique_subgroups_sex_gender) {
  subgroup_data <- sex_gender_data %>%
    filter(Subgroup == subgroup)
  
  # Calculate the mean of each column for the subgroup
  mean_values <- subgroup_data %>%
    summarise(
      Subgroup = subgroup,
      Mean_Cases = mean(Cases.during.this.week, na.rm = TRUE),
      Mean_Deaths = mean(Deaths.during.this.week, na.rm = TRUE),
      Mean_Case_Rate = mean(Week.case.rate, na.rm = TRUE),
      Mean_Death_Rate = mean(Week.death.rate, na.rm = TRUE)
    )
  # Add the mean values to the summary data frame
  summary_data_sex_gender <- rbind(summary_data_sex_gender, mean_values)
}

# Print the summary data as a table
output_table_sex_gender <- knitr::kable(summary_data_sex_gender, format = "markdown")
output_table_sex_gender


# Filter rows containing "Age" in the Group column
age_data <- case_and_death_demographics %>%
  filter(Group == "Age")

# Get unique subgroups
unique_subgroups_age <- unique(age_data$Subgroup)

# Create an empty data frame to store the results
summary_data_age <- data.frame(
  Subgroup = character(),
  Mean_Cases = double(),
  Mean_Deaths = double(),
  Mean_Case_Rate = double(),
  Mean_Death_Rate = double(),
  stringsAsFactors = FALSE
)
# Iterate over each unique subgroup
for (subgroup in unique_subgroups_age) {
  subgroup_data <- age_data %>%
    filter(Subgroup == subgroup)
  
  # Calculate the mean of each column for the subgroup
  mean_values <- subgroup_data %>%
    summarise(
      Subgroup = subgroup,
      Mean_Cases = mean(Cases.during.this.week, na.rm = TRUE),
      Mean_Deaths = mean(Deaths.during.this.week, na.rm = TRUE),
      Mean_Case_Rate = mean(Week.case.rate, na.rm = TRUE),
      Mean_Death_Rate = mean(Week.death.rate, na.rm = TRUE)
    )
  
  # Add the mean values to the summary data frame
  summary_data_age <- rbind(summary_data_age, mean_values)
}

# Print the summary data as a table
output_table_age <- knitr::kable(summary_data_age, format = "markdown")
output_table_age

# Prepare data for regression model
regression_data <- case_and_death_demographics %>%
  filter(!is.na(lag_cases_1) & !is.na(lag_cases_2) & !is.na(lag_deaths_1) & !is.na(lag_deaths_2))

# Generate descriptive statistics for predictor variables
predictor_stats <- regression_data %>%
  summarise(
    mean_estimated_population = mean(Estimated.population, na.rm = TRUE),
    sd_estimated_population = sd(Estimated.population, na.rm = TRUE),
    mean_percent_population = mean(Percent.of.the.population, na.rm = TRUE),
    sd_percent_population = sd(Percent.of.the.population, na.rm = TRUE),
    mean_cases = mean(Cases.during.this.week, na.rm = TRUE),
    sd_cases = sd(Cases.during.this.week, na.rm = TRUE),
    mean_week_case_rate = mean(Week.case.rate, na.rm = TRUE),
    sd_week_case_rate = sd(Week.case.rate, na.rm = TRUE),
    mean_cumulative_cases = mean(Cumulative.cases, na.rm = TRUE),
    sd_cumulative_cases = sd(Cumulative.cases, na.rm = TRUE),
    mean_cumulative_case_rate = mean(Cumulative.case.rate, na.rm = TRUE),
    sd_cumulative_case_rate = sd(Cumulative.case.rate, na.rm = TRUE),
    mean_deaths = mean(Deaths.during.this.week, na.rm = TRUE),
    sd_deaths = sd(Deaths.during.this.week, na.rm = TRUE),
    mean_week_death_rate = mean(Week.death.rate, na.rm = TRUE),
    sd_week_death_rate = sd(Week.death.rate, na.rm = TRUE),
    mean_cumulative_deaths = mean(Cumulative.deaths, na.rm = TRUE),
    sd_cumulative_deaths = sd(Cumulative.deaths, na.rm = TRUE),
    mean_cumulative_death_rate = mean(Cumulative.death.rate, na.rm = TRUE),
    sd_cumulative_death_rate = sd(Cumulative.death.rate, na.rm = TRUE),
    mean_lag_cases_1 = mean(lag_cases_1, na.rm = TRUE),
    sd_lag_cases_1 = sd(lag_cases_1, na.rm = TRUE),
    mean_lag_cases_2 = mean(lag_cases_2, na.rm = TRUE),
    sd_lag_cases_2 = sd(lag_cases_2, na.rm = TRUE),
    mean_lag_deaths_1 = mean(lag_deaths_1, na.rm = TRUE),
    sd_lag_deaths_1 = sd(lag_deaths_1, na.rm = TRUE),
    mean_lag_deaths_2 = mean(lag_deaths_2, na.rm = TRUE),
    sd_lag_deaths_2 = sd(lag_deaths_2, na.rm = TRUE),
    mean_high_case_week = mean(high_case_week, na.rm = TRUE),
    sd_high_case_week = sd(high_case_week, na.rm = TRUE)
  )

print(predictor_stats)



# Prepare data for regression model
regression_data <- case_and_death_demographics %>%
  filter(!is.na(lag_cases_1) & !is.na(lag_cases_2) & !is.na(lag_deaths_1) & !is.na(lag_deaths_2))

# Multiple regression model
fit_lm <- lm(Cases.during.this.week ~ lag_cases_1 + lag_cases_2 + lag_deaths_1 + lag_deaths_2, data = regression_data)

# Summary of the model
summary(fit_lm)


# Install the forecast package
install.packages("forecast")

# Load the forecast library
library(forecast)

# Time series object for confirmed cases
ts_cases <- ts(weekly_cases_and_deaths$Confirmed.cases, frequency = 52)

# Fit ARIMA model
fit_arima <- auto.arima(ts_cases)

# Forecast for next 4 weeks (1 month)
forecast_arima <- forecast(fit_arima, h = 4)
plot(forecast_arima)

# Point estimates and prediction intervals
point_estimates <- forecast_arima$mean
lower_bounds <- forecast_arima$lower[, 2]  # 95% lower prediction interval
upper_bounds <- forecast_arima$upper[, 2]  # 95% upper prediction interval

forecast_report <- data.frame(
  Week = seq.Date(from = max(weekly_cases_and_deaths$Week.End.Date), by = "week", length.out = 4),
  Point_Estimate = point_estimates,
  Lower_Bound = lower_bounds,
  Upper_Bound = upper_bounds
)

print(forecast_report)


#Visualizations
# Plot actual vs. predicted values
predicted_values <- predict(fit_lm, newdata = regression_data)
actual_values <- regression_data$Cases.during.this.week

plot(actual_values, predicted_values, 
     xlab = "Actual Cases", ylab = "Predicted Cases", 
     main = "Actual vs. Predicted Cases")
abline(0, 1, col = "red")

# Plot residuals
residuals <- actual_values - predicted_values
plot(predicted_values, residuals, 
     xlab = "Predicted Cases", ylab = "Residuals", 
     main = "Residual Plot")
abline(h = 0, col = "red")

# Correlation analysis
correlation_matrix <- cor(regression_data %>% select(lag_cases_1, lag_cases_2, lag_deaths_1, lag_deaths_2, Cases.during.this.week))
print(correlation_matrix)


# Remove rows with NA values across all relevant columns at once
regression_data <- case_and_death_demographics %>%
  drop_na(lag_cases_1, lag_cases_2, lag_deaths_1, lag_deaths_2,
          Estimated.population, Percent.of.the.population, Week.case.rate, Week.death.rate, 
          Cumulative.case.rate, Cumulative.death.rate)




# Prepare data for LASSO regression
x <- model.matrix(Cases.during.this.week ~ lag_cases_1 + lag_cases_2 + lag_deaths_1 + lag_deaths_2 + 
                    Estimated.population + Percent.of.the.population + Week.case.rate + Week.death.rate + 
                    Cumulative.case.rate + Cumulative.death.rate, data = regression_data)[, -1]
y <- regression_data$Cases.during.this.week

# Ensure x and y have the same number of observations
nrow(x) == length(y)  # This should return TRUE

# Fit the LASSO model using cross-validation to find the best lambda
set.seed(123)  # For reproducibility
cv_lasso <- cv.glmnet(x, y, alpha = 1)

# Best lambda value
best_lambda <- cv_lasso$lambda.min

# Fit the final LASSO model
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

# Print the coefficients
print(coef(lasso_model))

# Predict using the LASSO model
predicted_lasso <- predict(lasso_model, s = best_lambda, newx = x)

# Print predicted values
print(predicted_lasso)


#Calculations Evaluation Metrics
# Actual values
actual_values <- regression_data$Cases.during.this.week

# Calculate Mean Squared Error (MSE)
mse <- mean((predicted_lasso - actual_values)^2)
print(paste("Mean Squared Error:", mse))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error:", rmse))

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(predicted_lasso - actual_values))
print(paste("Mean Absolute Error:", mae))

# Plot actual vs. predicted values
plot(actual_values, predicted_lasso, 
     xlab = "Actual Cases", ylab = "Predicted Cases", 
     main = "Actual vs. Predicted Cases by LASSO Model")
abline(0, 1, col = "red")

# Calculate residuals
residuals <- actual_values - predicted_lasso

# Plot residuals
plot(predicted_lasso, residuals, 
     xlab = "Predicted Cases", ylab = "Residuals", 
     main = "Residual Plot for LASSO Model")
abline(h = 0, col = "red")




# Fit Poisson regression model
poisson_model <- glm(Cases.during.this.week ~ lag_cases_1 + lag_cases_2 + lag_deaths_1 + lag_deaths_2 + 
                       Estimated.population + Percent.of.the.population + Week.case.rate + Week.death.rate + 
                       Cumulative.case.rate + Cumulative.death.rate, 
                     family = poisson(link = "log"), data = regression_data)

# Summary of the model
summary(poisson_model)


# Line chart for confirmed cases
ggplot(data = weekly_cases_and_deaths, aes(x = Week.Start.Date, y = Confirmed.cases)) +
  geom_line(color = "blue") +
  labs(title = "Weekly Confirmed COVID-19 Cases",
       x = "Week Start Date",
       y = "Number of Confirmed Cases") +
  theme_minimal()

# Line chart for confirmed deaths
ggplot(data = weekly_cases_and_deaths, aes(x = Week.Start.Date, y = Confirmed.deaths)) +
  geom_line(color = "red") +
  labs(title = "Weekly Confirmed COVID-19 Deaths",
       x = "Week Start Date",
       y = "Number of Confirmed Deaths") +
  theme_minimal()

# Line chart for probable cases
ggplot(data = weekly_cases_and_deaths, aes(x = Week.Start.Date, y = Probable.cases)) +
  geom_line(color = "green") +
  labs(title = "Weekly Probable COVID-19 Cases",
       x = "Week Start Date",
       y = "Number of Probable Cases") +
  theme_minimal()

# Line chart for probable deaths
ggplot(data = weekly_cases_and_deaths, aes(x = Week.Start.Date, y = Probable.deaths)) +
  geom_line(color = "purple") +
  labs(title = "Weekly Probable COVID-19 Deaths",
       x = "Week Start Date",
       y = "Number of Probable Deaths") +
  theme_minimal()



# Load necessary libraries
install.packages("caret")
library(caret)
install.packages("e1071")  # For confusionMatrix function
library(e1071)

# Ensure data is clean and ready for analysis
regression_data <- regression_data %>%
  filter(!is.na(lag_cases_1) & !is.na(lag_cases_2) & !is.na(lag_deaths_1) & !is.na(lag_deaths_2))

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(regression_data$high_case_week, p = 0.7, list = FALSE)
train_data <- regression_data[train_index, ]
test_data <- regression_data[-train_index, ]

# Fit logistic regression model
logistic_model <- glm(high_case_week ~ lag_cases_1 + lag_cases_2 + lag_deaths_1 + lag_deaths_2 + 
                        Estimated.population + Percent.of.the.population + Week.case.rate + Week.death.rate + 
                        Cumulative.case.rate + Cumulative.death.rate, 
                      family = binomial(link = "logit"), data = train_data)

# Make predictions on the test set
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Generate the confusion matrix
confusion_matrix <- confusionMatrix(factor(predicted_classes), factor(test_data$high_case_week))
print(confusion_matrix)


# Load necessary libraries
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)


# Convert categorical variables to factors for better plotting
case_and_death_demographics$Group <- as.factor(case_and_death_demographics$Group)
case_and_death_demographics$Subgroup <- as.factor(case_and_death_demographics$Subgroup)

# Filter data for age group analysis
age_group_data <- case_and_death_demographics %>%
  filter(Group == "Age")

# Histogram of Weekly Cases by Age Group
ggplot(age_group_data, aes(x = Cases.during.this.week, fill = Subgroup)) +
  geom_histogram(binwidth = 10, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of Weekly Cases by Age Group", x = "Weekly Cases", y = "Frequency") +
  theme_minimal()


# Ensure proper conversion of date columns to Date type
case_and_death_demographics$Week.Start.Date <- mdy(case_and_death_demographics$Week.Start.Date)
case_and_death_demographics$Week.End.Date <- mdy(case_and_death_demographics$Week.End.Date)

# Filter data for gender analysis
gender_data <- case_and_death_demographics %>%
  filter(Group == "Reported sex or gender")

# Aggregate data on a monthly basis
monthly_gender_data <- gender_data %>%
  mutate(Month = floor_date(Week.Start.Date, "month")) %>%
  group_by(Month, Subgroup) %>%
  summarise(
    Monthly_Cases = sum(Cases.during.this.week, na.rm = TRUE),
    Monthly_Deaths = sum(Deaths.during.this.week, na.rm = TRUE)
  ) %>%
  ungroup()

# Line Chart for Monthly Cases by Gender
ggplot(monthly_gender_data, aes(x = Month, y = Monthly_Cases, color = Subgroup, group = Subgroup)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Monthly COVID-19 Cases by Gender", x = "Month", y = "Monthly Cases") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter data for race/ethnicity analysis
race_ethnicity_data <- case_and_death_demographics %>%
  filter(Group == "Race and Hispanic ethnicity")

# Aggregate race/ethnicity data on a monthly basis
monthly_race_ethnicity_data <- race_ethnicity_data %>%
  mutate(Month = floor_date(Week.Start.Date, "month")) %>%
  group_by(Month, Subgroup) %>%
  summarise(
    Monthly_Cases = sum(Cases.during.this.week, na.rm = TRUE),
    Monthly_Deaths = sum(Deaths.during.this.week, na.rm = TRUE)
  ) %>%
  ungroup()

# Line Chart for Monthly Cases by Race/Ethnicity
ggplot(monthly_race_ethnicity_data, aes(x = Month, y = Monthly_Cases, color = Subgroup, group = Subgroup)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Monthly COVID-19 Cases by Race/Ethnicity", x = "Month", y = "Monthly Cases") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
