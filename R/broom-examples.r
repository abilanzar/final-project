install.packages("gtsummary")
install.packages("usethis")
install.packages("janitor")
library(janitor)
library(usethis)
library(tidyverse)
library(tidycat)
library(broom)
library(gtsummary)
library(ggplot2)


here::here()
getwd()


load(here::here("data", "raw", "diabetes.rda"))
diabetes <- diabetes %>% janitor::clean_names()


colnames(diabetes)
str(diabetes)
table(diabetes$diabetes_5y) #Diagnosis of diabetes in the following 5 years (pos or neg), 268 pos,500 neg, 0 missing;
summary(diabetes$glucose_mg_dl)
summary(diabetes$pedigree)
summary(diabetes$bmi)
summary(diabetes$age)
sd(diabetes$age)
#Diabetes Prediction Dataset from the Pima Indian Tribe and the NIDDK

#The primary analysis task is to classify in each participant whether diabetes developed within 5 years of data collection
#(diabetes_5y = pos), or the participant tested repeatedly negative for diabetes over the next 5 years (diabetes_5y = neg).

#1. Create a {gtsummary} table of descriptive statistics

# Load required libraries
library(janitor)
library(tidyverse)
library(broom)
library(gtsummary)

# Load the data and clean variable names
load(here::here("data", "raw", "diabetes.rda"))
diabetes <- diabetes %>% janitor::clean_names()

# Custom function to calculate mean ± sd
mean_sd_custom <- function(x) {
	paste0(sprintf("%.2f ± %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)))
}

# Define a list of continuous variables
continuous_vars <- c(age, bmi, pregnancy_num, glucose_mg_dl, dbp_mm_hg, triceps_mm,
										 insulin_microiu_ml, pedigree)

# Create a {gtsummary} table of descriptive statistics with mean ± sd
descriptive_table <- tbl_summary(
	diabetes,
	by = diabetes_5y,
	include = continuous_vars,
	label = list(
		age ~ "Age (years)",
		bmi ~ "Body mass index (kg/m2)",
		pregnancy_num ~ "Number of pregnancies",
		glucose_mg_dl ~ "Plasma glucose concentration at 2 hours (mg-dl)",
		dbp_mm_hg ~ "Diastolic blood pressure (mm Hg)",
		triceps_mm ~ "Triceps skin fold thickness (mm)",
		insulin_microiu_ml ~ "Serum insulin at 2 hours (microIU/ml)",
		pedigree ~ "Diabetes pedigree score"
	),
	missing_text = "Missing"
) |>
	add_p(
		test = list(
			all_continuous() ~ "t.test",
			all_categorical() ~ "chisq.test"
		)
	) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**") |>
	add_stat(
		fun = list(mean_sd_custom),
		locations = continuous_vars,
		header = "**Mean ± SD**"
	)

# Print the descriptive table
descriptive_table





#2. Fit a regression and present well-formatted results from the regression (1 pt)
#The regression doesn’t have to be of any particular scientific interest, and you don’t have to interpret it in any particular way
#You may use {broom} or {gtsummary} or both

# Fit a linear regression model
model <- lm(glucose_mg_dl ~ age, data = diabetes)

# Summarize the model using gtsummary
regression_summary <- model %>%
	tidy() %>%
	gtsummary::tbl_regression()

# Print the regression summary
regression_summary


#3. Create a figure (1 pt)
#It doesn’t need to look pretty; feel free to adapt some of the examples from class,
#which were as simple as hist(data$variable) and as complicated as the forest plot in the {broom} section
# Create a histogram of the 'age' variable
hist(diabetes$age, main = "Histogram of Age", xlab = "Age (years)")
hist(diabetes$pregnancy_num)
hist(diabetes$bmi) #n
hist(diabetes$glucose_mg_dl) #n
hist(diabetes$triceps_mm) #n
hist(diabetes$dbp_mm_hg) #n
hist(diabetes$insulin_microiu_ml)
hist(diabetes$pedigree) #n


#4. Write and use a function that does something with the data (1 pt)

# Custom function to calculate standard deviation
calculate_sd <- function(data, variable_name) {
	sd_value <- sd(data[[variable_name]])
	return(sd_value)
}

# Example usage of the custom function to calculate the standard deviation of 'age'
age_sd <- calculate_sd(diabetes, "age")
print(paste("Standard Deviation of Age:", age_sd))


#5. Create and render a quarto document that includes at least:
#The table, regression results, and figure, with appropriate captions (1 pt)
#Inline R code in at least 2 places, 1 pulling a statistic from a table (i.e., using gtsummary::inline_text())
#and 1 printing something else (like we did with the mean age in the example) (1 pt)
#Cross-references to a table and a figure at least once each (1 pt)
#A brief description of the data, including its source (1 pt)




