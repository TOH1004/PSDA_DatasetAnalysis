library(readxl)
library(ggplot2)

# Read the Excel file
sleep_data <- read_excel("C:/Users/tstho/Downloads/UTM/Y1S2/SECI1143 Probability & Statistical Data Analysis/Sleep_health.xlsx")
View(sleep_data)
# View the structure of the data
str(sleep_data)

# Create a contingency table of Gender vs Sleep Disorder
contingency_table <- table(sleep_data$Gender, sleep_data$`Sleep Disorder`)
print("Contingency Table:")
print(contingency_table)

# Calculate degrees of freedom
df <- (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1)
print(paste("Degrees of Freedom:", df))

# Calculate critical value for alpha = 0.05
alpha <- 0.05
critical_value <- qchisq(1 - alpha, df)
print(paste("Critical Value (α = 0.05):", round(critical_value, 4)))

# Check expected counts (should be >5 for valid chi-square test)
print("Expected Counts:")
print(chi_test$expected)

# Perform chi-square test
chi_test <- chisq.test(contingency_table)

# Print the chi-square test results
print("Chi-square Test Results:")
print(chi_test)

