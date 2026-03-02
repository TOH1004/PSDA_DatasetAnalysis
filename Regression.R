library(readxl)
library(ggplot2)

df <- read_excel("C:/Users/idaya/Downloads/Sleep_health_and_lifestyle_dataset.xlsx")

cat("--- Initial Scatter Plot (Figure 3.2.4) ---\n")
ggplot(data = df, aes(x = `Physical Activity Level`, y = `Quality of Sleep`)) +
  geom_point() +
  labs(
    title = "Quality of Sleep vs. Physical Activity Level",
    x = "Physical Activity Level",
    y = "Quality of Sleep"
  ) +
  theme_minimal()
cat("\n")

ggplot(data = df, aes(x = `Physical Activity Level`, y = `Quality of Sleep`)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.7) +
  labs(
    title = "Quality of Sleep vs. Physical Activity Level (Jittered)",
    x = "Physical Activity Level",
    y = "Quality of Sleep"
  ) +
  theme_minimal()

model <- lm(`Quality of Sleep` ~ `Physical Activity Level`, data = df)

model_summary <- summary(model)

cat("--- Linear Regression Model Summary ---\n")
print(model_summary)
cat("\n")

cat("--- Estimated Coefficients (b0 and b1) ---\n")
coefficients <- coef(model)
coefficient_table <- data.frame(
  Coefficient = names(coefficients),
  Value = as.numeric(coefficients)
)
print(coefficient_table)
cat("\n")

cat("--- Explained and Unexplained Variation ---\n")
y_observed <- df$`Quality of Sleep`
y_predicted <- fitted(model)
residuals <- residuals(model)
mean_y_observed <- mean(y_observed)

SST <- sum((y_observed - mean_y_observed)^2)
SSE <- sum(residuals^2)
SSR <- sum((y_predicted - mean_y_observed)^2)

sum_of_squares_table <- data.frame(
  Statistic = c("SST (Total Sum of Squares)", "SSE (Error Sum of Squares)", "SSR (Regression Sum of Squares)"),
  Value = c(SST, SSE, SSR)
)
print(sum_of_squares_table)
cat("\n")

cat("--- Coefficient of Determination (R-squared) ---\n")
r_squared <- model_summary$r.squared
cat("Coefficient of Determination (R-squared):", r_squared, "\n\n")

cat("--- Standard Deviation of Regression Slope (Sb1) ---\n")
std_error_b1 <- model_summary$coefficients["`Physical Activity Level`", "Std. Error"]
cat("Standard Deviation of Regression Slope (Sb1):", std_error_b1, "\n\n")

cat("--- Inference about the Slope: t-Test ---\n")

cat("● Hypothesis Statement:\n")
cat("   H0: β1 = 0 (no linear relationship between Quality of Sleep and Physical Activity Level)\n")
cat("   H1: β1 ≠ 0 (a linear relationship does exist between Quality of Sleep and Physical Activity Level)\n\n")

n <- nrow(df)
df_t_test <- n - 2

t0 <- model_summary$coefficients["`Physical Activity Level`", "t value"]

alpha <- 0.05
critical_value_lower <- qt(alpha / 2, df = df_t_test)
critical_value_upper <- qt(1 - alpha / 2, df = df_t_test)

cat(sprintf("● Degrees of Freedom (df = n-2): %d\n", df_t_test))
cat(sprintf("● Critical Values (α = %.2f, df = %d):\n", alpha, df_t_test))
cat(sprintf("   Since this is a two-tailed test, the critical values are: %.3f and %.3f\n", critical_value_lower, critical_value_upper))
cat(sprintf("   Hence, we reject H0 if test statistic < %.3f or test statistic > %.3f.\n\n", critical_value_lower, critical_value_upper))
cat(sprintf("● Calculated Test Statistic (t0): %.3f\n\n", t0))

conclusion_text <- ""
if (t0 < critical_value_lower || t0 > critical_value_upper) {
  conclusion_text <- "Since the test statistic (t0) falls outside the critical values, we reject the null hypothesis (H0). This indicates that a linear relationship does exist between Physical Activity Level and Quality of Sleep."
} else {
  conclusion_text <- "Since the test statistic (t0) falls within the critical values, we fail to reject the null hypothesis (H0). This indicates that there is no significant linear relationship between Physical Activity Level and Quality of Sleep."
}
cat("● Conclusion:\n")
cat(conclusion_text, "\n\n")

cat("--- Jittered Scatter Plot with Linear Regression Line (Figure 3.2.6) ---\n")
ggplot(data = df, aes(x = `Physical Activity Level`, y = `Quality of Sleep`)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Quality of Sleep vs. Physical Activity Level (Jittered with Regression Line)",
    x = "Physical Activity Level",
    y = "Quality of Sleep"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

