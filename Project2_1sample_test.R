data <- read_excel("C:/Users/User/Desktop/PSDA/Sleep_health_and_lifestyle_dataset.xlsx")

str(data)
names(data)

t.test(data$`Sleep Duration`, mu = 8, alternative = "less")
sd(data$`Sleep Duration`)
length(na.omit(data$`Sleep Duration`))
qt(0.05, df = 374)
n=374
pval=pt(-21.095, df=n-1, lower.tail = TRUE)


# Sample size
n <- length(na.omit(data$`Sleep Duration`))
n

# Sample mean
xbar <- mean(data$`Sleep Duration`, na.rm = TRUE)
xbar

# Sample standard deviation
s <- sd(data$`Sleep Duration`, na.rm = TRUE)
s

# Hypothesized population mean
mu <- 8
mu

# Calculate t-statistic
t <- (xbar - mu) / (s / sqrt(n))
t

# Calculate p-value for left-tailed test
pval <- pt(t, df = n - 1, lower.tail = TRUE)
pval

alpha = 0.05

t.alpha = qt(alpha, df = n-1)
t.alpha

# Display results
t
pval
