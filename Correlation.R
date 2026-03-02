#import data
library(readxl)
Sleep_health_and_lifestyle_dataset <- read_excel("C:/Users/OWNER/Downloads/Sleep_health_and_lifestyle_dataset.xlsx", 
                                                 col_types = c("numeric", "skip", "skip", 
                                                               "skip", "numeric", "skip", "skip", 
                                                               "numeric", "skip", "skip", "skip", 
                                                               "skip", "skip"))

#Scatter plot showing the relationship between Sleep Duration and Stress Level
#with points overlapping
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = `Sleep Duration`, y = `Stress Level`)) +
  geom_point(color = "steelblue") +
  labs(title = "Relationship Between Sleep Duration and Stress Level",
       x = "Sleep Duration (hours)",
       y = "Stress Level") +
  theme_minimal()

# Scatter plot with jitter to prevent overlap
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = `Sleep Duration`, y = `Stress Level`)) +
  geom_jitter(color = "steelblue", width = 0.2, height = 0.2) + 
  labs(title = "Sleep Duration vs Stress Level",
       x = "Sleep Duration (hours)",
       y = "Stress Level") +
  theme_minimal()

#calculate sample correlation coefficient
correlation <-cor(Sleep_health_and_lifestyle_dataset$`Sleep Duration`, 
                  Sleep_health_and_lifestyle_dataset$`Stress Level`, method = "pearson")

#print sample correlation coefficient
print(paste("Sample correlation coefficient = ", correlation))

#Significance Test for Correlation
#calculate test statistic
n<-374
r<-correlation
teststat<-r/sqrt((1-r^2)/(n-2))
print(paste0("Test Statistic = ", teststat))

#calculate the critical value
alpha<-0.05
df<-n-2
critval<-qt(1-alpha/2, df)
print(paste("Critical value at 95% confidence level = ", critval))

#Performing correlation significance test using RStudio
cor.test(Sleep_health_and_lifestyle_dataset$`Sleep Duration`,
           Sleep_health_and_lifestyle_dataset$`Stress Level`,
           method = "pearson")

