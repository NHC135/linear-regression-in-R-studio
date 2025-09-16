#-------------------------------------------------------------
# import Data and set up
#--------------------------------------------------------------
setwd("D:/Project Data Sets/")

data <- read.csv("data/ecommerce-users.csv")
View(data)

str(data)
summary(data)
#--------------------------------------------------------------
# Create plots and Search For Insights
#--------------------------------------------------------------
#packages ggplot2 for visualizations
#patchwork for side by side comparisons
library(ggplot2)

# Correlation between time on website and yearly amount spent ? 
ggplot(data, aes(x=Time.on.Website, y = Yearly.Amount.Spent)) + 
  geom_point(color="orange") + 
  ggtitle("Time on website vs. Yearly amount spent") +
  xlab("Time on website") + 
  ylab("Yearly amount spent")
# The data is very scatter no visible correlation == 0

#Correlation for Avg. session length  and yearly amount spent? 
ggplot(data, aes(x=Avg..Session.Length, y= Yearly.Amount.Spent)) +
  geom_point(color="blue")
  ggtitle("Avg session length vs. Yearly amount spent") + 
  xlab("Avg session length")
  ylab("Yearly amount spent")

##pairplot of all continuous 0
  pairs(data[c("Avg..Session.Length",
               "Time.on.App", 
               "Time.on.Website", 
               "Length.of.Membership",
               "Yearly.Amount.Spent"
  )], 
  col= "orange",
  pch = 16, 
  labels = c("Avg Session Length",
             "Time on App",
             "Time on Website", 
             "Length of Membership",
             "Yearly Amount Spent"),
  main = "Pairplot of All Continuous Variables")

#--------------------------------------------------------------
# Exploring the Selected Variable
#--------------------------------------------------------------
  
#is the variable normally distributed? 
hist(data$Length.of.Membership) #Stock

ggplot(data, aes(x=Time.on.Website)) + 
  geom_histogram(color = "white",
                 fill = "orange",
                 binwidth = .5) + 
  xlab("Time on Website")

#Another way to confirm normality is to utilize QQplots or Quantile Quantile 
#first standardize the data we wish to check normality
scale(data$Length.of.Membership)
qqnorm(data$Length.of.Membership)
qqline(data$Length.of.Membership)

#QQplot with ggplot
ggplot(data,aes(sample= Length.of.Membership))+
  geom_qq(col = "orange") +
  geom_qq_line(col = "red")+
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

shapiro.test(data$Length.of.Membership)

#Box plot, stock function
boxplot(data$Length.of.Membership)

ggplot(data, aes(x= Time.on.Website, y=Yearly.Amount.Spent)) +
  geom_boxplot(fill= "orange")

#-------------------------------------------------------------
#Fitting a Linear Model
#-------------------------------------------------------------

lm.fit1 <- lm(Yearly.Amount.Spent ~ Length.of.Membership, data = data)
summary(lm.fit1)

# Create the plot using geom_smooth
ggplot(lm.fit1, aes(x = Length.of.Membership, y = Yearly.Amount.Spent)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se= FALSE, color = "blue") +
  ggtitle("Linear Model") +
  xlab("Length of Membership") +
  ylab("Yearly amount spent")

#-------------------------------------------------------------
#Residual Analysis
#-------------------------------------------------------------
#Stock QQpplot 
lm.fit1_residuals <- scale(residuals(lm.fit1))

qqnorm(lm.fit1_residuals)
qqline(lm.fit1_residuals, col="red")

hist(lm.fit1_residuals)

shapiro.test(lm.fit1_residuals)

#-------------------------------------------------------------
#Evaluation of the Model
#-------------------------------------------------------------

#Use set.seed as a random number generator 
#and select 80% of rows used for the training set.
set.seed(1)
row.number <- sample(1:nrow(data), 0.8*nrow(data))

train <- data[row.number,]
test <- data[-row.number,]

#estimate the linear fit with the training set.
lm.fit0.8 <- lm(Yearly.Amount.Spent ~ Length.of.Membership, data = train)
summary(lm.fit0.8)

#predicting the test data
prediction0.8 <- predict(lm.fit0.8, newdata = test)

err0.8 <- prediction0.8 - test$Yearly.Amount.Spent

#root mean square error
rmse <- sqrt(mean(err0.8^2))

#mean absolute percentage error 
mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))

c(RSME = rmse, mape= mape, R2 = summary(lm.fit0.8)$r.squared)
summary(lm.fit0.8)

#-------------------------------------------------------------
# Multiple Regression
#-------------------------------------------------------------
lm.fit <- lm(Yearly.Amount.Spent ~ Avg..Session.Length + Time.on.App + Time.on.Website)

