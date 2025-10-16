#############################################
#
# script to see examples or linear regression
# in R using built in IRIS data
# MML 10/07/25
#############################################

rm(list=ls())

# subset the virginica species
flower <- iris[iris$Species == "virginica",]

# make a scatter plot to look at sepal length vs petal length
plot(flower$Sepal.Length, flower$Petal.Length, pch = 19,
     xlab = "Sepal length", ylab = "Petal length",
     main = "Iris virginica")

# fit a regression model
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)

# plot the residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab = "Sepal Length", ylab = "Residuals")
abline(h = 0)

# check normality of residuals
hist(summary(fit)$residuals, col = "red",
     main = "Residual Distribution", xlab = "Residuals")

# qqnorm or qqline can provide another visual check
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)

# use Shapiro-Wilk test to check normality
shapiro.test(summary(fit)$residuals)
