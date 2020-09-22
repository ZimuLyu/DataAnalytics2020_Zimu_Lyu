multivariate <- read.csv('/Users/cleardog/BA/MGMT 6962 Data Analytics/Lab Data and File/multivariate.csv')
attach(multivariate)
View(multivariate)
names(multivariate)

plot(Income, Population)
plot(Income, Immigrant)
plot(Immigrant, Homeowners)

mlm <- lm(Homeowners~Immigrant)
mlm
abline(mlm)
abline(mlm, col = 2, lwd = 3)
abline(mlm, col = 3, lwd = 3)

summary(mlm)
summary(mlm)$coef
attributes(mlm)
mlm$coefficients

library(ggplot2)
plot(mtcars$wt, mtcars$mpg)
# These results are same
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

plot(pressure$temperature,pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")

qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(temperature, pressure)) +
  geom_line() +
  geom_point()

barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
help("barplot")

# Bar graphs
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(cyl)) +
  geom_bar()

# Compare different histograms
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mtcars$mpg, binwidth = 4)
ggplot(mtcars, aes(mpg)) +
  geom_histogram()
ggplot(mtcars, aes(mpg)) +
  geom_histogram(binwidth = 4)
ggplot(mtcars, aes(mpg)) +
  geom_histogram(binwidth = 5)

# Box plots
plot(ToothGrowth$supp, ToothGrowth$len) 
boxplot(len ~ supp, data = ToothGrowth) # with axis labels
boxplot(len ~ supp + dose, data =  ToothGrowth) #combine 2 variables on x-axis
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
ggplot(ToothGrowth, aes(supp, len)) +
  geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(interaction(supp, dose), len)) +
  geom_boxplot()
# It's clearer by using different colors
ggplot(ToothGrowth, aes(supp, len, color = factor(dose))) +
  geom_boxplot()
ggplot(ToothGrowth, aes(factor(dose), len, color = supp)) +
  geom_boxplot()

install.packages("gcookbook")
library(gcookbook)
