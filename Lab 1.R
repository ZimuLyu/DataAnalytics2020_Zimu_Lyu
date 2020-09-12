# import the data
library(readxl)
EPI_data <- read_xls("/Users/cleardog/BA/MGMT 6962 Data Analytics/Lab/2010EPI_data.xls", sheet = 5, na="NA")

View(EPI_data)
attach(EPI_data)
fix(EPI_data) 

EPI
tf <- is.na(EPI)
E <- EPI[!tf]
E

#stats
summary(EPI)
fivenum(EPI, na.rm = TRUE)

#Stem-and-Leaf Plots
help(stem)
stem(EPI)

#Histograms
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob = TRUE)
#Add Connected Line Segments to a Plot
help(lines)
lines(density(EPI, na.rm = TRUE, bw = 1.))
#Add a Rug to a Plot
help(rug)
rug(EPI)

#Cumulative Density Function
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)

par(pty = "s")
qqnorm(EPI); qqline(EPI)

#Simulated data from t-distribution:
x <- rt(250, df = 5)
qqnorm(x); qqline(x)

#Make a Q-Q plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#Using another variable, DALY
plot(ecdf(DALY), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(DALY); qqline(DALY)

#WATER_H
plot(ecdf(WATER_H), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(WATER_H); qqline(WATER_H)

#Comparing distributions
boxplot(EPI,DALY)
qqplot(EPI,DALY)

#Intercompare
boxplot(EPI, ENVHEALTH)
boxplot(EPI, ECOSYSTEM)
boxplot(EPI, AIR_H)
boxplot(EPI, AIR_E)
boxplot(EPI, WATER_H)
boxplot(EPI, WATER_E)
boxplot(EPI, BIODIVERSITY)

help(distribution)
#Note: I forgot some knowledge about statistics learned before
#search the meanings of those functionslater

EPILand <- EPI[!Landlock]
EPILand
ELand <- EPILand[!is.na(EPILand)]
ELand
hist(ELand) #Frenquency
hist(ELand, seq(30., 95., 1.0), prob = TRUE) #Density

EPINoWater <- EPI[!No_surface_water]
EPINoWater
ENoWater <- EPINoWater [!is.na(EPINoWater)]
ENoWater
hist(ENoWater)
hist(ENoWater, seq(30., 95., 1.0), prob = TRUE)

EPIDesert <- EPI[!Desert]
EPIDesert
EDesert <- EPIDesert [!is.na(EPIDesert)]
EDesert
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob = TRUE)

EPI_South_Asia <- EPI[EPI_regions == "South Asia"]
EPI_South_Asia

EPI[GEO_subregion == "South Asia"]
