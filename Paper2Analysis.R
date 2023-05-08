# Empty the envionment and prepare the environment
rm(list = ls())

# Load the library
library(foreign) # this package can only read the Stata version 5-12
library(haven)
library(tidyr)
library(dplyr)
library(visreg)
library(mice)
library(broom)
library(lattice)
library(sandwich)
library(lmtest)
library(ggplot2)
# Read the sata data into R
Paper2Data <- read_dta("Paper2Dataset.dta")

# Inspect the data
head(Paper2Data); tail(Paper2Data)
str(Paper2Data) # check the structure of the data

ggplot(data = Paper2Data, aes(x = NDVI_300_EC3, y = everasthmaR4)) + geom_point()
ggplot(data = Paper2Data, aes(x = NDVI_300_EC3, y = everasthmaR4)) + geom_jitter(width = 0.05, height = 0.05) + geom_smooth(method = "glm", method.args = list(family = "poisson"))

# Poisson Regression
fit1 <- glm(everasthmaR4 ~ NDVI_300_EC3 + factor(gender) + ageR3 + factor(bmicatR3) + factor(education3) + factor(R3_smoking5R3) + factor(maritalstatusR3), data=subset(Paper2Data, everasthmaR3=="0"), family = poisson(link = "log"))
summary(fit1)
visreg(fit1, "Paper2Data$NDVI_300_EC3", xlab = "NDVI", ylab = "IRR (asthma)")

fit1 <- glm(everasthmaR4 ~ iqr_NDVI_2010 + factor(gender) + factor(bmicatR3) + factor(education3) + factor(R3_smoking5R3) + factor(maritalstatusR3), data=subset(Paper2Data, everasthmaR3=="0"), family = poisson())
summary(fit1)