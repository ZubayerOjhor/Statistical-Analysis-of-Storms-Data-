install.packages("dplyr")
library(dplyr)

install.packages("BSDA")
library(BSDA)

# Load the Storms dataset
data(storms)

# Subset the data for the years 1975 and 1991
storms_1975 <- filter(storms, year == 1975)
storms_1991 <- filter(storms, year == 1991)

# Calculating the population variances for both years
var_1975 <- var(storms_1975$lat)
var_1991 <- var(storms_1991$lat)

#Defining the variables for Z test
s_1975 <-storms_1975$lat
s_1991 <-storms_1991$lat

#Conducticting the Z test
z_test_result <- z.test(s_1975,s_1991,sigma.x = sqrt(var_1975),sigma.y = sqrt(var_1991),conf.level = 0.95)
print(z_test_result)

#Proportion Calculation
#p1 <- (x1/n1)
#p2 <- (x2/n2)

data(storms)
# Subset the data for the years 2020 and 2021
s1 <-filter(storms, year ==2020)
s2 <-filter(storms, year ==2021)

# Sample Size for both cases
n1 <- nrow(s1)
n2 <- nrow(s2)

# Number of cases possess the attribute of interest
x1 <- sum(s1$status == "tropical depression")
x2 <-sum(s2$status == "tropical storm")

#For Prop_test variables(vector) Declaration
n <- c(n1, n2)
x <- c(x1, x2)
#Prop test
prop_test <- prop.test(x,n,conf.level = 0.95,correct = TRUE)

print(prop_test)

#installing ConfIntVariance package for var.test
install.packages("ConfIntVariance")
library(ConfIntVariance)

#loading the dataset
data(storms)
# Subsetting the data for the years 2020 and 2021
s1999 <-filter(storms, year ==1999)
s2000 <-filter(storms, year ==2000)

#Conducting var.test
variance_ratio<-var.test(s1999$pressure,s2000$pressure)
print(variance_ratio)














