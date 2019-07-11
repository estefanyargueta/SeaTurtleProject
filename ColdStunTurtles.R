##  -------------------------------------------------------------  ##
###### Getting Started ####
##  -------------------------------------------------------------  #
# Clear all dataframes in the Environment
rm(list = ls())
# Set the console
setwd("~/SeaTurtleProject")
#Loading Packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
#Loading Data
Data <- read_excel("Loggerheadcoldstun.xlsx")
View(Data)


##  -------------------------------------------------------------  ##
                  #### Reshaping my Data ####
##  -------------------------------------------------------------  ##

# We are looking at the relationship between mass and ccl/scl
# To examine that relationship, we will first take out all the data that does not have a mass value. 
str(Data)
# we do not need all the columns in the data so lets only grab what we want 
Data <- select(Data, STSSN, SCL.NT, SCL.NN, SCW, CCL.NT, CCL.NN, CCW, Mass)

Data <- Data %>% 
  filter(!is.na(Mass))
str(Data)

fit1 <- lm(Mass ~ SCL.NT + SCW + SCL.NT:SCW, data = Data)
fit2 <- lm(Mass ~ SCL.NT + SCW, data = Data)
fit3 <- lm(Mass ~ SCL.NN + SCW + SCL.NN:SCW, data = Data)
fit4 <- lm(Mass ~ SCL.NN + SCW, data = Data)
fit5 <- lm(Mass ~ CCL.NT + CCW + CCL.NT:CCW, data = Data)
fit6 <- lm(Mass ~ CCL.NT + CCW, data = Data)
fit7 <- lm(Mass ~ CCL.NN + CCW + CCL.NN:CCW, data = Data)
summary(fit7)
fit8 <- lm(Mass ~ CCL.NN + CCW, data = Data)

AIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)
