#### Getting Started ####
# Clear all dataframes in the Environment
rm(list= ls())
# Set the console
setwd("~/SeaTurtleProject")
#Loading Packages
install.packages("tidyverse")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
#Loading Data - The data are on the same excel file but different sheets
Data <- read_excel("HBSP_TagDataEstefany - Copy.xlsx", 
                                                          col_types = c("numeric", "numeric", "text", 
                                                                        "text", "numeric", "numeric", "numeric", 
                                                                        "date", "text", "text", "text", "numeric", 
                                                                        "numeric", "text", "text", "text", 
                                                                        "text", "text", "text", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric"))

glimpse(Data)
NestData <- read_excel("HBSP_TagDataEstefany - Copy.xlsx", 
                       sheet = "Nest data", col_types = c("numeric", 
                                                          "text", "text", "numeric", "text", 
                                                          "numeric", "numeric", "date", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "text", "text", "text", 
                                                          "numeric", "numeric", "numeric", 
                                                          "text", "text", "numeric", "text", 
                                                          "numeric", "numeric", "numeric", 
                                                          "text", "numeric", "numeric", "text", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "text", "text", 
                                                          "numeric", "text", "numeric", "numeric", 
                                                          "text"))
str(NestData)
str(Data)

# Sheets have lots of data so now we are only grabbing the data we need to find reoccuring females 
# We are grabbig the data that will allow us to study the relationship between body size and clutch size
# We want reoccuring females because the idea is that they will grow over the years. We want to see how their 
# growth affects their clutch size. 
LengthData <- select(Data, UID, Date, Year, SCL.NT, SCL.NN, SCW, CCL.NT, CCL.NN, CCW, FT.LFnew, FT.RFnew, FT.LRnew, FT.RRnew, FT.LFexisting, FT.RFexisting, FT.LRexisting, FT.RRexisting, PIT.LF, PIT.RF)

NestInfo <- select(NestData, UID, Date, Year, EmergeDate, IncubationDays, ClutchCount, ShellsOver50, UnhatchedEggs, DeadHatchlings, LiveHatchlings, HatchSuccess, EmergenceSuccess)

#Joining the two datas together
# UID is not the turtle ID. It is the ID for the data entry to a specific event
Data <- inner_join(LengthData, NestInfo, by = c("Date","UID", "Year"))
View(Data)

# I am using the gather function to put mulitiple columns into one single column.
# The data sheet had different columns for each flipper and if it was a new tag or an existing tag
# Here I am putting all the new tags together and all the existing tags together
Data.2 <- gather(Data,
                 key = "NewFlipper",
                  value = "NewMetal",
                  FT.LFnew:FT.RRnew)
Data.2 <- gather(Data.2,
                 key = "ExistingFlipper",
                 value = "ExistingMetal",
                 FT.LFexisting:FT.RRexisting)

View(Data.2)
str(Data.2)

# I want to see how many new tags apear in the existing tag column so I am taking out all the NA values in the NewTag column
Data.3 <- Data.2 %>% 
  filter(!is.na(NewMetal))

ReoccuringFemales <- Data.3$NewMetal[Data.3$NewMetal %in% Data.2$ExistingMetal]
ReoccuringFemales
# Now I have a list of metal tag numbers that reoccur in the data set. I want to subset my data into only including
# the information of reoccuring females. 

# Here I am trying to put all the metal tags numbers together into one column. 
# My idea is to grab all rows whose metal tag number matches one of the numbers on my list of reoccuring females. 
Data.2.1 <- gather(Data.2,
                   key = "NeworExisting",
                   value = "MetalTag",
                   FT.LFnew:FT.RRexisting)
View(Data.2.1)
#When I try to run this function. It does not work. 
#Hey I changed the order of varibles in your function. In the order below it's asking if the Metal Tag ID is in ReoccuringFemals. If TRUE then it gets that row. Let me know if this is what you want. 
Data.4 <- Data.2.1[Data.2.1$MetalTag %in% ReoccuringFemales,]
# here is another way to do it too
test.4 <- filter(Data.2.1, MetalTag %in% ReoccuringFemales)

str(Data.4)
View(test.4)
