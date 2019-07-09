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
Data <- read_excel("HBSP_TagDataEstefany - Copy.xlsx", 
                   col_types = c("numeric", "numeric", "text", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "date", "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))
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
#Grabbing the parts of the data that we need
LengthData <- select(Data, UID, Date, Year, SCL.NT, SCL.NN, SCW, CCL.NT, CCL.NN, CCW, FT.LFnew, FT.RFnew, FT.LRnew, FT.RRnew, FT.LFexisting, FT.RFexisting, FT.LRexisting, FT.RRexisting, PIT.LF, PIT.RF)

NestInfo <- select(NestData, UID, Date, Year, EmergeDate, IncubationDays, ClutchCount, ShellsOver50, UnhatchedEggs, DeadHatchlings, LiveHatchlings, HatchSuccess, EmergenceSuccess)

#Joining the two datas together
Data <- inner_join(LengthData, NestInfo, by = c("Date","UID", "Year"))

##  -------------------------------------------------------------  ##
  #### Reshaping my Data and comparing measurements ####
##  -------------------------------------------------------------  ##

# Since we want to study the relationship of clutch count and carapace length, we are taking all NA values for Clutch COunt
Data <- Data %>% 
  filter(!is.na(ClutchCount))

#I am using the gather function to put all the measurements into one column so that I can compare them
Data.v2 <- gather(Data,
                   key = "MeasurementType",
                   value = "Length",
                   SCL.NT:CCW)
View(Data.v2)
# comparing measurements
# finding how many observations there are for each type of measurement
Data.v2 %>% 
  group_by(MeasurementType) %>% 
  filter(!is.na(Length)) %>% 
  summarize (n())

#comparing which would allow me to have the largest data set
Data %>% 
  filter(is.na(SCL.NN), !is.na(CCL.NN)) %>% 
  summarize(n())

Data %>% 
  filter(is.na(CCL.NN), !is.na(SCL.NN)) %>% 
  summarize(n())
# I decided to anaylze SCL.NN because it has the most observations.

##  -------------------------------------------------------------  ##
        #### Now we will convert CCL.NN to SCL.NN ####
##  -------------------------------------------------------------  ##

# Finding where there are NA values for SCL.NN and values for CCL.NN
# I will use the values for CCL.NN and transform them into SCL.NN
Data.v3 <- Data %>%
  filter(is.na(SCL.NN), !is.na(CCL.NN)) %>% 
  mutate(SCL.NN = 3.61 + 0.892*(CCL.NN))
Data.v3
# This gave me 11 more observations for SCL.NN

#Lets get the other observations for SCL.NN where there were no missing values
PartData.v3 <- Data %>%
  filter(!is.na(SCL.NN))
PartData.v3

#Now we will combind the table with the new calculated SCL.NN and the table with no missing values for SCL.NN
Data.v4 <- bind_rows(Data.v3, PartData.v3)

##  -------------------------------------------------------------  ##
        #### Running a regression on the new SCL.NN ####
##  -------------------------------------------------------------  ##

#identifying outliers #
par(mfrow=c(1,2))
outlier_values <- boxplot.stats(Data.v4$ClutchCount)$out 
boxplot(Data.v4$ClutchCount, main = "Clutch Count", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

Data.v4$ClutchCount
qnt <- quantile(Data.v4$ClutchCount, probs=c(.25, .75), na.rm = T)
caps <- quantile(Data.v4$ClutchCount, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Data.v4$ClutchCount, na.rm = T)
Data.v4$ClutchCount[Data.v4$ClutchCount < (qnt[1] - H)] <- caps[1]
Data.v4$ClutchCount[Data.v4$ClutchCount > (qnt[2] + H)] <- caps[2]
boxplot(Data.v4$ClutchCount, main = "Clean Clutch Data")


outlier_values <-boxplot.stats(Data.v4$CCL.NN)$out 
boxplot(Data.v4$CCL.NN, main="Straight Carapace Notch-Notch", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

qnt <- quantile(Data.v4$SCL.NN, probs=c(.25, .75), na.rm = T)
caps <- quantile(Data.v4$SCL.NN, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Data.v4$SCL.NN, na.rm = T)
Data.v4$SCL.NN[Data.v4$SCL.NN < (qnt[1] - H)] <- caps[1]
Data.v4$SCL.NN[Data.v4$SCL.NN > (qnt[2] + H)] <- caps[2]
boxplot(Data.v4$SCL.NN, main = "Clean SCL.NN")


# Data.v4 has 396 observations
glimpse(Data.v4)
# has years - 1980, 1990-94, 96-2003, 2005-18  
unique(Data.v4$Year)

#Linear Model Info
fit4 <- lm(ClutchCount ~ SCL.NN, data = Data.v4)
summary(fit4)

# Graph
theme_update(plot.title = element_text(hjust = 0.5))
par(mfrow=c(1,1))
plot1 <- ggplot(Data.v4, aes(x = SCL.NN, y = ClutchCount)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red", se = FALSE)

plot1 + labs(title="Positive Relationship between Straight Carapace Length and Clutch Count",
             x ="Straight Carapace Length (cm)", y = "Clutch Count",
             caption="R-squared = 0.096") +
  theme( plot.title = element_text(size=14, face="bold"),
         axis.title.x = element_text(size=14),
         axis.title.y = element_text(size=14))

##### End
