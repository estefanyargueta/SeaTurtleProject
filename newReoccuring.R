##  -------------------------------------------------------------  ##
                  ###### Getting Started ####
##  -------------------------------------------------------------  #
# Clear all dataframes in the Environment
rm(list= ls())
# Set the console
setwd("~/SeaTurtleProject")
#Loading Packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library("RColorBrewer")
#Loading Data - The data are on the same excel file but different sheets
Data <- read_excel("HBSP_TagDataEstefany - Copy.xlsx", 
                   sheet = "CLEANTAG", col_types = c("text", 
                                                     "numeric", "numeric", "text", "text", 
                                                     "numeric", "numeric", "text", "date", 
                                                     "text", "text", "text", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "text"))

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
LengthData <- select(Data, E.TurtleCode, UID, Date, Year, SCL.NT, SCL.NN, SCW, CCL.NT, CCL.NN, CCW)

NestInfo <- select(NestData, UID, Date, Year, EmergeDate, IncubationDays, ClutchCount, ShellsOver50, UnhatchedEggs, DeadHatchlings, LiveHatchlings, HatchSuccess, EmergenceSuccess)

#Joining the two datas together 
# n = 808
# UID is not the turtle ID. It is the ID for the data entry to a specific event
Data <- inner_join(LengthData, NestInfo, by = c("Date","UID", "Year"))
View(Data)

# There are many duplicate rows because in the original "Data" each row was a different tag number
# We do not need the tag numbers anymore because E.TurtleCode is the code for each unique turtle. 
# Here, we delete the duplicate rows by only requesting the unique rows. 
# n = 436
Data <- Data %>% distinct()

##  -------------------------------------------------------------  ##
                    #### Reshaping my Data ####
##  -------------------------------------------------------------  ##

# Since our study is looking at the relationship between body size and clutch size, we want to filter out the rows that do not have 
# information on clutch size --- n = 237
Data <- Data %>% 
  filter(!is.na(ClutchCount))

str(Data)
#So now we want to find the reoccuring females. 
Data.2 <- Data %>% 
  group_by(E.TurtleCode) %>%
  summarize(ntimes = n_distinct(Year)) %>% 
  arrange(desc(ntimes))

View(Data.2)

Data.3 <- filter(Data.2, ntimes >= 3)
Data.3

# We have 4 turtles that show up for more than 3 years
Data.4 <- Data %>% 
  filter(E.TurtleCode %in% c("5T", "7O", "7V", "7W"))
View(Data.4)

##  -------------------------------------------------------------  ##
        #### Yearly Reoccuring females analysis  ####
##  -------------------------------------------------------------  ##

#Looking at the Data in Data.4 -- the measurements with no missing data are SCL.NN and CCL.NN. 
# We will use SCL.NN since we use if for most statistical analysis.
# Growth throughout the years looking at the two measurements
theme_update(plot.title = element_text(hjust = 0.5))
# That just places the title of the graph in the center 


#Lets see how the turtles grow over the years
Growth <- ggplot(Data.4, aes(x = Year, y = SCL.NN, color = E.TurtleCode)) +
  geom_point(size = 4) + 
  geom_smooth(method = "lm", se = FALSE)

Growth + labs(title="Growth in Reoccuring Females",
              x ="Year", y = "Straight Carapace Length Notch-Notch (cm)") +
  theme( plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14))

# Linear Regreesion between CC and SCL.NN
fit.1 <- lm(ClutchCount ~ SCL.NN, data = Data.4)
summary(fit.1)

#Plotting that relationship
# This plot adds linear regreession for each turtle, not the group as a whole
ggplot(Data.4, aes(x= SCL.NN, y= ClutchCount, color = E.TurtleCode))+
  geom_point(size=4) + 
  geom_smooth(method = "lm", se = FALSE)

# We will plot the points and add the linear regreesion model after
plot2 <- ggplot(Data.4, aes(x= SCL.NN, y= ClutchCount, col = Data.4$E.TurtleCode))+
  geom_point(size=4)
#Adding the linear regression model and editing the look of the graph
plot2 + geom_line(data = fortify(fit.1), aes(x = Data.4$SCL.NN, y = .fitted)) +
   labs(title="Increasing Clutch Count with Carapace Length",
         x ="Straight Carapace Length (cm)", y = "Clutch Count",
        caption="R-squared = 0.41") +
  theme( plot.title = element_text(size=14, face="bold"),
         axis.title.x = element_text(size=14),
         axis.title.y = element_text(size=14))+
  theme(legend.title = element_text(size=14),
        legend.justification=c(1,0), 
        legend.position=c(0.95, 0.05),  
        legend.background = element_blank(),
        legend.key = element_blank()) +
  guides(color=guide_legend("Turtle ID")) 

##  ------------------------------------------------------------------------  ##
    #### looking at reoccuring females over the whole sampling period ####
##  ------------------------------------------------------------------------  ##
# Finding the amount of times the turtle has visited the island
Data.5 <- Data %>% 
  group_by(E.TurtleCode) %>%
  summarize(ntimes = n_distinct(UID)) %>% 
  arrange(desc(ntimes))
View(Data.5)
# Filtering for turtles that have apppeared more than three times
filter(Data.5, ntimes >= 3)
#selecting the data for those specific turtles. 
Data.6 <- Data %>% 
  filter(E.TurtleCode %in% c("5T", "7O", "7V", "7W", "1F", "1U", "5V", "6D", "8P", "9C", "2F", "3N", "4W", "5W", "6Q", "6W", "7Z", "8N", "9A"))
View(Data.6)

# Looking at the Data in Data.6 -- there are missing values for some measurements. 
# We will be consistant with using SCL.NN

##  ---------------------------------------------------------------------------------------------------------  ##
    #### Now we will convert CCL.NN to SCL.NN for reoccuring turtles over whole sampling period ####
##  ----------------------------------------------------------------------------------------------------------  ##

# Finding where there are NA values for SCL.NN and values for CCL.NN
# I will use the values for CCL.NN and transform them into SCL.NN
Data.7 <- Data.6 %>%
  filter(is.na(SCL.NN), !is.na(CCL.NN)) %>% 
  mutate(SCL.NN = 3.61 + 0.892*(CCL.NN))
Data.7
# This gave me 1 more observations for SCL.NN

#Lets get the other observations for SCL.NN where there were no missing values
PartData.7 <- Data.6 %>%
  filter(!is.na(SCL.NN))
PartData.7

#Now we will combind the table with the new calculated SCL.NN and the table with no missing values for SCL.NN
Data.8 <- bind_rows(Data.7, PartData.7)
View(Data.8)
#We have a total of 63 observations

# We will use SCL.NN since we use if for most statistical analysis.
# Growth throughout the years looking at the two measurements
theme_update(plot.title = element_text(hjust = 0.5))
# That just places the title of the graph in the center 


#Lets see how the turtles grow over the years
Growth.2 <- ggplot(Data.8, aes(x = Year, y = SCL.NN, color = E.TurtleCode)) +
  geom_point(size = 4) + 
  geom_smooth(method = "lm", se = FALSE)

Growth.2 + labs(title="Growth in Reoccuring Females",
              x ="Year", y = "Straight Carapace Length Notch-Notch (cm)") +
  theme( plot.title = element_text(size=14, face="bold"),
         axis.title.x = element_text(size=14),
         axis.title.y = element_text(size=14))

#Lets take the average of the measurements for each year. 
Data.9 <- Data.8 %>% 
  group_by(E.TurtleCode, Year) %>% 
  summarize(AverageLength = mean(SCL.NN))
View(Data.9)

unique(Data.9$Year)

# I want to do a linear regression on how growth changes over time but it is not letting me :(
# Now we will regraph the average lengths over time 
fit.9 <- lm(AverageLength ~ Year, data=Data.9)
summary(fit.9)

Growth3 <- ggplot(Data.9, aes(x = Year, y = AverageLength, color = E.TurtleCode)) +
  geom_point(size = 4)
Growth3
Growth3  +
  labs(title="Growth in Reoccuring Females",
              x ="Year", y = "Straight Carapace Length Notch-Notch (cm)") +
  theme( plot.title = element_text(size=14, face="bold"),
         axis.title.x = element_text(size=14),
         axis.title.y = element_text(size=14))

# Linear Regreesion between CC and SCL.NN
fit.8 <- lm(ClutchCount ~ SCL.NN, data = Data.8)
summary(fit.8)

#Plotting that relationship
# This plot adds linear regreession for each turtle, not the group as a whole
ggplot(Data.8, aes(x= SCL.NN, y= ClutchCount, color = E.TurtleCode))+
  geom_point(size=4) + 
  geom_smooth(method = "lm", se = FALSE)

# We will plot the points and add the linear regreesion model after
plot8 <- ggplot(Data.8, aes(x= SCL.NN, y= ClutchCount, col = Data.8$E.TurtleCode))+
  geom_point(size=4)
#Adding the linear regression model and editing the look of the graph
plot8 + geom_line(data = fortify(fit.8), aes(x = Data.8$SCL.NN, y = .fitted)) +
  labs(title="Increasing Clutch Count with Carapace Length",
       x ="Straight Carapace Length (cm)", y = "Clutch Count",
       caption="R-squared = 0.06") +
  theme( plot.title = element_text(size=14, face="bold"),
         axis.title.x = element_text(size=14),
         axis.title.y = element_text(size=14))+
  theme(legend.title = element_text(size=14),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  guides(color=guide_legend("Turtle ID")) 
