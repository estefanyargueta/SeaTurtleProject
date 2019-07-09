#### Getting Started ####
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

Data.4 <- Data %>% 
  filter(E.TurtleCode %in% c("5T", "7O", "7V", "7W"))
View(Data.4)

#### Looking at the Data in Data.4 -- the measurements with no missing data are SCL.NN and CCL.NN. We will use those for statistical analysis. ####
# Growth throughout the years looking at the two measurements
theme_update(plot.title = element_text(hjust = 0.5))

Growth <- ggplot(Data.4, aes(x = Year, y = SCL.NN, color = E.TurtleCode)) +
  geom_point(size = 4)

Growth + labs(title="Growth in Reoccuring Females",
              x ="Year", y = "Straight Carapace Length Notch-Notch (cm)") +
  theme( plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14))

ggplot(Data.4, aes(x= Year, y= CCL.NN, color = E.TurtleCode))+
  geom_point(size=4)

fit.1 <- lm(ClutchCount ~ SCL.NN, data = Data.4)
summary(fit.1)
ggplot(Data.4, aes(x= SCL.NN, y= ClutchCount, color = E.TurtleCode))+
  geom_point(size=4) + 
  geom_smooth(method = "lm", se = FALSE)

plot2 <- ggplot(Data.4, aes(x= SCL.NN, y= ClutchCount, col = Data.4$E.TurtleCode))+
  geom_point(size=4)

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

####### 
