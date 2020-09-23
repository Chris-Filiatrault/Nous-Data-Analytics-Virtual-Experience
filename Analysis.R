

# ==================================================================================================================================================================
# How has the student to teacher ratio in Queensland’s government-run high schools compared over time with:
# - other schools in the state
# - those in NSW and Victoria?
# ==================================================================================================================================================================

setwd("~/Documents/Data Analysis/GitHub/Nous' Data Analytics Virtual Experience")

# install.packages("readxl")
library(readxl)
library(ggplot2)
library(tidyverse)

data <- data.frame(read_excel("table 53a student (fte) to teaching staff (fte) ratios, 2006-2019.xls", sheet = 2, skip = 4, n_max = 1891))

# Note number of rows
nrow(data) #1890


# =========================================
# =============DATA CLEANING===============
# =========================================

# ===INSPECT===
head(data) 
str(data)
summary(data)

# ===REMOVE UNNEEDED CHARACTERS===
# need to remove first two characters of State/Territory
# first, check that the second value of every string in State.Teritory is an empty space 
# meaning the string should start with "a " or "b " or "c " or "d ", and we're safe to remove those values
state_index <- substr(data$State.Territory, start = 2, stop = 2) == " "
all(state_index) # TRUE 
data$State.Territory <- substring(data$State.Territory, 3)

# same process for Affiliation and School.Level
affiliation_index <- substr(data$Affiliation, start = 2, stop = 2) == " "
all(affiliation_index) # TRUE 
data$Affiliation <- substring(data$Affiliation, 3)


school_level_index <- substr(data$School.Level, start = 2, stop = 2) == " "
all(school_level_index) # TRUE 
data$School.Level <- substring(data$School.Level, 3)

head(data)


# ===CHANGE DATA TYPES AS NEEDED===
str(data)

# change Year, State.Territory, Affiliation & School.Level to a factor

head(data)
data$Year <- as.factor(data$Year)
data$State.Territory <- as.factor(data$State.Territory)
data$Affiliation <- as.factor(data$Affiliation)
data$School.Level <- as.factor(data$School.Level)


# ===FIX FACTOR NAMES===

# Need to capitalise School.Level factor names
# change "Primary school" to "Primary School", and "Secondary school" to "Secondary School"
levels(data$School.Level)
data$School.Level[data$School.Level == "Primary school"] <- "Primary School"
data$School.Level[data$School.Level == "Secondary school"] <- "Secondary School"



# =========================================
# ======Filter, Aggregate & plot data======
# =========================================


# ===Qld government vs other Qld schools===

# Filter (Qld only)
qld_data <- data[data$State.Territory=="Qld",]

# Aggregate data
qld_aggregate <- aggregate(qld_data$Student.to.Teaching.Staff.Ratio, by=list(qld_data$Year, qld_data$Affiliation), mean)
names(qld_aggregate) <- c("Year", "Affiliation", "STR")

# Plot
ggplot(qld_aggregate, aes(x = as.numeric(Year), y = STR, col=Affiliation)) +
  geom_line() +
  ggtitle("Student/Teacher ratio in QLD schools by affiliation") +
  xlab("Year") +
  ylab("Student/Teacher Ratio") +
  scale_x_discrete(limits = levels(qld_aggregate$Year))




# ===Qld government vs NSW & VIC schools===  

# Filter (NSW, VIC, and QLD Government schools)
qld_nsw_vic <- data[data$State.Territory %in% c("NSW", "Vic.") | data$State.Territory == "Qld" & data$Affiliation == "Government",] 

# Aggregate 
qld_nsw_vic_aggregate <- aggregate(qld_nsw_vic$Student.to.Teaching.Staff.Ratio, by=list(qld_nsw_vic$Year, qld_nsw_vic$State.Territory), mean)
names(qld_nsw_vic_aggregate) <- c("Year", "State", "STR")

# Plot
ggplot(qld_nsw_vic_aggregate, aes(x = as.numeric(Year), y = STR, col=State)) +
  geom_line() +
  ggtitle("Student/Teacher ratio in QLD (Government), NSW (all) & VIC (all) schools") +
  xlab("Year") +
  ylab("Student/Teacher Ratio") +
  scale_x_discrete(limits = levels(qld_aggregate$Year)) +
  scale_color_manual(labels = c("NSW","QLD Gov", "VIC"), values = c("blue", "red", "black"))



# INCLUDE SOURCE IN PHOTOS!!!
  


