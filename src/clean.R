#Data Wrangling

## CLEANING DATA
## Author: Tarini Bhatnagar
## date: "01/18/2018"

# This script takes raw data as input, performs some tidying tasks and writes the data to a new output .csv file. 

# Dependencies 
# tidyverse

# Input: data/database.csv

# Data cleaning tasks:
# Convert csv to dataframe
# Selecting relevant columns 
# Replace spaces in attribute names with `_`.

# Output: src/tidy_eq_data.csv 


library(tidyverse)

#Loading raw data
dat <- read_csv("data/database.csv")

#Selecting specific columns

eq_data <- dat %>% 
  select("Date", "Time", "Latitude", "Longitude", "Type", "Depth", "Magnitude", "Magnitude Type", "ID", "Status")

#Replacing spaces in column names with _
names(eq_data)<-names(eq_data)%>% 
  stringr::str_replace_all("\\s","_")

#Write tidy data to a new .csv file
write.csv(eq_data, file="src/tidy_eq_data", row.names=FALSE)


