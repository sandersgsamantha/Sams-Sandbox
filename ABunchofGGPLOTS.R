#PLOT ON PLOTS ON PLOTS # 
#Date: 08-14-25

#### SETUP ####
## Load necessary packages
require(ggplot2)
require(dplyr)
require(tidyr)
require(ggpubr)
library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-UNC-Wilmington/Fish Team/BOEM/Data Analysis/Sams Sandbox/")

## Load the data
SIA <- read.csv("boem_bulk_CN_data_250817.csv")
metadata <- read.csv("boem_sia_metadata_250814.csv")


#checking for dupliates
duplicated(metadata$fish.id)

fish.bulk <- merge(SIA,metadata, by ="fish.id") # merge two datasets by "fish.id" column

#### ADDING MONTH & SEASON COLUMN ####
#create a column for months based off "collection.date"
#need to first make "collection data at object
class(fish.bulk$collection.date) #classified as a character
fish.bulk$collection.date <- as.Date(fish.bulk$collection.date, format= "%m/%d/%Y")
#new column object <- as.Date = read as date from (fish.bulk$collection.date, and its in X format)
#now add a month Column
fish.bulk<- fish.bulk %>% 
  mutate(month= format(collection.date, "%B"))

#create a column for seasons
fish.bulk<- fish.bulk %>% 
  mutate(season=case_when(
    month %in% c("December", "January","February") ~ "Winter",
    month %in% c("March", "April","May") ~ "Spring",
    month %in% c("June", "July","August") ~ "Summer",
    month %in% c("September", "October", "November") ~ "Autumn"
  ))

#create column for month-year
fish.bulk$month.year <- format(fish.bulk$collection.date, "%B-%y")

#create a column for east vs west
# Create the 'east_vs_west' column
fish.bulk <- fish.bulk %>%
  mutate(
    east_vs_west = case_when(
      str_detect(site, "E") ~ "East",  # Checks if the 'site' column contains the word "East"
      str_detect(site, "W") ~ "West",  # Checks if the 'site' column contains the word "West"
      TRUE ~ "Other"                     # Assigns "Other" to any remaining sites
    )
  )
#create a column for in vs out
fish.bulk <- fish.bulk %>%
  mutate(
    in_vs_out = case_when(
      str_detect(site, "I") ~ "In",  # Checks if the 'site' column contains the word "I"
      str_detect(site, "O") ~ "Out",  # Checks if the 'site' column contains the word "O"
      TRUE ~ "Other"                     # Assigns "Other" to any remaining sites
    ))
#shoal distance
fish.bulk <- fish.bulk %>%
  mutate(
    distance = case_when(
      str_detect(site, "1") ~ "inshore",  
      str_detect(site, "2") ~ "midshore",
      str_detect(site,"3")~ "offshore",
      TRUE ~ "Other"))

#create a column for east.in east.out west.in west.out
fish.bulk <- fish.bulk %>%
  mutate(
    shoal.spot = paste0(east_vs_west, ".", in_vs_out))

#view data
head(fish.bulk)

#### MOVE HERE ####
str(fish.bulk) # ensure data in correct formats.

# data that will be assigned unique colors or shapes generally need to be a factor.
fish.bulk$fish.id <- as.factor(fish.bulk$fish.id) 
fish.bulk$species <- as.factor(fish.bulk$species) 
fish.bulk$site <- as.factor(fish.bulk$site)
fish.bulk$month <- as.factor(fish.bulk$month)
fish.bulk$season <- as.factor (fish.bulk$season)
fish.bulk$distance<- as.factor(fish.bulk$distance)
fish.bulk$shoal.spot<- as.factor(fish.bulk$shoal.spot)
fish.bulk$in_vs_out<- as.factor(fish.bulk$in_vs_out)
fish.bulk$east_vs_west<- as.factor(fish.bulk$east_vs_west)
fish.bulk$month.year<- as.factor(fish.bulk$month.year)

# columns with numbers and "NA" are often initially assigned as "chr = character" or "int = integer (i.e., no decimal points)" but should be "num = numeric" (allows decimal points)
fish.bulk$weight.g <- as.numeric(fish.bulk$weight.g)
fish.bulk$tl.mm <- as.numeric(fish.bulk$tl.mm)
fish.bulk$sl.mm <- as.numeric(fish.bulk$sl.mm)
fish.bulk$fl.mm <- as.numeric(fish.bulk$fl.mm)

# manually check the data set to ensure modifications were applied correctly
str(fish.bulk)
summary(fish.bulk$species)
# remove non-target species
fish.bulk <- subset(fish.bulk, species != "Lizardfish" & species != "Pigfish") 

# fix species names
levels(fish.bulk$species)[2] <- "Atlantic Croaker"

# Species
fish.bulk %>% 
  group_by(species) %>%
  summarize(n = n(), meanSL = mean(sl.mm, na.rm = TRUE)/10,
            minSL = min(sl.mm, na.rm = TRUE)/10,
            maxSL = max(sl.mm, na.rm = TRUE)/10)

#Subset by species
data.CRO<- subset(fish.bulk, species== "Atlantic Croaker")
data.ATB<- subset(fish.bulk, species== "Atlantic Bumper")
data.SPO<- subset(fish.bulk, species== "Spot")
data.PIN<- subset(fish.bulk, species== "Pinfish")
data.THH<- subset(fish.bulk, species== "Thread Herring")
data.WEF<- subset(fish.bulk, species== "Weakfish")
data.MOF<- subset(fish.bulk, species== "Moonfish")
data.STA<- subset(fish.bulk, species== "Striped Anchovy")

# Visualize Data####
library(ggplot2)

# Boxplot by site
#SITE
ggplot(fish.bulk, aes(x = site, y = c.n, fill= site)) +
  geom_boxplot() +
  labs(title = "C:N values by Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(fish.bulk, aes(x = site, y = d13C, fill= site)) +
  geom_boxplot() +
  labs(title = "Carbon Isotope values by Site")
ggplot(fish.bulk, aes(x = site, y = d15N, fill= site)) +
  geom_boxplot() +
  labs(title = "Nitrogen Isotope values by Site")

# Boxplot by Season
ggplot(fish.bulk, aes(x = season, y = c.n, fill= month.year)) +
  geom_boxplot() +
  labs(title = "C:N values by Season") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(fish.bulk, aes(x = season, y = d13C, fill= month.year)) +
  geom_boxplot() +
  labs(title = "Carbon Isotope values by Season") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(fish.bulk, aes(x = season, y = d15N, fill= month.year)) +
  geom_boxplot() +
  labs(title = "Nitrogen Isotope values by Season") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot by month-year
# Use mutate() to reorder the 'month.year' column
fish.bulk <- fish.bulk %>%
  mutate(month.year = fct_reorder(month.year, collection.date))
#Plot
ggplot(data = fish.bulk, aes(x = month.year, y = c.n, fill = month.year)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "C:N values by Month and Year")
ggplot(fish.bulk, aes(x = month.year, y = d13C, fill= month.year)) +
  geom_boxplot() +
  labs(title = "Carbon Isotope values by Month and Year")
ggplot(fish.bulk, aes(x = month.year, y = d15N, fill=month.year)) +
  geom_boxplot() +
  labs(title = "Nitrogen Isotope values by Month and Year")

#Boxplot IN vs OUT
ggplot(data = fish.bulk, aes(x =in_vs_out, y = c.n, fill =distance )) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "C:N values by Month and Year")
ggplot(fish.bulk, aes(x =in_vs_out , y = d13C, fill= distance)) +
  geom_boxplot() +
  labs(title = "Carbon Isotope values by Month and Year")
ggplot(fish.bulk, aes(x = in_vs_out, y = d15N, fill=distance)) +
  geom_boxplot() +
  labs(title = "Nitrogen Isotope values by Month and Year")

