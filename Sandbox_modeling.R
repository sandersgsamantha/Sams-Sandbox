## Load necessary packages
require(ggplot2)
require(SIBER)
require(dplyr)
require(tidyr)
require(ggpubr)
require(nicheROVER)
library(tidyverse)
library(rstatix)

## Set your working directory
#setwd("C:/Users/...") # If on a PC
#setwd("/Users/...") # If on a Mac
#setwd("~/Desktop/KI_fish/data")

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
      TRUE ~ "Other"                     
    ))

#create a column for east.in east.out west.in west.out
fish.bulk <- fish.bulk %>%
  mutate(shoal.spot = paste0(east_vs_west, ".", in_vs_out))
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
# remove non-target species
fish.bulk <- subset(fish.bulk, species != "Lizardfish" & species != "Pigfish") 

# fix species names
levels(fish.bulk$species)[2] <- "Atlantic Croaker"

#fish.bulk$dist_cat <- factor(fish.bulk$dist_cat, level = c("Very Low", "Medium", "Very High"))
#fish.bulk$species <- factor(fish.bulk$species, level = c("LU.FULV", "CE.UROD", "LU.BOHA","CE.ARGU","CA.MELA","AP.FURC"))

### Bulk SIA summary statistics  -----------------------------------------------------------

# Species
fish.bulk %>% 
  group_by(species) %>%
  summarize(n = n(), meanSL = mean(sl.mm, na.rm = TRUE)/10,
            minSL = min(sl.mm, na.rm = TRUE)/10,
            maxSL = max(sl.mm, na.rm = TRUE)/10)

# Density plot
# data should not be skewed if normal 
ggdensity(fish.bulk$weight.g, fill="lightgrey" )
ggdensity(fish.bulk$sl.mm, fill="lightgrey" )

ggqqplot(fish.bulk$weight.g)+
  ggtitle("Fish Weights") # normalish
ggqqplot(fish.bulk$sl.mm)+
  ggtitle("Fish Standard Length") 
ggqqplot(fish.bulk$tl.mm)+
  ggtitle("Fish Total Lengths") #normal ish

############## Shapiro-Wilk's test#################
#' This is another way to test for normality but with statistical values instead
#' of visually. These results can be sensitive to sample size 
#' (small samples normally pass normality test) so it's important to run this 
#' with visual data (qqplot)
#' Null hypothesis = p> 0.05 and sample distribution is normal 
#' Hypothesis= p< 0.05 the distribution is not normal and data is significantly 
#' different from normal distribution 
#' 
fish.bulk%>% shapiro_test(tl.mm,sl.mm,fl.mm,weight.g) 
#not normal but will check this with subset of data by species

#Subset data by species ####
data.CRO<- subset(fish.bulk, species== "Atlantic Croaker")
data.ATB<- subset(fish.bulk, species== "Atlantic Bumper")
data.SPO<- subset(fish.bulk, species== "Spot")
data.PIN<- subset(fish.bulk, species== "Pinfish")
data.THH<- subset(fish.bulk, species== "Thread Herring")
data.WEF<- subset(fish.bulk, species== "Weakfish")
data.MOF<- subset(fish.bulk, species== "Moonfish")
data.STA<- subset(fish.bulk, species== "Striped Anchovy")

#looking at data based off fish weights and size
ggdensity(data.CRO$tl.mm, fill="lightgrey")
ggqqplot(data.CRO$sl.mm)+
  ggtitle("CRO Standard Length")
ggqqplot(data.CRO$weight.g)+
  ggtitle("CRO Weight")

# Histogram and QQ plot
hist(data.CRO$weight.g)
qqnorm(data.CRO$weight.g); qqline(data.CRO$weight.g)

data.ATB%>%shapiro_test(tl.mm, sl.mm,fl.mm, weight.g)
data.CRO%>%shapiro_test(tl.mm, sl.mm,fl.mm, weight.g) 
data.MOF%>%shapiro_test(tl.mm, sl.mm,fl.mm, weight.g)
data.PIN%>%shapiro_test(tl.mm, sl.mm,fl.mm, weight.g)
data.SPO%>%shapiro_test(tl.mm, sl.mm,fl.mm, weight.g)
data.STA%>%shapiro_test(tl.mm, sl.mm,fl.mm, weight.g)
data.THH%>%shapiro_test(tl.mm, sl.mm,fl.mm, weight.g)
data.WEF%>%shapiro_test(tl.mm, sl.mm,fl.mm, weight.g)

# Transform if skewed
# Log transformation most common
data.CRO$log_weight <- log(data.CRO$weight.g)
data.CRO$log_sl.mm<- log(data.CRO$sl.mm)


#recheck normality 
hist(data.CRO$log_weight)
qqnorm(data.CRO$log_weight); qqline(data.CRO$log_weight)
data.CRO%>%shapiro_test(log_sl.mm,log_weight)
  #log_sl.mm (p) == 4.36e-17 need more transforming
  #log_weight (p) == 6.17e- 1. good

#lets try fixing the sl.mm with sqr.rt
data.CRO$sqrt_sl.mm <- sqrt(data.CRO$sl.mm)
#CHECK
hist(data.CRO$sqrt_sl.mm)
qqnorm(data.CRO$sqrt_sl.mm); qqline(data.CRO$sqrt_sl.mm)
data.CRO%>%shapiro_test(sqrt_sl.mm)
#NOPE lets try this 

library(MASS)
#boxcox
boxcox_sl_length <- boxcox(lm(sl.mm ~ 1, data = data.CRO))
lambda_l <- boxcox_sl_length$x[which.max(boxcox_sl_length$y)]
data.CRO$bc_sl.mm <- (data.CRO$sl.mm^lambda_l - 1) / lambda_l

hist(data.CRO$bc_sl.mm)
qqnorm(data.CRO$bc_sl.mm); qqline(data.CRO$bc_sl.mm)
data.CRO%>%shapiro_test(bc_sl.mm)
#NOPE

# Univariate models
model1 <- lm(d15N ~ sl.mm, data = data.CRO)
model2 <- lm(d15N ~ weight.g, data = data.CRO)
# Or both together
model3 <- lm(d15N ~ sl.mm + weight.g, data = data.CRO)

model4 <- lm(d13C ~ sl.mm, data = data.CRO)
model5 <- lm(d13C ~ weight.g, data = data.CRO)
model6 <- lm(d13C ~ sl.mm + weight.g, data = data.CRO)

# Summary output
summary(model1) #standard length vs d15N
summary(model2) #weight vs d15N
summary(model3)
summary(model4) #standard length vs d13C
summary(model5) #weight vs d13C
summary(model6)

# Add species to the model
anova(model4)

ggplot(data.CRO, aes(x = d13C, y = sl.mm)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

ggplot(data.CRO, aes(x = d15N, y = sl.mm)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

ggplot(data.CRO, aes(x = d13C, y = weight.g)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

ggplot(data.CRO, aes(x = d15N, y = weight.g)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

#looking at date and Site
data.CRO$collection.date <- as.factor(data.CRO$collection.date)  # if it's categorical
data.CRO$site <- as.factor(data.CRO$site)

# Visualize Data####
library(ggplot2)

# Boxplot by site

    ### CROAKER ####
#SITE
ggplot(data.CRO, aes(x = site, y = c.n, fill= site)) +
  geom_boxplot() +
  labs(title = "C:N values by Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.CRO, aes(x = site, y = d13C, fill= site)) +
  geom_boxplot() +
  labs(title = "Carbon Isotope values by Site")
ggplot(data.CRO, aes(x = site, y = d15N, fill= site)) +
  geom_boxplot() +
  labs(title = "Nitrogen Isotope values by Site")


# Boxplot by Season
ggplot(data.CRO, aes(x = season, y = c.n, fill= season)) +
  geom_boxplot() +
  labs(title = "C:N values by Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.CRO, aes(x = season, y = d13C, fill= season)) +
  geom_boxplot() +
  labs(title = "Carbon Isotope values by Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.CRO, aes(x = season, y = d15N, fill= season)) +
  geom_boxplot() +
  labs(title = "Nitrogen Isotope values by Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot by month-year
ggplot(data.CRO, aes(x = month.year, y = d13C, fill= site)) +
  geom_boxplot() +
  labs(title = "Carbon Isotope values by Site and Date")
ggplot(data.CRO, aes(x = month.year, y = d15N, fill=site)) +
  geom_boxplot() +
  labs(title = "Nitrogen Isotope values by Site and Date")
ggplot(data.CRO, aes(x = month.year, y = c.n, fill = site)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "C:N values by Site and Date")

anova_model <- aov(d15N ~ site * collection.date, data = data.CRO)
summary(anova_model)

library(emmeans)

# Pairwise comparisons
emmeans(anova_model, pairwise ~ site)
emmeans(anova_model, pairwise ~ collection.date)
emmeans(anova_model, pairwise ~ site | collection.date)  # site differences at each date

lm_model <- lm(d13C ~ site * collection.date, data = data.CRO)
summary(lm_model)


