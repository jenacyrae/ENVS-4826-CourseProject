# ENVS 4826 Course Project | Jenacy Samways | A00418300

# Loading the data 

usaram_summary <- read.csv("data/nwca2011_usaram_summary.csv")
usaram_attributes <- read.csv("data/nwca2011_usaram_attributes.csv")
usaram_siteinfo <- read.csv("data/nwca2011_siteinfo copy.csv")

# Data cleaning

wetlandsurveydata <- usaram_summary[-c(1,3,4,6,8:9,11,15:23,28)]
wetlandlocationdata <- usaram_siteinfo[c(2,9,10,14)]

# Data joining 

library(dplyr)

wetlanddata <- inner_join(wetlandsurveydata,wetlandlocationdata, by = c("UID"))

# Further data cleaning

library(tidyverse)
library(janitor)

wetlanddata <- wetlanddata %>% 
  janitor::clean_names()

wetlanddata <- rename(wetlanddata,"UID"="uid","agri_severity"="agr_severity_m3","habitat_severity"="habitat_severity_m3","hydro_severity"="hyd_stressors_severity_m3","landuse_severity"="landuse_severity_m3","total_invasive_cover"="total_invasive_cover_m11","grazing_severity"="excessive_grazing_severity_m12","fire_severity"="fire_severity_m12","human_use_severity"="human_use_severity_m12","latitude"="analysis_lat","longitude"="analysis_lon","region"="coe_region")

# Removing duplicate site visits

wetlanddata <- wetlanddata[wetlanddata$visit_no != 2, ]

# Data frames for each region

AridWest <- filter(wetlanddata, region == "Arid West")
Atlantic <- filter(wetlanddata, region == "Atlantic and Gulf Coastal Plain")
EasternMountains <- filter(wetlanddata, region == "Eastern Mountains and Piedmont")
GreatPlains <- filter(wetlanddata, region == "Great Plains")
Midwest <- filter(wetlanddata, region == "Midwest")
North <- filter(wetlanddata, region == "Northcentral and Northeast")
West <- filter(wetlanddata, region == "Western Mountains, Valleys, and Coast")

# Converting invasive cover values to metric scale

wetlanddata <- wetlanddata %>% mutate(total_invasive_cover=recode(total_invasive_cover, 
                                                                  `ABSENT`="0",
                                                                  `<5%`="1",
                                                                  `5-25%`="2",
                                                                  `26-75%`="3",
                                                                  `>75%`="4"))

wetlanddata$total_invasive_cover <- as.numeric(as.character(wetlanddata$total_invasive_cover))

# Data visualization - histogram

library(ggplot2)

ggplot(wetlanddata, aes(x=total_invasive_cover)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  labs(x = "\nTotal Invasive Cover", y = "Count") +
  ggtitle("Distribution of total invasive cover values\n") +
  theme(
    panel.grid = element_blank(),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),
    plot.title = element_text(hjust = 0.5)
  )
  

# Data visualization - boxplots


ggplot(data = wetlanddata, aes(x=region, y=total_invasive_cover)) + 
  geom_boxplot() +
  facet_wrap(~ region, scales = "free") +
  ggtitle("Distribution of total invasive cover values by region\n") +
  labs(x = "\nRegion", y = "Total Invasive Cover\n") +
  theme(
    panel.grid = element_blank(),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"),
    plot.title = element_text(hjust = 0.5)
  )

# Filtering the data by region

AridWest <- filter(wetlanddata, region == "Arid West")
Atlantic <- filter(wetlanddata, region == "Atlantic and Gulf Coastal Plain")
EasternMountains <- filter(wetlanddata, region == "Eastern Mountains and Piedmont")
GreatPlains <- filter(wetlanddata, region == "Great Plains")
Midwest <- filter(wetlanddata, region == "Midwest")
North <- filter(wetlanddata, region == "Northcentral and Northeast")
West <- filter(wetlanddata, region == "Western Mountains, Valleys, and Coast")

# Modelling the data

library(glm2)
human_use_regression <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=wetlanddata)
summary(human_use_regression)

humanfire_regression <- glm(total_invasive_cover ~ human_use_severity + fire_severity, family="poisson", data=wetlanddata)
summary(humanfire_regression)

humangrazing_regression <- glm(total_invasive_cover ~ human_use_severity + grazing_severity, family="poisson", data=wetlanddata)
summary(humangrazing_regression)

# Modelling the data by region

AridWest_glm <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=AridWest)
summary(AridWest_glm)
Atlantic_glm <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=Atlantic)
summary(Atlantic_glm)
EasternMountains_glm <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=EasternMountains)
summary(EasternMountains_glm)
GreatPlains_glm <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=GreatPlains)
summary(GreatPlains_glm)
Midwest_glm <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=Midwest)
summary(Midwest_glm)
North_glm <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=North)
summary(North_glm)
West_glm <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=West)
summary(West_glm)
