# ENVS 4826 Course Project - Unused Script | Jenacy Samways | A00418300

# Data prep

usaram_summary <- read.csv("data/nwca2011_usaram_summary.csv")
usaram_siteinfo <- read.csv("data/nwca2011_siteinfo copy.csv")

wetlandsurveydata <- usaram_summary[-c(1,3,4,6,8:9,11,15:23,28)]
wetlandlocationdata <- usaram_siteinfo[c(2,9,10,14)]

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

# Testing linear model

library(lme4)

testmodel <- lm(wetlanddata$human_use_severity ~ wetlanddata$total_invasive_cover)

summary(testmodel)

par(mfrow=c(2,2))
plot(testmodel)

ggplot(wetlanddata, aes(x = total_invasive_cover, y = human_use_severity)) +
  geom_point()+
  geom_smooth(method = "lm")

# Can conclude from the plots and summary that no significant relationship is present at this broad of a scale. 

# Exploration of subregion 

wetlanddata$region <- as.factor(wetlanddata$region)
table(wetlanddata$region)

# Exploration of subregion as a nested variable

mixedmodel <- lmer(human_use_severity ~ total_invasive_cover + (1|region), 
                   data = wetlanddata, REML = FALSE)
summary(mixedmodel)

ggplot(wetlanddata, aes(y = human_use_severity, x = total_invasive_cover, color = region)) +  
  geom_point(size = 5, alpha = 0.5) + 
  geom_line(aes(group = region)) +
  theme_classic()

# Still no signifciant effects / suitable fit using a linear mixed model. 

# Grouping and averaging by region

wetlanddata$total_invasive_cover <- as.numeric(as.character(wetlanddata$total_invasive_cover))

by_region <- group_by(wetlanddata, region)
cover_avg_by_region <- summarize(by_region,
                                 avg_invasive= mean(total_invasive_cover, na.rm = TRUE))

