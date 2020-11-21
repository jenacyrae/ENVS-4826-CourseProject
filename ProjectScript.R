# ENVS 4826 Course Project | Jenacy Samways | A00418300

# Loading the data 

usaram_summary <- read.csv("data/nwca2011_usaram_summary.csv")
usaram_attributes <- read.csv("data/nwca2011_usaram_attributes.csv")
usaram_siteinfo <- read.csv("data/nwca2011_siteinfo copy.csv")

# Data cleaning

wetlandsurveydata <- usaram_summary[-c(1,4:6,8:9,11,15:23,28)]
wetlandlocationdata <- usaram_siteinfo[c(2,9,10,14,28)]

# Data joining 

wetlanddata <- inner_join(wetlandsurveydata,wetlandlocationdata, by = c("UID"))

# Further data cleaning

library(tidyverse)
library(janitor)

wetlanddata <- wetlanddata %>% 
  janitor::clean_names()

wetlanddata <- rename(wetlanddata,"UID"="uid","date"="date_col","agri_severity"="agr_severity_m3","habitat_severity"="habitat_severity_m3","hydro_severity"="hyd_stressors_severity_m3","landuse_severity"="landuse_severity_m3","total_invasive_cover"="total_invasive_cover_m11","grazing_severity"="excessive_grazing_severity_m12","fire_severity"="fire_severity_m12","human_use_severity"="human_use_severity_m12","latitude"="analysis_lat","longitude"="analysis_lon","region"="coe_region","major_river_basin"="maj_river_basin")


