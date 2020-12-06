# ENVS 4826 Course Project - Mapping Script | Jenacy Samways | A00418300

# Loading the data 

usaram_summary <- read.csv("data/nwca2011_usaram_summary.csv")
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

# Mapping data points by region

library(ggplot2)
library(sf)
library(stars)

write.csv(wetlanddata, file = "data/wetlanddata.csv")

wetlandmap <- st_read("data/wetlanddata.csv",
                      options = c("X_POSSIBLE_NAMES=longitude", 
                                  "Y_POSSIBLE_NAMES=latitude"),
                      crs = 4326, stringsAsFactors=T)

wetlandmap <- wetlandmap[!is.na(wetlanddata$total_invasive_cover),]
str(wetlandmap)
wetlandmap$total_invasive_cover <- factor(wetlandmap$total_invasive_cover,levels=c("ABSENT","<5%","5-25%","26-75%",">75%"))
levels(wetlandmap$total_invasive_cover)


library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearth")
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data=wetlandmap, mapping = aes(color = region)) +
  coord_sf(xlim = c(-132, -62), ylim = c(22, 53), expand = FALSE) +
  ggtitle("United States 2011 Wetland Survey Sites") +
  theme(plot.title = element_text(hjust = 0.5, size = 18))

# Mapping data points by invasive cover 
library(RColorBrewer)

ggplot(data = world, na.rm = TRUE) +
  geom_sf() +
  geom_sf(data=wetlandmap, mapping = aes(color = total_invasive_cover)) +
  coord_sf(xlim = c(-132, -62), ylim = c(22, 53), expand = FALSE) +
  ggtitle("Wetland Survey Sites") +
  scale_colour_brewer("Total invasive cover", palette = "YlOrRd", breaks = levels(wetlandmap$total_invasive_cover), labels = c("Absent", "< 5%", "5-25%","26-75%",">75%")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) 
