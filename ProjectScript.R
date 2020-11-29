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

ggplot(data = world) +
  geom_sf() +
  geom_sf(data=wetlandmap, mapping = aes(color = total_invasive_cover)) +
  coord_sf(xlim = c(-132, -62), ylim = c(22, 53), expand = FALSE) +
  ggtitle("Wetland Survey Sites") +
  scale_colour_brewer("Total invasive cover", palette = "YlOrRd", labels = c("Absent", "< 5%", "5-25%","26-75%",">75%")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) 

# Recoding total invasive cover

wetlanddata <- wetlanddata %>% mutate(total_invasive_cover=recode(total_invasive_cover, 
                         `ABSENT`="0",
                         `<5%`="1",
                         `5-25%`="2",
                         `26-75%`="3",
                         `>75%`="4"))


# Grouping and averaging by region

wetlanddata$total_invasive_cover <- as.numeric(as.character(wetlanddata$total_invasive_cover))

by_region <- group_by(wetlanddata, region)
cover_avg_by_region <- summarize(by_region,
                             avg_invasive= mean(total_invasive_cover, na.rm = TRUE))

# Data filtering - probably unnessecary

AridWest <- filter(wetlanddata, region == "Arid West")
Atlantic <- filter(wetlanddata, region == "Atlantic and Gulf Coastal Plain")
EasternMountains <- filter(wetlanddata, region == "Eastern Mountains and Piedmont")
GreatPlains <- filter(wetlanddata, region == "Great Plains")
Midwest <- filter(wetlanddata, region == "Midwest")
North <- filter(wetlanddata, region == "Northcentral and Northeast")
West <- filter(wetlanddata, region == "Western Mountains, Valleys, and Coast")

# Data visualization - boxplots

library(ggplot2)


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


# Modelling the data

library(glm2)
human_use_regression <- glm(total_invasive_cover ~ human_use_severity, family="poisson", data=wetlanddata)

par(mfrow=c(2,2))
plot(human_use_regression)

combined_regression <- glm(total_invasive_cover ~ human_use_severity + fire_severity + grazing_severity, family="poisson", data=wetlanddata)

par(mfrow=c(2,2))
plot(combined_regression)

ggplot(wetlanddata, aes(x = human_use_severity, y = total_invasive_cover)) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_classic()



