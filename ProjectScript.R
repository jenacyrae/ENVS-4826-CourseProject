# ENVS 4826 Course Project | Jenacy Samways | A00418300

# Loading the data 

usaram_summary <- read.csv("data/nwca2011_usaram_summary.csv")
usaram_attributes <- read.csv("data/nwca2011_usaram_attributes.csv")
usaram_siteinfo <- read.csv("data/nwca2011_siteinfo copy.csv")

# Data cleaning

wetlanddata <- usaram_summary[-c(1,6,8:9,11,15:23,28)]
wetlandlocationdata <- usaram_siteinfo[c(2,9,10,14,28)]
