### main script to call the other scripts and test the system ###

# case study on ice duration Lake Mendota and Monona (WI)
##### prepare ice cover data ####
#this code is taken from https://lter.github.io/lterdatasampler/articles/ntl_icecover_vignette.html
library(lterdatasampler)
library(tidyverse)

ntl_icecover <- ntl_icecover %>%
  drop_na(ice_duration) %>% 
  filter(year>1884)%>%
  group_by(year) %>%
  group_by(lakeid)

## prepare air temperature data
ntl_airtemp_avg <- ntl_airtemp %>% 
  filter(year > 1884) %>%   # filter out the known biased data
  group_by(year) %>%
  summarise(avg_air_temp_adjusted = mean(ave_air_temp_adjusted, na.rm=TRUE))

# add a column to group the Fall and Spring season into a same year, similarly to what is done when defining hydrological year
ntl_airtemp_hydro <- ntl_airtemp %>%
  mutate(hydroyear = if_else(month(sampledate) < 10, year-1, year))

# compute the average air temperature from Nov to April
ntl_airtemp_avg_winter <-  ntl_airtemp_hydro %>%
  filter(month(sampledate) %in% c(11:12,1:4)) %>% # filter the months from Nov to April
  group_by(hydroyear) %>%
  summarise(avg_air_temp_adjusted = mean(ave_air_temp_adjusted))

ntl_joined_avg <- ntl_icecover%>%
  left_join(by = c("year" = "hydroyear"), ntl_airtemp_avg_winter)%>%
  drop_na()%>%
  rename(MAT_winter = avg_air_temp_adjusted, lake = lakeid)%>%
  mutate(lake = recode(lake,"Lake Mendota" = "Mendota",
                            "Lake Monona" = "Monona"))

# remnant levels cause problems in the plotting function
ntl_joined_avg <- droplevels(ntl_joined_avg)

##### prepare everything to run the analyse()-function ####
# check for right working directory:
if(endsWith(getwd(), "scripts")){
  cat("Great, your working directory is set correctly!")
} else{
  warning(paste)("Attention! Your working directory must be the folder 'scripts',\n
            but your current working directory is ", getwd(), ".")}

# install / load packages used by AS
source("helper_functions.R")
install_packages_AS() # installs packages needed in AS in cases not yet installed, and loads them

# create folder structure for outputs
create_folder()

# get analyse function 
source("analyse.R")


##### run the analyse function ####

result = analyse(formula = ice_duration ~ MAT_winter * lake, 
                 data = ntl_joined_avg, 
                 dist = "uninormal", 
                 mode = "test", 
                 verbose = T, 
                 report_type = "pdf")

