#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "magrittr", "sf", "tidycensus", "readxl", "units", "scales")
pack(packages)
rm(pack, packages)
options(scipen=999)

#import processed data
source("Data Processing.R")

#Convert bridges df to sf

bridges_sf <- bridges_region %>% filter(!is.na(latitude) & !is.na(longitude)) %>%
st_as_sf(coords = c("latitude", "longitude"))

bridges_sf %>%
  filter(year == 2018 & longitude_adj >70) %>%
ggplot() +
  geom_sf()