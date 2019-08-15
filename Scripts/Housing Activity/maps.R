#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readxl", "tidyr", "lubridate", "ggplot2", "tidyverse", "readr", "sf", "viridis")
pack(packages)
rm(pack, packages)

#import data
source("import.R")
source("data_processing.R")

municSF <- st_read("../Data/Boundary Shapefiles/munic/munic.shp") %>% left_join(permits, by = c("GEOID_10" = "geoid"))

countySF <- st_read("../Data/Boundary Shapefiles/counties/counties.shp") %>% left_join(permitsCounty, by = c("CO_NAME" = "county"))

countySF$permitsperCap <-  countySF$nPermits / countySF$population
countySF$permitsperSqMile <- countySF$nPermits / countySF$AREA_SQMI

municSF$AREA_SQMI <- municSF$SQ_FEET /27878400
municSF$permitsperCap <-  municSF$nPermits / municSF$population
municSF$permitsperSqMile <- municSF$nPermits / municSF$AREA_SQMI

municSF %>% filter(year == 2016) %>%
ggplot() +
  geom_sf(aes(fill = nPermits))

municSF %>% filter(year == 2016) %>%
  ggplot() +
  geom_sf(aes(fill = permitsperCap)) +
  scale_fill_viridis()

municSF %>% filter(year == 2012) %>%
  ggplot() +
  geom_sf(aes(fill = permitsperSqMile)) +
  scale_fill_viridis(limits=c(0,50)) +
  ggtitle("2012")

municSF %>% filter(year == 2014) %>%
  ggplot() +
  geom_sf(aes(fill = permitsperSqMile)) +
  scale_fill_viridis(limits=c(0,50)) +
  ggtitle("2014")

municSF %>% filter(year == 2016) %>%
  ggplot() +
  geom_sf(aes(fill = permitsperSqMile)) +
  scale_fill_viridis(limits=c(0,50)) +
  ggtitle("2016")