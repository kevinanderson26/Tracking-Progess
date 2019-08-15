#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse", "readxl", "magrittr")
pack(packages)
rm(pack, packages)

source("Data_Import.R")

#adjust pre-2013 NJ Transit values
for(i in 1:length(unlinkedTrips$year)){
  if(unlinkedTrips$agencyCode[i] == "2080" & unlinkedTrips$year[i] <= 2012){
    unlinkedTrips$unlinkedTrips[i] <- unlinkedTrips$unlinkedTrips[i] * 0.105}
  rm(i)}

#combine hybrid rail, light rail, and streetcar categories as Light Rail
#(NJ Transit River Line is only service coded as hybrid rail, SEPTA recoded trolleys as streetcars post-2011)
for(i in 1:length(unlinkedTrips$mode)){
  if(unlinkedTrips$mode[i] %in% c("YR", "LR", "SR")){
    unlinkedTrips$mode[i] <- "LR/YR/SR"
    unlinkedTrips$mode_name[i] <- "Light Rail"}
  rm(i)}

#combine demand response and vanpool categories (non-scheduled services)
for(i in 1:length(unlinkedTrips$mode)){
  if(unlinkedTrips$mode[i] %in% c("DR", "VP")){
    unlinkedTrips$mode[i] <- "DR/VP"
    unlinkedTrips$mode_name[i] <- "Non-Scheduled Services"}
  rm(i)}

#collapse service type groups
unlinkedTrips %<>% group_by(year, agencyCode, mode) %>%
  summarize(unlinkedTrips = sum(unlinkedTrips),
            agency_name = first(agency_name),
            mode_name = first(mode_name))

#join population data to tables
population <- read_csv("../Data/population estimate 2000-2017.csv") %>%
  group_by(year) %>%
  summarize(population = sum(population))

unlinkedTrips %<>% left_join(population, by = "year") %>%
  mutate(unlinkedTripsPerCap = unlinkedTrips/population)

rm(population)

#group by Mode and Agency
unlinkedMode <- unlinkedTrips %>% group_by(year, mode) %>% 
  summarize(unlinkedTrips = sum(unlinkedTrips), population = first(population), mode_name = first(mode_name)) %>%
  mutate(unlinkedTripsPerCap = unlinkedTrips/population)

unlinkedAgency<- unlinkedTrips %>% group_by(year, agencyCode) %>% 
  summarize(unlinkedTrips = sum(unlinkedTrips), population = first(population), agency_name = first(agency_name)) %>%
  mutate(unlinkedTripsPerCap = unlinkedTrips/population)

#calculate total trips and trips per capita for all modes/agencies

total_trips <- unlinkedTrips %>% group_by(year) %>% 
  summarize(unlinkedTrips = sum(unlinkedTrips), population = first(population)) %>%
  mutate(unlinkedTripsPerCap = unlinkedTrips/population)


