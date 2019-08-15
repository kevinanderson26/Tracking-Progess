#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr")
pack(packages)
rm(pack, packages)

#source input files
source("4) merge_tables.R")

#add region code
all_vmt$region <- ifelse(all_vmt$countyCode == 101, "Philadelphia",
                         ifelse(all_vmt$stateCode == 34, "New Jersey Suburbs", 
                                "Pennsylvania Suburbs"))
  
# create regional, count, and DVRPC tables.
# 1) summarize by year and relevant geography
# 2) convert VMT to billions of miles, 
# 3) round total VMT to two decimal places; round VMT per capita and per vehicle to whole numbers.
vmtAllDVRPC <- all_vmt %>% group_by(year) %>% 
  summarize(milesAnnual = sum(milesAnnual), 
            pop = sum(pop), 
            vehicles = sum(vehiclesTotal),
            milesPerCapita = milesAnnual/pop,
            milesPerVehicle = milesAnnual/vehicles,
            region = "All DVRPC") %>%
  select(-pop, -vehicles)  %>%
  mutate(milesAnnual = milesAnnual / 1000000000,
         milesAnnual = round(milesAnnual, 2),
         milesPerCapita = round(milesPerCapita, 0),
         milesPerVehicle = round(milesPerVehicle, 0))

vmtStates <- all_vmt %>% group_by(state, year) %>% 
  summarize(milesAnnual = sum(milesAnnual), 
            pop = sum(pop), 
            vehicles = sum(vehiclesTotal),
            milesPerCapita = milesAnnual/pop,
            milesPerVehicle = milesAnnual/vehicles) %>%
  select(-pop, -vehicles) %>%
  mutate(milesAnnual = milesAnnual / 1000000000,
         milesAnnual = round(milesAnnual, 2),
         milesPerCapita = round(milesPerCapita, 0),
         milesPerVehicle = round(milesPerVehicle, 0))

vmtSuburbs <- all_vmt %>% group_by(region, year) %>% 
  summarize(milesAnnual = sum(milesAnnual), 
            pop = sum(pop), 
            vehicles = sum(vehiclesTotal),
            milesPerCapita = milesAnnual/pop,
            milesPerVehicle = milesAnnual/vehicles) %>%
  select(-pop, -vehicles) %>%
  mutate(milesAnnual = milesAnnual / 1000000000,
         milesAnnual = round(milesAnnual, 2),
         milesPerCapita = round(milesPerCapita, 0),
         milesPerVehicle = round(milesPerVehicle, 0))

vmtCounties <- all_vmt %>% group_by(county, year) %>% 
  summarize(milesAnnual = sum(milesAnnual), 
            pop = sum(pop), 
            vehicles = sum(vehiclesTotal),
            milesPerCapita = milesAnnual/pop,
            milesPerVehicle = milesAnnual/vehicles) %>%
  select(-pop, -vehicles)  %>%
  mutate(milesAnnual = milesAnnual / 1000000000,
         milesAnnual = round(milesAnnual, 2),
         milesPerCapita = round(milesPerCapita, 0),
         milesPerVehicle = round(milesPerVehicle, 0))
