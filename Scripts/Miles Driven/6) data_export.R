#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse", "readxl")
pack(packages)
rm(pack, packages)

#source processed data
source("5) data_processing.R")

#spread state variables, keep only Pennsylvania, rejoin into single table
vmtStatesSpread1 <- vmtStates %>% 
  select (year, state, milesAnnual) %>% 
  spread(state, milesAnnual) %>% 
  select(year = year, vmtAllPACounties = 'Pennsylvania')

vmtStatesSpread2 <- vmtStates %>% 
  select (year, state, milesPerCapita) %>% 
  spread(state, milesPerCapita) %>% 
  select(year = year, vmtPerCapAllPACounties = 'Pennsylvania')

vmtStatesSpread3 <- vmtStates %>% 
  select (-milesAnnual, -milesPerCapita) %>% 
  spread(state, milesPerVehicle) %>% 
  select(year = year, vmtPerVehicleAllPACounties = 'Pennsylvania')

vmtStatesSpread <- vmtStatesSpread1 %>% full_join(vmtStatesSpread2, by = "year") %>% 
  full_join(vmtStatesSpread3, by = "year")
rm(vmtStatesSpread1, vmtStatesSpread2, vmtStatesSpread3)

#spread subegion variables, rejoin into single table
vmtSuburbsSpread1 <- vmtSuburbs %>% 
  select (year, region, milesAnnual) %>% 
  spread(region, milesAnnual) %>% 
  select(year = year, vmtPASuburbs = 'Pennsylvania Suburbs',
         vmtNJSuburbs = 'New Jersey Suburbs')

vmtSuburbsSpread2 <- vmtSuburbs %>% 
  select (year, region, milesPerCapita) %>% 
  spread(region, milesPerCapita) %>% 
  select(year = year, vmtPerCapPASuburbs = 'Pennsylvania Suburbs', 
         vmtPerCapNJSuburbs = 'New Jersey Suburbs')

vmtSuburbsSpread3 <- vmtSuburbs %>% 
  select (-milesAnnual, -milesPerCapita) %>% 
  spread(region, milesPerVehicle) %>% 
  select(year = year, vmtPerVehiclePASuburbs = 'Pennsylvania Suburbs', 
         vmtPerVehicleNJSuburbs = 'New Jersey Suburbs')

vmtSuburbsSpread <- vmtSuburbsSpread1 %>% full_join(vmtSuburbsSpread2, by = "year") %>% 
  full_join(vmtSuburbsSpread3, by = "year")
rm(vmtSuburbsSpread1, vmtSuburbsSpread2, vmtSuburbsSpread3)

#spread County variables, rejoin into single table
vmtCountiesSpread1 <- vmtCounties %>% 
  select (year, county, milesAnnual) %>% 
  spread(county, milesAnnual) %>% 
  select(year = year, vmtBucksCo = Bucks, vmtChesterCo = Chester, vmtDelawareCo = Delaware, 
         vmtMontgomeryCo = Montgomery,vmtPhillyCo = Philadelphia, vmtBurlingtonCo = Burlington, 
         vmtCamdenCo = Camden, vmtGloucesterCo = Gloucester, vmtMercerCo = Mercer)

vmtCountiesSpread2 <- vmtCounties %>% 
  select (year, county, milesPerCapita) %>% 
  spread(county, milesPerCapita) %>% 
  select(year = year, vmtPerCapBucksCo = Bucks, vmtPerCapChesterCo = Chester, vmtPerCapDelawareCo = Delaware, 
         vmtPerCapMontgomeryCo = Montgomery,vmtPerCapPhillyCo = Philadelphia, 
         vmtPerCapBurlingtonCo = Burlington, vmtPerCapCamdenCo = Camden, 
         vmtPerCapGloucesterCo = Gloucester, vmtPerCapMercerCo = Mercer)

vmtCountiesSpread3 <- vmtCounties %>% 
  select (year, county, milesPerVehicle) %>% 
  spread(county, milesPerVehicle) %>% 
  select(year = year, vmtPerVehicleBucksCo = Bucks, vmtPerVehicleChesterCo = Chester, 
         vmtPerVehicleDelawareCo = Delaware, vmtPerVehicleMontgomeryCo = Montgomery,
         vmtPerVehiclePhillyCo = Philadelphia, vmtPerVehicleBurlingtonCo = Burlington, 
         vmtPerVehicleCamdenCo = Camden, vmtPerVehicleGloucesterCo = Gloucester, 
         vmtPerVehicleMercerCo = Mercer)

vmtCountiesSpread <- vmtCountiesSpread1 %>% full_join(vmtCountiesSpread2, by = "year") %>% 
  full_join(vmtCountiesSpread3, by = "year")
rm(vmtCountiesSpread1, vmtCountiesSpread2, vmtCountiesSpread3)

#format regional tablet to match spread tables
vmtDVRPCSpread <- vmtAllDVRPC %>% select(year, vmtDVRPC = milesAnnual, vmtPerCapDVRPC = milesPerCapita, vmtPerVehicleDVRPC = milesPerVehicle)

#join spread tables, round to two decimal places
vmtSpread <- vmtDVRPCSpread %>% 
  full_join(vmtStatesSpread, by = "year") %>%
  full_join(vmtSuburbsSpread, by = "year") %>%
  full_join(vmtCountiesSpread, by = "year")

rm(vmtDVRPCSpread, vmtSuburbsSpread, vmtCountiesSpread)
write_csv(vmtSpread, "../Processed Data/vmtWeb.csv")