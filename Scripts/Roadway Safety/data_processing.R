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

source("data_import.R")

# create KSI table
# 1) combine death and major injury totals to create KSI totals
# 2) select only needed variables
# 3) normalize KSI totals to with KSI per 100,000 population and per 1,000,000 VMT
ksi <- crashAll %>% mutate(ksi = deathTotal + majInjTotal,
                           ksiBikePed = deathBicycle + majInjBicycle + deathPed + majInjPed,
                           ksiMotorVehicle = ksi - ksiBikePed) %>% 
                    select(stateCode, countyCode, geoid, state, county, subregion,
                           year, ksi, ksiBikePed, ksiMotorVehicle, pop, milesAnnual) %>%
                    mutate(ksiPerPop = ksi/pop * 100000, 
                           ksiPerVMT = ksi/milesAnnual * 100000000,
                           ksiBikePedPerPop = ksiBikePed/pop * 100000,
                           ksiMotorVehiclePerPop = ksiMotorVehicle/pop * 100000,
                           ksiBikePedPerVMT = ksiBikePed/milesAnnual * 100000000,
                           ksiMotorVehiclePerVMT = ksiMotorVehicle/milesAnnual * 100000000)

# create KSI tables for aggregated geographies (Region, Subregion, County)
# 1) group by year and relevant geography variable)
# 2) summarize to sum KSI, pop, and VMT variables
# 3) normalize KSI totals to with KSI per 100,000 population and per 1,000,000 VMT

ksiRegion <- ksi %>% group_by(year) %>% 
                     summarize(ksi = sum(ksi),
                               ksiBikePed = sum(ksiBikePed),
                               ksiMotorVehicle = sum(ksiMotorVehicle),
                               pop = sum(pop),
                               milesAnnual = sum(milesAnnual),
                               ksiPerPop = ksi/pop * 100000, 
                               ksiPerVMT = ksi/milesAnnual * 100000000,
                               ksiBikePedPerPop = ksiBikePed/pop * 100000,
                               ksiMotorVehiclePerPop = ksiMotorVehicle/pop * 100000,
                               ksiBikePedPerVMT = ksiBikePed/milesAnnual * 100000000,
                               ksiMotorVehiclePerVMT = ksiMotorVehicle/milesAnnual * 100000000)

ksiSubregion <- ksi %>% group_by(subregion, year) %>% 
                        summarize(ksi = sum(ksi),
                               ksiBikePed = sum(ksiBikePed),
                               ksiMotorVehicle = sum(ksiMotorVehicle),
                               pop = sum(pop),
                               milesAnnual = sum(milesAnnual),
                               ksiPerPop = ksi/pop * 100000, 
                               ksiPerVMT = ksi/milesAnnual * 100000000,
                               ksiBikePedPerPop = ksiBikePed/pop * 100000,
                               ksiMotorVehiclePerPop = ksiMotorVehicle/pop * 100000,
                               ksiBikePedPerVMT = ksiBikePed/milesAnnual * 100000000,
                               ksiMotorVehiclePerVMT = ksiMotorVehicle/milesAnnual * 100000000)

ksiCounty <- ksi %>% group_by(county, year) %>% 
                     summarize(stateCode = first(stateCode), 
                               countyCode = first(countyCode), 
                               geoid = first(geoid), 
                               state = first(state), 
                               ksi = sum(ksi),
                               ksiBikePed = sum(ksiBikePed),
                               ksiMotorVehicle = sum(ksiMotorVehicle),
                               pop = sum(pop),
                               milesAnnual = sum(milesAnnual),
                               ksiPerPop = ksi/pop * 100000, 
                               ksiPerVMT = ksi/milesAnnual *100000000,
                               ksiBikePedPerPop = ksiBikePed/pop * 100000,
                               ksiMotorVehiclePerPop = ksiMotorVehicle/pop * 100000,
                               ksiBikePedPerVMT = ksiBikePed/milesAnnual * 100000000,
                               ksiMotorVehiclePerVMT = ksiMotorVehicle/milesAnnual * 100000000)

#add 5-year averages to Regional table for total KSI
ksiRegion$ksi5yrAvg <- NA
for(i in 1:length(ksiRegion$year)){
  ksiRegion$ksi5yrAvg[i] <- ifelse(ksiRegion$year[i] < min(ksiRegion$year) + 4, NA, (sum(ksiRegion$ksi[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Regional table for bike/ped KSI
ksiRegion$ksiBikePed5yrAvg <- NA
for(i in 1:length(ksiRegion$year)){
  ksiRegion$ksiBikePed5yrAvg[i] <- ifelse(ksiRegion$year[i] < min(ksiRegion$year) + 4, NA, (sum(ksiRegion$ksiBikePed[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Regional table for motor vehicle KSI
ksiRegion$ksiMotorVehicle5yrAvg <- NA
for(i in 1:length(ksiRegion$year)){
  ksiRegion$ksiMotorVehicle5yrAvg[i] <- ifelse(ksiRegion$year[i] < min(ksiRegion$year) + 4 , NA, (sum(ksiRegion$ksiMotorVehicle[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Regional table for total KSI per pop
ksiRegion$ksiPerPop5yrAvg <- NA
for(i in 1:length(ksiRegion$year)){
  ksiRegion$ksiPerPop5yrAvg[i] <- ifelse(ksiRegion$year[i] < min(ksiRegion$year) + 4, NA, (sum(ksiRegion$ksiPerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Regional table for bike/ped KSI per pop
ksiRegion$ksiBikePedPerPop5yrAvg <- NA
for(i in 1:length(ksiRegion$year)){
  ksiRegion$ksiBikePedPerPop5yrAvg[i] <- ifelse(ksiRegion$year[i] < min(ksiRegion$year) + 4, NA, (sum(ksiRegion$ksiBikePedPerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Regional table for motor vehicle KSI per pop
ksiRegion$ksiMotorVehiclePerPop5yrAvg <- NA
for(i in 1:length(ksiRegion$year)){
  ksiRegion$ksiMotorVehiclePerPop5yrAvg[i] <- ifelse(ksiRegion$year[i] < min(ksiRegion$year) + 4 , NA, (sum(ksiRegion$ksiMotorVehiclePerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Regional table for total KSI per vmt
ksiRegion$ksiPerVMT5yrAvg <- NA
for(i in 1:length(ksiRegion$year)){
  ksiRegion$ksiPerVMT5yrAvg[i] <- ifelse(ksiRegion$year[i] < min(ksiRegion$year) + 4, NA, (sum(ksiRegion$ksiPerVMT[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to County table for total KSI
ksiCounty$ksi5yrAvg <- NA
for(i in 1:length(ksiCounty$year)){
  ksiCounty$ksi5yrAvg[i] <- ifelse(ksiCounty$year[i] < min(ksiCounty$year) + 4, NA, (sum(ksiCounty$ksi[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to County table for bike/ped KSI
ksiCounty$ksiBikePed5yrAvg <- NA
for(i in 1:length(ksiCounty$year)){
  ksiCounty$ksiBikePed5yrAvg[i] <- ifelse(ksiCounty$year[i] < min(ksiCounty$year) + 4 , NA, (sum(ksiCounty$ksiBikePed[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to County table for motor vehicle KSI
ksiCounty$ksiMotorVehicle5yrAvg <- NA
for(i in 1:length(ksiCounty$year)){
  ksiCounty$ksiMotorVehicle5yrAvg[i] <- ifelse(ksiCounty$year[i] < min(ksiCounty$year) + 4 , NA, (sum(ksiCounty$ksiMotorVehicle[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to County table for total KSI per pop
ksiCounty$ksiPerPop5yrAvg <- NA
for(i in 1:length(ksiCounty$year)){
  ksiCounty$ksiPerPop5yrAvg[i] <- ifelse(ksiCounty$year[i] < min(ksiCounty$year) + 4 , NA, (sum(ksiCounty$ksiPerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to County table for bike/ped KSI per pop
ksiCounty$ksiBikePedPerPop5yrAvg <- NA
for(i in 1:length(ksiCounty$year)){
  ksiCounty$ksiBikePedPerPop5yrAvg[i] <- ifelse(ksiCounty$year[i] < min(ksiCounty$year) + 4, NA, (sum(ksiCounty$ksiBikePedPerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to County table for motor vehicle KSI per pop
ksiCounty$ksiMotorVehiclePerPop5yrAvg <- NA
for(i in 1:length(ksiCounty$year)){
  ksiCounty$ksiMotorVehiclePerPop5yrAvg[i] <- ifelse(ksiCounty$year[i] < min(ksiCounty$year) + 4, NA, (sum(ksiCounty$ksiMotorVehiclePerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to County table for total KSI per vmt
ksiCounty$ksiPerVMT5yrAvg <- NA
for(i in 1:length(ksiCounty$year)){
  ksiCounty$ksiPerVMT5yrAvg[i] <- ifelse(ksiCounty$year[i] < min(ksiCounty$year) + 4, NA, (sum(ksiCounty$ksiPerVMT[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Subregion table for total KSI
ksiSubregion$ksi5yrAvg <- NA
for(i in 1:length(ksiSubregion$year)){
  ksiSubregion$ksi5yrAvg[i] <- ifelse(ksiSubregion$year[i] < min(ksiSubregion$year) + 4, NA, (sum(ksiSubregion$ksi[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Subregion table for bike/ped KSI
ksiSubregion$ksiBikePed5yrAvg <- NA
for(i in 1:length(ksiSubregion$year)){
  ksiSubregion$ksiBikePed5yrAvg[i] <- ifelse(ksiSubregion$year[i] < min(ksiSubregion$year) + 4, NA, (sum(ksiSubregion$ksiBikePed[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Subregion table for motor vehicle KSI
ksiSubregion$ksiMotorVehicle5yrAvg <- NA
for(i in 1:length(ksiSubregion$year)){
  ksiSubregion$ksiMotorVehicle5yrAvg[i] <- ifelse(ksiSubregion$year[i] < min(ksiSubregion$year) + 4 + 4, NA, (sum(ksiSubregion$ksiMotorVehicle[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Subregion table for total KSI per pop
ksiSubregion$ksiPerPop5yrAvg <- NA
for(i in 1:length(ksiSubregion$year)){
  ksiSubregion$ksiPerPop5yrAvg[i] <- ifelse(ksiSubregion$year[i] < min(ksiSubregion$year) + 4, NA, (sum(ksiSubregion$ksiPerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Subregion table for bike/ped KSI per pop
ksiSubregion$ksiBikePedPerPop5yrAvg <- NA
for(i in 1:length(ksiSubregion$year)){
  ksiSubregion$ksiBikePedPerPop5yrAvg[i] <- ifelse(ksiSubregion$year[i] < min(ksiSubregion$year) + 4, NA, (sum(ksiSubregion$ksiBikePedPerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Subregion table for motor vehicle KSI per pop
ksiSubregion$ksiMotorVehiclePerPop5yrAvg <- NA
for(i in 1:length(ksiSubregion$year)){
  ksiSubregion$ksiMotorVehiclePerPop5yrAvg[i] <- ifelse(ksiSubregion$year[i] < min(ksiSubregion$year) + 4, NA, (sum(ksiSubregion$ksiMotorVehiclePerPop[(i-4):(i)])/5))
  rm(i)}

#add 5-year averages to Subregion table for total KSI per vmt
ksiSubregion$ksiPerVMT5yrAvg <- NA
for(i in 1:length(ksiSubregion$year)){
  ksiSubregion$ksiPerVMT5yrAvg[i] <- ifelse(ksiSubregion$year[i] < min(ksiSubregion$year) + 4, NA, (sum(ksiSubregion$ksiPerVMT[(i-4):(i)])/5))
  rm(i)}

#round all averages and per capita and per vmt rates to nearest hundreth
ksiRegion %<>%
  mutate(ksiPerPop = round(ksiPerPop, 2),
         ksiPerVMT = round(ksiPerVMT, 2),
         ksiBikePedPerPop = round(ksiBikePedPerPop, 2),
         ksiMotorVehiclePerPop = round(ksiMotorVehiclePerPop, 2),
         ksiBikePedPerVMT = round(ksiBikePedPerVMT, 2),
         ksiMotorVehiclePerVMT = round(ksiMotorVehiclePerVMT, 2),
         ksi5yrAvg = round(ksi5yrAvg, 2),
         ksiBikePed5yrAvg = round(ksiBikePed5yrAvg, 2),
         ksiMotorVehicle5yrAvg = round(ksiMotorVehicle5yrAvg, 2),
         ksiPerPop5yrAvg = round(ksiPerPop5yrAvg, 2),
         ksiBikePedPerPop5yrAvg = round(ksiBikePedPerPop5yrAvg, 2),
         ksiMotorVehiclePerPop5yrAvg = round(ksiMotorVehiclePerPop5yrAvg, 2),
         ksiPerVMT5yrAvg = round(ksiPerVMT5yrAvg, 2))

ksiSubregion %<>%
  mutate(ksiPerPop = round(ksiPerPop, 2),
         ksiPerVMT = round(ksiPerVMT, 2),
         ksiBikePedPerPop = round(ksiBikePedPerPop, 2),
         ksiMotorVehiclePerPop = round(ksiMotorVehiclePerPop, 2),
         ksiBikePedPerVMT = round(ksiBikePedPerVMT, 2),
         ksiMotorVehiclePerVMT = round(ksiMotorVehiclePerVMT, 2),
         ksi5yrAvg = round(ksi5yrAvg, 2),
         ksiBikePed5yrAvg = round(ksiBikePed5yrAvg, 2),
         ksiMotorVehicle5yrAvg = round(ksiMotorVehicle5yrAvg, 2),
         ksiPerPop5yrAvg = round(ksiPerPop5yrAvg, 2),
         ksiBikePedPerPop5yrAvg = round(ksiBikePedPerPop5yrAvg, 2),
         ksiMotorVehiclePerPop5yrAvg = round(ksiMotorVehiclePerPop5yrAvg, 2),
         ksiPerVMT5yrAvg = round(ksiPerVMT5yrAvg, 2))

ksiCounty %<>%
  mutate(ksiPerPop = round(ksiPerPop, 2),
         ksiPerVMT = round(ksiPerVMT, 2),
         ksiBikePedPerPop = round(ksiBikePedPerPop, 2),
         ksiMotorVehiclePerPop = round(ksiMotorVehiclePerPop, 2),
         ksiBikePedPerVMT = round(ksiBikePedPerVMT, 2),
         ksiMotorVehiclePerVMT = round(ksiMotorVehiclePerVMT, 2),
         ksi5yrAvg = round(ksi5yrAvg, 2),
         ksiBikePed5yrAvg = round(ksiBikePed5yrAvg, 2),
         ksiMotorVehicle5yrAvg = round(ksiMotorVehicle5yrAvg, 2),
         ksiPerPop5yrAvg = round(ksiPerPop5yrAvg, 2),
         ksiBikePedPerPop5yrAvg = round(ksiBikePedPerPop5yrAvg, 2),
         ksiMotorVehiclePerPop5yrAvg = round(ksiMotorVehiclePerPop5yrAvg, 2),
         ksiPerVMT5yrAvg = round(ksiPerVMT5yrAvg, 2))