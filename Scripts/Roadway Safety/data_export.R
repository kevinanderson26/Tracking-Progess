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
source("data_processing.R")

#format regional table
ksiDVRPCSpread <- ksiRegion %>% 
  select(year = year,
         ksi5yrAvgDVRPC = ksi5yrAvg,
         ksiBikePedDVRPC = ksiBikePed,
         ksiMotorVehicleDVRPC = ksiMotorVehicle,
         ksiPerCap5yrAvgDVRPC = ksiPerPop5yrAvg,
         ksiBikePedPerCapDVRPC = ksiBikePedPerPop, 
         ksiMotorVehiclePerCapDVRPC = ksiMotorVehiclePerPop,
         ksiPerVMT5yrAvgDVRPC = ksiPerVMT5yrAvg,
         ksiPerVMTDVRPC = ksiPerVMT)

#spread subregion variables, rejoin into single table
ksiSubregionSpread1a <- ksiSubregion %>% 
  select (year, subregion, ksi5yrAvg) %>% 
  spread(subregion, ksi5yrAvg) %>% 
  select(year = year, 
         ksi5yrAvgNJSubregion = 'NJ Suburbs', 
         ksi5yrAvgPASubregion = 'PA Suburbs')

ksiSubregionSpread1b <- ksiSubregion %>% 
  select (year, subregion, ksiBikePed) %>% 
  spread(subregion, ksiBikePed) %>% 
  select(year = year, 
         ksiBikePedNJSubregion = 'NJ Suburbs', 
         ksiBikePedPASubregion = 'PA Suburbs')
  
ksiSubregionSpread1c <- ksiSubregion %>% 
  select (year, subregion, ksiMotorVehicle) %>% 
  spread(subregion, ksiMotorVehicle) %>% 
  select(year = year, 
         ksiMotorVehicleNJSubregion = 'NJ Suburbs', 
         ksiMotorVehiclePASubregion = 'PA Suburbs')
  
ksiSubregionSpread2a <- ksiSubregion %>% 
  select (year, subregion, ksiPerPop5yrAvg) %>% 
  spread(subregion, ksiPerPop5yrAvg) %>% 
  select(year = year, 
         ksiPerPop5yrAvgNJSubregion = 'NJ Suburbs', 
         ksiPerPop5yrAvgPASubregion = 'PA Suburbs')

ksiSubregionSpread2b <- ksiSubregion %>% 
  select (year, subregion, ksiBikePedPerPop) %>% 
  spread(subregion, ksiBikePedPerPop) %>% 
  select(year = year, 
         ksiBikePedPerPopNJSubregion = 'NJ Suburbs', 
         ksiBikePedPerPopPASubregion = 'PA Suburbs')

ksiSubregionSpread2c <- ksiSubregion %>% 
  select (year, subregion, ksiMotorVehiclePerPop) %>% 
  spread(subregion, ksiMotorVehiclePerPop) %>% 
  select(year = year, 
         ksiMotorVehiclePerPopNJSubregion = 'NJ Suburbs', 
         ksiMotorVehiclePerPopPASubregion = 'PA Suburbs')

ksiSubregionSpread3a <- ksiSubregion %>% 
  select (year, subregion, ksiPerVMT5yrAvg) %>% 
  spread(subregion, ksiPerVMT5yrAvg) %>% 
  select(year = year, 
         ksiPerVMT5yrAvgNJSubregion = 'NJ Suburbs', 
         ksiPerVMT5yrAvgPASubregion = 'PA Suburbs')

ksiSubregionSpread3b <- ksiSubregion %>% 
  select (year, subregion, ksiPerVMT) %>% 
  spread(subregion, ksiPerVMT) %>% 
  select(year = year, 
         ksiPerVMTNJSubregion = 'NJ Suburbs', 
         ksiPerVMTPASubregion = 'PA Suburbs')

ksiSubregionSpread <- ksiSubregionSpread1a %>% full_join(ksiSubregionSpread1b, by = "year") %>% 
  full_join(ksiSubregionSpread1c, by = "year") %>% full_join(ksiSubregionSpread2a, by = "year") %>%
  full_join(ksiSubregionSpread2b, by = "year") %>% full_join(ksiSubregionSpread2c, by = "year") %>% 
  full_join(ksiSubregionSpread3a, by = "year") %>% full_join(ksiSubregionSpread3b, by = "year")

rm(ksiSubregionSpread1a, ksiSubregionSpread1b, ksiSubregionSpread1c, ksiSubregionSpread2a, ksiSubregionSpread2b, ksiSubregionSpread2c, ksiSubregionSpread3a, ksiSubregionSpread3b)

#spread County variables, rejoin into single table
ksiCountySpread1a <- ksiCounty %>% 
  select (year, county, ksi5yrAvg) %>% 
  spread(county, ksi5yrAvg) %>% 
  select(year = year, 
         ksi5yrAvgBucksCo = Bucks, ksi5yrAvgChesterCo = Chester, 
         ksi5yrAvgDelawareCo = Delaware, 
         ksi5yrAvgMontgomeryCo = Montgomery, ksi5yrAvgPhillyCo = Philadelphia, 
         ksi5yrAvgBurlingtonCo = Burlington, ksi5yrAvgCamdenCo = Camden, 
         ksi5yrAvgGloucesterCo = Gloucester, ksi5yrAvgMercerCo = Mercer)

ksiCountySpread1b <- ksiCounty %>% 
  select (year, county, ksiBikePed) %>% 
  spread(county, ksiBikePed) %>% 
  select(year = year, 
         ksiBikePedBucksCo = Bucks, ksiBikePedChesterCo = Chester, 
         ksiBikePedDelawareCo = Delaware, 
         ksiBikePedMontgomeryCo = Montgomery, ksiBikePedPhillyCo = Philadelphia, 
         ksiBikePedBurlingtonCo = Burlington, ksiBikePedCamdenCo = Camden, 
         ksiBikePedGloucesterCo = Gloucester, ksiBikePedMercerCo = Mercer)

ksiCountySpread1c <- ksiCounty %>% 
  select (year, county, ksiMotorVehicle) %>% 
  spread(county, ksiMotorVehicle) %>% 
  select(year = year, 
         ksiMotorVehicleBucksCo = Bucks, ksiMotorVehicleChesterCo = Chester, 
         ksiMotorVehicleDelawareCo = Delaware, 
         ksiMotorVehicleMontgomeryCo = Montgomery, ksiMotorVehiclePhillyCo = Philadelphia, 
         ksiMotorVehicleBurlingtonCo = Burlington, ksiMotorVehicleCamdenCo = Camden, 
         ksiMotorVehicleGloucesterCo = Gloucester, ksiMotorVehicleMercerCo = Mercer)

ksiCountySpread2a <- ksiCounty %>% 
  select (year, county, ksiPerPop5yrAvg) %>% 
  spread(county, ksiPerPop5yrAvg) %>% 
  select(year = year, 
         ksiPerPop5yrAvgBucksCo = Bucks, ksiPerPop5yrAvgChesterCo = Chester, 
         ksiPerPop5yrAvgDelawareCo = Delaware, 
         ksiPerPop5yrAvgMontgomeryCo = Montgomery, ksiPerPop5yrAvgPhillyCo = Philadelphia, 
         ksiPerPop5yrAvgBurlingtonCo = Burlington, ksiPerPop5yrAvgCamdenCo = Camden, 
         ksiPerPop5yrAvgGloucesterCo = Gloucester, ksiPerPop5yrAvgMercerCo = Mercer)

ksiCountySpread2b <- ksiCounty %>% 
  select (year, county, ksiBikePedPerPop) %>% 
  spread(county, ksiBikePedPerPop) %>% 
  select(year = year, 
         ksiBikePedPerPopBucksCo = Bucks, ksiBikePedPerPopChesterCo = Chester, 
         ksiBikePedPerPopDelawareCo = Delaware, 
         ksiBikePedPerPopMontgomeryCo = Montgomery, ksiBikePedPerPopPhillyCo = Philadelphia, 
         ksiBikePedPerPopBurlingtonCo = Burlington, ksiBikePedPerPopCamdenCo = Camden, 
         ksiBikePedPerPopGloucesterCo = Gloucester, ksiBikePedPerPopMercerCo = Mercer)

ksiCountySpread2c <- ksiCounty %>% 
  select (year, county, ksiMotorVehiclePerPop) %>% 
  spread(county, ksiMotorVehiclePerPop) %>% 
  select(year = year, 
         ksiMotorVehiclePerPopBucksCo = Bucks, ksiMotorVehiclePerPopChesterCo = Chester, 
         ksiMotorVehicleDelawarePerPopCo = Delaware, 
         ksiMotorVehiclePerPopMontgomeryCo = Montgomery, ksiMotorVehiclePerPopPhillyCo = Philadelphia, 
         ksiMotorVehiclePerPopBurlingtonCo = Burlington, ksiMotorVehiclePerPopCamdenCo = Camden, 
         ksiMotorVehiclePerPopGloucesterCo = Gloucester, ksiMotorVehiclePerPopMercerCo = Mercer)

ksiCountySpread3a <- ksiCounty %>% 
  select (year, county, ksiPerVMT5yrAvg) %>% 
  spread(county, ksiPerVMT5yrAvg) %>% 
  select(year = year, 
         ksiPerVMT5yrAvgBucksCo = Bucks, ksiPerVMT5yrAvgChesterCo = Chester, 
         ksiPerVMT5yrAvgDelawareCo = Delaware, 
         ksiPerVMT5yrAvgMontgomeryCo = Montgomery, ksiPerVMT5yrAvgPhillyCo = Philadelphia, 
         ksiPerVMT5yrAvgBurlingtonCo = Burlington, ksiPerVMT5yrAvgCamdenCo = Camden, 
         ksiPerVMT5yrAvgGloucesterCo = Gloucester, ksiPerVMT5yrAvgMercerCo = Mercer)

ksiCountySpread3b <- ksiCounty %>% 
  select (year, county, ksiPerVMT) %>% 
  spread(county, ksiPerVMT) %>% 
  select(year = year, 
         ksiPerVMTBucksCo = Bucks, ksiPerVMTChesterCo = Chester, ksiPerVMTDelawareCo = Delaware, 
         ksiPerVMTMontgomeryCo = Montgomery, ksiPerVMTPhillyCo = Philadelphia, 
         ksiPerVMTBurlingtonCo = Burlington, ksiPerVMTCamdenCo = Camden, 
         ksiPerVMTGloucesterCo = Gloucester, ksiPerVMTMercerCo = Mercer)


ksiCountySpread <- ksiCountySpread1a %>% full_join(ksiCountySpread1b, by = "year") %>% 
  full_join(ksiCountySpread1c, by = "year") %>% full_join(ksiCountySpread2a, by = "year") %>%
  full_join(ksiCountySpread2b, by = "year") %>% full_join(ksiCountySpread2c, by = "year") %>% 
  full_join(ksiCountySpread3a, by = "year") %>% full_join(ksiCountySpread3b, by = "year")

rm(ksiCountySpread1a, ksiCountySpread1b, ksiCountySpread1c, ksiCountySpread2a, ksiCountySpread2b, ksiCountySpread2c, ksiCountySpread3a, ksiCountySpread3b)

#join spread tables
ksiSpread <- ksiDVRPCSpread %>% full_join(ksiSubregionSpread, by = "year") %>% full_join(ksiCountySpread, by = "year")
rm(ksiDVRPCSpread, ksiSubregionSpread, ksiCountySpread)

write_csv(ksiSpread, "../Processed Data/crashes Web.csv")
