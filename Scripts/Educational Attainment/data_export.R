#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("readr", "tidyr", "lubridate", "ggplot2", "tidyverse", "readxl", "sf", "stringr")
pack(packages)
rm(pack, packages)

source("high_school_ratios.R")

#high school rate tables
hsRateSpreadDVRPC <- highSchoolRatioCounty %>% filter(!geography == "United States") %>% 
                      group_by(year) %>%
                      summarize(highSchoolPlus = sum(highSchoolPlus), 
                                highSchoolLess = sum(highSchoolLess), 
                                hsRateDVRPC = highSchoolPlus / (highSchoolPlus + highSchoolLess)) %>%
                      select(year, hsRateDVRPC)

hsRateSpreadSubregion <- highSchoolRatioCounty %>% filter(!geography == "United States") %>% 
  group_by(year, subregion) %>%
  summarize(highSchoolPlus = sum(highSchoolPlus), 
            highSchoolLess = sum(highSchoolLess), 
            hsRate = highSchoolPlus / (highSchoolPlus + highSchoolLess)) %>%
  select(year, subregion, hsRate) %>%
  spread(subregion, hsRate) %>%
  rename(hsRateNJSuburbs = 'New Jersey Suburbs', 
         hsRatePASuburbs = 'Pennsylvania Suburbs', 
         hsRatePhillySubregion = 'Philadelphia Subregion')
  
hsRateSpreadCounty <- highSchoolRatioCounty %>% filter(!geography == "United States") %>% 
  group_by(year, geography) %>%
  summarize(highSchoolPlus = sum(highSchoolPlus), 
            highSchoolLess = sum(highSchoolLess), 
            hsRate = highSchoolPlus / (highSchoolPlus + highSchoolLess)) %>%
  select(year, geography, hsRate) %>%
  spread(geography, hsRate) %>%
  rename(hsRateBucksCo = 'Bucks',
         hsRateChesterCo = 'Chester',
         hsRateDelawareCo = 'Delaware',
         hsRateMontgomeryCo = 'Montgomery',
         hsRatePhiladelphiaCo = 'Philadelphia',
         hsRateBurlingtonCo = 'Burlington',
         hsRateCamdenCo = 'Camden',
         hsRateGloucesterCo = 'Gloucester',
         hsRateMercerCo = 'Mercer')

hsRateSpreadPlanningArea <- highSchoolRatioMunic %>% filter(!geography == "United States") %>%
  filter(year %in% c(2000, 2012, 2017)) %>% 
  group_by(year, planningArea) %>%
  summarize(highSchoolPlus = sum(highSchoolPlus), 
            highSchoolLess = sum(highSchoolLess), 
            hsRate = highSchoolPlus / (highSchoolPlus + highSchoolLess)) %>%
  select(year, planningArea, hsRate) %>%
  spread(planningArea, hsRate) %>%
  rename(hsRateCore = 'Core City',
         hsRateDeveloped = 'Developed Community',
         hsRateGrowing = 'Growing Suburb',
         hsRateRural = 'Rural Area')

hsRateSpread <- hsRateSpreadDVRPC %>% full_join(hsRateSpreadSubregion, by = "year") %>% 
  full_join(hsRateSpreadCounty, by = "year") %>% 
  full_join(hsRateSpreadPlanningArea, by = "year")

rm(hsRateSpreadDVRPC, hsRateSpreadSubregion, hsRateSpreadCounty, hsRateSpreadPlanningArea)

#round high school rate table to two digits, export to csv
hsRateSpread <- round(hsRateSpread, 3)
write_csv(hsRateSpread, "../Processed Data/edattain web 1.csv")

#ed category table

scaling_factor <- 1000
rounding_factor <- 1

edCatSpreadDVRPC <- edattain1yr %>% filter(! geography == "United States") %>% 
  group_by(year, edCategory) %>%
  summarize(n = sum(n)) %>% 
  mutate(n = n / scaling_factor,
         n = round(n, rounding_factor), 
         geoEd = paste("DVRPC", edCategory, sep = "-")) %>%
  select(-edCategory) %>%
  spread(geoEd, n)

edCatSpreadCounty <- edattain1yr %>% filter(! geography == "United States") %>%
  mutate(n = n / scaling_factor,
         n = round(n, rounding_factor),
         geoEd = paste(geography, edCategory, sep = "-")) %>%
  select(-edCategory, -geography, -geoid, -subregion) %>%
  spread(geoEd, n)

edCatSpreadSubregion <- edattain1yr %>% filter(!geography == "United States") %>% 
  group_by(year, subregion, edCategory) %>%
  summarize(n = sum(n)) %>% 
  mutate(n = n / scaling_factor,
         n = round(n, rounding_factor),
         geoEd = paste(subregion, edCategory, sep = "-")) %>%
  ungroup() %>%
  select(-subregion, -edCategory) %>%
  spread(geoEd, n)

edCatSpreadPlanningArea <- edattain5yr %>% filter(!geography == "United States") %>%
  filter(year %in% c(2000, 2012, 2017)) %>% 
  group_by(year, planningArea, edCategory) %>%
  summarize(n = sum(n)) %>% 
  mutate(n = n / scaling_factor,
         n = round(n, rounding_factor),
         geoEd = paste(planningArea, edCategory, sep = "-")) %>%
  ungroup() %>%
  select(-planningArea, -edCategory) %>%
  spread(geoEd, n)

edCatSpread <- edCatSpreadDVRPC %>% full_join(edCatSpreadSubregion, by = "year") %>% 
  full_join(edCatSpreadCounty, by = "year") %>% 
  full_join(edCatSpreadPlanningArea, by = "year")

rm(edCatSpreadDVRPC, edCatSpreadSubregion, edCatSpreadCounty, edCatSpreadPlanningArea)

#export to csv
write_csv(edCatSpread, "../Processed Data/edattain web 2.csv")