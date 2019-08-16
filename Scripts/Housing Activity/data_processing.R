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

#import raw data
source("Scripts/Housing Activity/import.R")

#calculate annual and cumulative ratios by planning area
denseCat <- c("Core City", "Developed Community")
unitsDense <- units %>% filter(planning_area %in% denseCat) %>%
  group_by(year) %>%
  summarize(unitsDense = sum(units)) %>%
  mutate(csumDense = cumsum(unitsDense))

units_total <- units %>% group_by(year) %>%
  summarize(unitsTotal = sum(units)) %>%
  mutate(csumTotal = cumsum(unitsTotal))

units_ratio <- left_join(unitsDense, units_total, by = "year") %>%
  mutate(ratio_annual = unitsDense / unitsTotal * 100,
         ratio_cumulative = csumDense / csumTotal * 100)

rm(denseCat, unitsDense, unitsTotal)

#calculate annual and cumulative units by planning area for multiple geographies

units_PA_County <- units %>% group_by(year, county, planning_area) %>%
  summarize(units = sum(units)) %>% ungroup() %>% 
  mutate(co_pa = paste(county, planning_area, sep ="- "))

missing_PA_County <- data.frame(year = 1980:2018, 'Bucks County- Core City' = 0, 
                                'Burlington County- Core City' = 0, 'Chester County- Core City' = 0,
                                'Gloucester County- Core City' = 0, 'Montgomery County- Core City' = 0,
                                'Delaware County- Rural Area' = 0, 'Philadelphia County- Growing Suburb' = 0, 
                                'Philadelphia County- Developed Community' = 0, 
                                'Philadelphia County- Rural Area' = 0) %>%
  gather("co_pa", "units", 2:10) %>%
  mutate(co_pa = str_replace_all(co_pa, c(".County.."= " County- ", ".City" = " City",
                                          ".Community" = " Community", ".Suburb" = " Suburb", ".Area" = " Area")),
         county = str_match(co_pa, "^[:alpha:]+\\s[:alpha:]+"),
         planning_area = str_match(co_pa, "[:alpha:]+\\s[:alpha:]+$"))
  
units_PA_County %<>% bind_rows(missing_PA_County) %>%
  group_by(co_pa) %>%
  mutate(csum_units = cumsum(units))

units_PA_Region <- units_PA_County %>% group_by(year, planning_area) %>%
  summarize(units = sum(units), 
            csum_units = sum(csum_units),
            co_pa = paste("DVRPC-", first(planning_area), sep = " "),
            county = "DVRPC")

units_PA <- bind_rows(units_PA_County, units_PA_Region)

rm(missing_PA_County, units_PA_County, units_PA_Region)



#calculate annual and cumulative unit mix by geography
unit_mix_fun <- function(geo){
  geo <- enquo(geo)
  units %>%
    mutate(geoType = paste(!!geo, type, sep = "- ")) %>%
    group_by(geoType, year) %>%
    summarize(units = sum(units), geo = first(!!geo), type = first(type)) %>%
    mutate(csum_units = cumsum(units))}

unitsTypeRegion <- unit_mix_fun("DVRPC")
unitsTypeSubRegion <- unit_mix_fun(subregion)
unitsTypeCounty <- unit_mix_fun(county)
unitsTypePA <- unit_mix_fun(planning_area)

unitsType <- unitsTypeRegion %>% 
  bind_rows(unitsTypeSubRegion) %>%
  bind_rows(unitsTypeCounty) %>%
  bind_rows(unitsTypePA)

rm(unit_mix_fun, unitsTypeRegion, unitsTypeSubRegion, unitsTypeCounty, unitsTypePA)