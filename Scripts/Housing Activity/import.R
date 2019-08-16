#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl")
pack(packages)
rm(pack, packages)

#create empty data frame to bind inputs to
permits <- data.frame(state_code = character(), place_id = character(), 
                      county_code = character(), place_name = character(), 
                      bldgs_1 = integer(), units_1 = integer(), 
                      bldgs_2 = integer(), units_2 = integer(), 
                      bldgs_34 = integer(), units_34 = integer(), 
                      bldgs_5p = integer(), units_5p = integer(), 
                      year = integer())

#input raw data, filter only for DVRPC counties
for(i in c(1980:2018)){
  file_name <- paste("Raw Data/Housing Activity/yearly building permits/ne", i, "a.txt", sep = "")

  if(i <= 1998){
    temp <- read_csv(file_name, skip = 2, col_names = FALSE) %>% 
      select(state_code = X2,  place_id = X3,  county_code = X4,  place_name = X11,
             bldgs_1 = X12, units_1 = X13, bldgs_2 = X15, units_2 = X16,
             bldgs_34 = X18, units_34 = X19, bldgs_5p = X21, units_5p = X22)}

  if(i >= 1999 & i <= 2006){
    temp <- read_csv(file_name, skip = 3, col_names = FALSE) %>% 
      select(state_code = X2,  place_id = X3, county_code = X4, place_name = X14,
             bldgs_1 = X15, units_1 = X16, bldgs_2 = X18,  units_2 = X19,
             bldgs_34 = X21,  units_34 = X22, bldgs_5p = X24,  units_5p = X25)}

  if(i >= 2007){
    temp <- read_csv(file_name, skip = 3, col_names = FALSE) %>% 
      select(state_code = X2,  place_id = X3, county_code = X4, place_name = X17,
             bldgs_1 = X18, units_1 = X19, bldgs_2 = X21,  units_2 = X22,
             bldgs_34 = X24,  units_34 = X25, bldgs_5p = X27,  units_5p = X28)}

  temp$stco <-paste(temp$state_code, temp$county_code, sep = "")
  
  temp %<>% 
    filter(stco %in% c("42017", "42029", "42045", "42091", "42101", "34005", "34007", "34015", "34021")) %>% 
    select(-stco)

  temp$year <- i
  permits <- bind_rows(permits, temp)
  rm(i, file_name, temp)}

#clean up place names
permits$place_name %<>% str_replace_all("\\.", "")
permits$place_name %<>% str_replace_all("\\#", "")
permits$place_name %<>% str_replace_all("\\(N\\)", "")
permits$place_name %<>% str_to_title()
permits$place_name %<>% str_trim()

#create 11-digit ID column
permits$full_ID <- paste(permits$state_code, permits$county_code, permits$place_id, sep = "")
permits %<>% select(1:3, full_ID, everything())

#combine duplex and 3-4 unit buildings into small multi-family category
permits$sm_multi_bldgs <- permits$bldgs_2 + permits$bldgs_34
permits$sm_multi_units <- permits$units_2 + permits$units_34

#rename columns to building type and arrange columns
permits %<>% 
  rename(sf_bldgs = bldgs_1 , sf_units = units_1,
         lg_multi_bldgs = bldgs_5p, lg_multi_units = units_5p) %>%
  select(-bldgs_2, -bldgs_34, -units_2, -units_34) %>%
  select(year, state_code, county_code, place_id, full_ID, place_name,
         sf_bldgs, sf_units, sm_multi_bldgs, sm_multi_units, lg_multi_bldgs, lg_multi_units)

#correct Burlington/Evesham conflict by changing Burlington ID
for(i in 1:nrow(permits)){
  if(permits$place_name[i] == "Burlington"){permits$full_ID[i] <- "34005028500"}
  rm(i)}

#join FIPS IDs
geoid_lookup <- read_excel("Raw Data/Housing Activity/GEOID lookup.xlsx") %>% 
  select(full_ID, GEOID)

geoid_lookup$full_ID <- as.character(geoid_lookup$full_ID)

permits %<>% full_join(geoid_lookup, by = "full_ID")

rm(geoid_lookup)

#join Planning Areas
planning_areas <- read_excel("Raw Data/Housing Activity/2045 Planning Areas.xlsx") %>% 
  select(GEOID, planning_area = PA_2045)

permits %<>% left_join(planning_areas, by = "GEOID")

rm(planning_areas)

#reduce df to only units, and gather columns
units <- 
  permits %>% 
  select(year, place_name, county_code, geoid = GEOID, planning_area, 
         sf_units, sm_multi_units, lg_multi_units) %>%
  gather("type", "units", 6:8)

rm(permits)

#add county name field
units$county <- units$county_code %>%
  str_replace_all(c("005" = "Burlington County",
                    "007" = "Camden County",
                    "015" = "Gloucester County",
                    "017" = "Bucks County",
                    "021" = "Mercer County",
                    "029" = "Chester County",
                    "045" = "Delaware County",
                    "091" = "Montgomery County",
                    "101" = "Philadelphia County"))

#add subregion field
units$subregion <- ifelse(str_detect(units$geoid, "^34"), "New Jersey Suburbs",
                          ifelse(str_detect(units$geoid, "^42101"), "Philadelphia Subregion", 
                                 "Pennsylvania Suburbs"))

#rename and factor unit types
units$type %<>% 
  str_replace_all(c("sf_units" = "single family",
                    "sm_multi_units" = "small multi-family",
                    "lg_multi_units" = "large multi-family")) %>%
  factor(levels = c("single family", "small multi-family", "large multi-family"))

#add combined type and planning area label
units %<>% mutate(type_PA = paste(planning_area, type, sep = "- "))

units$type_PA %<>% 
  factor(levels = c("Core City- large multi-family",
                    "Core City- small multi-family",
                    "Core City- single family",
                    "Developed Community- large multi-family",
                    "Developed Community- small multi-family",
                    "Developed Community- single family",
                    "Growing Suburb- large multi-family",
                    "Growing Suburb- small multi-family",
                    "Growing Suburb- single family",
                    "Rural Area- large multi-family",
                    "Rural Area- small multi-family",
                    "Rural Area- single family"))