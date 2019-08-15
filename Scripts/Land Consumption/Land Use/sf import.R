#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "readxl", "sf")
pack(packages)
rm(pack, packages)

#load yearly files for 1990, 2000, 2005, 2010, 2015

landuse_2015_sf <- 
  st_read("../Data/Shapefiles/DVRPC_LandUse_2015/2015_Land_use.shp") %>%
  select(lu_code = LU_TYPE, lu_name = LU_LABEL) %>%
  mutate(year = 2015,
         lu_code = as.character(lu_code),
         lu_name = as.character(lu_name))

landuse_2010_sf <- 
  st_read("../Data/Shapefiles/DVRPC_LandUse_2010/2010_Land_use.shp") %>%
  select(lu_code = LU_TYPE, lu_name = LU_LABEL) %>%
  mutate(year = 2010,
         lu_code = as.character(lu_code),
         lu_name = as.character(lu_name))

landuse_2005_sf <- 
  st_read("../Data/Shapefiles/DVRPC_LandUse_2005/2005_Land_use.shp") %>%
  select(lu_code = LU_TYPE, lu_name = LU_LABEL) %>%
  mutate(year = 2005,
         lu_code = as.character(lu_code),
         lu_name = as.character(lu_name))

landuse_2000_sf <- 
  st_read("../Data/Shapefiles/DVRPC_LandUse_2000/2000_Land_use.shp") %>%
  select(lu_code = LANDUSE00, lu_name = DESCRIPTIO) %>%
  mutate(year = 2000,
         lu_code = as.character(lu_code),
         lu_name = as.character(lu_name))

landuse_1990_sf <- 
  st_read("../Data/Shapefiles/DVRPC_LandUse_1990.gdb", 
          layer = "ALL_LU_90") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO) %>%
  mutate(year = 1990,
         lu_code = as.character(lu_code),
         lu_name = as.character(lu_name)) %>%
  rename(geometry = Shape)

#load individual county files for 1995 and merge

landuse_1995_bucks_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Bucks") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO)
landuse_1995_chester_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Chester") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO)
landuse_1995_delaware_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Delaware") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO)
landuse_1995_montgomery_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Montgomery") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO)
landuse_1995_philadelphia_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Philadelphia") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO)
landuse_1995_burlington_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Burlington") %>%
  select(lu_code = LANDUSE, lu_name = CATEGORY)
landuse_1995_camden_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Camden") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO)
landuse_1995_gloucester_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Gloucester") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO)
landuse_1995_mercer_sf <- st_read("../Data/Shapefiles/DVRPC_LandUse_1995.gdb", layer = "Mercer") %>%
  select(lu_code = LANDUSE, lu_name = DESCRIPTIO)

landuse_1995_sf <- 
  rbind(landuse_1995_bucks_sf, landuse_1995_chester_sf, landuse_1995_delaware_sf, 
        landuse_1995_montgomery_sf, landuse_1995_philadelphia_sf, landuse_1995_burlington_sf,
        landuse_1995_camden_sf, landuse_1995_gloucester_sf, landuse_1995_mercer_sf) %>%
  rename(geometry = Shape) %>%
  mutate(year = 1995,
         lu_code = as.character(lu_code),
         lu_name = as.character(lu_name)) 

rm(landuse_1995_bucks_sf, landuse_1995_chester_sf, landuse_1995_delaware_sf, 
   landuse_1995_montgomery_sf, landuse_1995_philadelphia_sf, landuse_1995_burlington_sf,
   landuse_1995_camden_sf, landuse_1995_gloucester_sf, landuse_1995_mercer_sf)

# recode 1990 and 1995 land use codes to match 2000 and later
# 1) pad all 1990/1995 codes to 5 digits long
# 2) recode parking codes to correspond to land use types

landuse_1990_sf$lu_code <- str_pad(landuse_1990_sf$lu_code, 5, pad = "0", side = "right")
landuse_1995_sf$lu_code <- str_pad(landuse_1995_sf$lu_code, 5, pad = "0", side = "right")

landuse_1990_sf$lu_code %<>%
  mutate(lu_code = case_when(lu_code == "04020" ~ "02009",
                             lu_code == "04030" ~ "03009",
                             lu_code == "04040" ~ "04009",
                             lu_code == "04050" ~ "05009",
                             lu_code == "04060" ~ "06009",
                             lu_code == "04070" ~ "07009",
                             lu_code == "04080" ~ "08009",
                             lu_code == "04090" ~ "09009",
                             lu_code == "04100" ~ "10009",
                             lu_code == "04110" ~ "11009",
                             TRUE ~ lu_code))

landuse_1995_sf$lu_code %<>%
  mutate(lu_code = case_when(lu_code == "04020" ~ "02009",
                             lu_code == "04030" ~ "03009",
                             lu_code == "04040" ~ "04009",
                             lu_code == "04050" ~ "05009",
                             lu_code == "04060" ~ "06009",
                             lu_code == "04070" ~ "07009",
                             lu_code == "04080" ~ "08009",
                             lu_code == "04090" ~ "09009",
                             lu_code == "04100" ~ "10009",
                             lu_code == "04110" ~ "11009",
                             TRUE ~ lu_code))

#bind all land use year files
landuse <- rbind(landuse_1990_sf, landuse_1995_sf, landuse_2000_sf, 
                 landuse_2005_sf, landuse_2010_sf, landuse_2015_sf)

rm(landuse_1990_sf, landuse_1995_sf, landuse_2000_sf, 
   landuse_2005_sf, landuse_2010_sf, landuse_2015_sf)

#make land use labels consistent across years
landuse %<>% mutate(lu_name = as.character(lu_name),
                    lu_name = str_replace_all(lu_name, ":(?![:blank:])", ": "), #adds missing space after parking
                    lu_name = str_replace_all(lu_name, "Parking: Light  Industrial", "Parking: Light Industrial"),
                    lu_name = str_replace_all(lu_name, "Agriculture: Agricultural Bog", "Agriculture: Bog"),
                    lu_name = str_replace_all(lu_name, "Parking: Light Manufacturing", "Parking: Light Industrial"),
                    lu_name = str_replace_all(lu_name, "Parking: Heavy Manufacturing", "Parking: Heavy Industrial"),
                    lu_name = str_replace_all(lu_name, "Parking: Multi-Fami$", "Parking: Multi-Family"),
                    lu_name = str_replace_all(lu_name, "Utility: Parking", "Parking: Utility"))

#import planning area/municipal boundary shapefile
planning_areas <- 
  st_read(dsn = "../Data/Shapefiles/DVRPC_Connections_2045_Planning_Areas/DVRPC_Connections_2045_Planning_Areas.shp") %>%
  select(municipality = MUN_NAME, county = CO_NAME, geo_id = GEOID, planning_area = PA_2045)


#import population estimates and add county and subregion fields field
pop_est <- read_csv("../Data/population estimates.csv") %>%
  mutate(county = case_when(stCode == 34 & coCode == "005" ~ "Burlington",
                            stCode == 34 & coCode == "007" ~ "Camden",
                            stCode == 34 & coCode == "015" ~ "Mercer",
                            stCode == 34 & coCode == "021" ~ "Gloucester",
                            stCode == 42 & coCode == "017" ~ "Bucks",
                            stCode == 42 & coCode == "029" ~ "Chester",
                            stCode == 42 & coCode == "045" ~ "Delaware",
                            stCode == 42 & coCode == "091" ~ "Montgomery",
                            stCode == 42 & coCode == "101" ~ "Philadelphia"),
         subregion = case_when(stCode == 34 ~ "New Jersey Suburbs",
                               stCode == 42 & coCode != "101" ~ "Pennsylvania Suburbs",
                               TRUE ~ "Philadelphia")) %>%
  select(municipality = geography, county, planningArea, subregion, year, population) %>%
  filter(year %in% c(1990, 2000, 2005, 2010, 2015))

#create population data frames for various geographies
pop_region <- pop_est %>% group_by(year) %>% summarize(pop = sum(population))
pop_subregion <- pop_est %>% group_by(year, subregion) %>% summarize(pop = sum(population))
pop_pa <- pop_est %>% group_by(year, planningArea) %>% summarize(pop = sum(population)) %>%
  rename(planning_area = planningArea)
pop_county <- pop_est %>% group_by(year, county) %>% summarize(pop = sum(population))

#import 1995 population data and add to population data frames
pop_1995 <- read_csv("../Data/1995 Population.csv")

pop_1995_region <- pop_1995 %>% filter(Geo_Type == "Region") %>%
  mutate(year = 1995) %>%
  select(year, pop = '1995')

pop_region %<>% bind_rows(pop_1995_region)

pop_1995_subregion <- pop_1995 %>% filter(Geo_Type == "Subregion") %>%
  mutate(year = 1995) %>%
  select(year, subregion = Geography, pop = '1995')

pop_subregion %<>% bind_rows(pop_1995_subregion)

pop_1995_pa <- pop_1995 %>% filter(Geo_Type == "Planning Area") %>%
  mutate(year = 1995) %>%
  select(year, planning_area = Geography, pop = '1995')

pop_pa %<>% bind_rows(pop_1995_pa)

pop_1995_county <- pop_1995 %>% filter(Geo_Type == "County") %>%
  mutate(year = 1995) %>%
  select(year, county = Geography, pop = '1995')

pop_county %<>% bind_rows(pop_1995_county)
