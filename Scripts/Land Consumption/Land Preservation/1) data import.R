#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "sf", "lwgeom")
pack(packages)
rm(pack, packages)
options(scipen=999)

#import planning area and county boundary layers

planning_areas <- st_read(dsn = "../Data/DVRPC_Connections_2045_Planning_Areas/DVRPC_Connections_2045_Planning_Areas.shp") %>% 
  select(municipality = MUN_NAME, county = CO_NAME, geo_id = GEOID, planning_area = PA_2045)

counties <- st_read(dsn = "../Data/DVRPC_County_Boundaries/DVRPC_County_Boundaries.shp") %>%
  select(county = CO_NAME)

#create subregion layer

pa_counties <- c("Bucks", "Chester", "Delaware", "Montgomery")
nj_counties <- c("Burlington", "Camden", "Gloucester", "Mercer")

pa_sub <- counties %>% filter(county %in% pa_counties) %>%
  st_union() %>% st_sf() %>%
  mutate(subregion = "PA Suburbs")

nj_sub <- counties %>% filter(county %in% nj_counties) %>%
  st_union() %>% st_sf() %>%
  mutate(subregion = "NJ Suburbs")

philly_sub <- counties %>% filter(county =="Philadelphia") %>%
  st_union() %>% st_sf() %>%
  mutate(subregion = "Philadelphia")
  
subregions <- rbind(pa_sub, nj_sub, philly_sub)
rm(pa_counties, nj_counties, pa_sub, nj_sub, philly_sub)

#import preservation data and match CRS to planning areas

pres_2004 <- st_read(dsn = "../Data/ProtectedOpenSpace.gdb", 
                     layer = "DVRPC_ProtectedLands_04") %>%
             st_transform(crs = st_crs(planning_areas)) %>%
             st_make_valid() %>%
             select(space_acres = ACRES, pres_type = TYPE)

pres_2007 <- st_read(dsn = "../Data/ProtectedOpenSpace.gdb", 
                     layer = "DVRPC_ProtectedOpenSpace_2007") %>%
             st_transform(crs = st_crs(planning_areas)) %>%
             st_make_valid() %>%
             select(space_acres = Acres, pres_type = TYPE)

pres_2011 <- st_read(dsn = "../Data/ProtectedOpenSpace.gdb", 
                     layer = "DVRPC_ProtectedOpenSpace_2011") %>%
             st_transform(crs = st_crs(planning_areas)) %>%
             st_make_valid() %>%
             select(space_acres = GIS_Acres, pres_type = OSTYPE)

pres_2016 <- 
  st_read(dsn = "../Data/DVRPC_Protected_Open_Space_2016/DVRPC_Protected_Open_Space_2016.shp") %>%
  st_transform(crs = st_crs(planning_areas)) %>%
  st_make_valid() %>%
  select(space_acres = ACRES, pres_type = OS_TYPE)

#replace preservation type names to match

pres_2004$pres_type %<>% str_replace_all("Land Trust/Private Protected", "Non-profit")
pres_2007$pres_type %<>% str_replace_all("Non-Profit", "Non-profit")
pres_2011$pres_type %<>% str_replace_all("Nonprofit", "Non-profit")
pres_2016$pres_type %<>% str_replace_all("Nonprofit", "Non-profit")

#add public/private designation to preservation types

priv_pres <- c("Non-profit", "Preserved Farmland")

for(i in 1:length(pres_2004$pres_type)){
  pres_2004$pres_class[i] <- ifelse(pres_2004$pres_type[i] %in% priv_pres, 
                                    "Private", "Public")}
for(i in 1:length(pres_2007$pres_type)){
  pres_2007$pres_class[i] <- ifelse(pres_2007$pres_type[i] %in% priv_pres, 
                                    "Private", "Public")}
for(i in 1:length(pres_2011$pres_type)){
  pres_2011$pres_class[i] <- ifelse(pres_2011$pres_type[i] %in% priv_pres, 
                                    "Private", "Public")}
for(i in 1:length(pres_2016$pres_type)){
  pres_2016$pres_class[i] <- ifelse(pres_2016$pres_type[i] %in% priv_pres, 
                                    "Private", "Public")}

rm(i, priv_pres)