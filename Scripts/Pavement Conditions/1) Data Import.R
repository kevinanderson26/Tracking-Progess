#load packages
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "magrittr", "sf", "tidycensus", "readxl", "units", "scales")
pack(packages)
rm(pack, packages)
options(scipen=999)

#import PA summary data, gather all pavement quality variables
pa_pavement_summary <- read_xlsx("../Data/Pennsylvania/PA data summary.xlsx") %>%
  gather("pavement_quality", "miles", -year, -road_type) %>%
  arrange(year, road_type)
  

#import NJ data for each year, add year field, select only needed columns and assign common names

nj_pavement_2010 <- read_xls("../Data/New Jersey/2010 TotDVRPC_DataDump.xls", skip = 12) %>%
  mutate(year = 2010) %>%
  select(year, route = Route, direction = Dir,
         mp_from = 'Mile Post From', mp_to = 'Mile Post To',
         standard_route_id = 'SRI (Standard Route Identifier)',
         route_name = 'Route Name',
         nhs = 'Natl Hwy Sys (Y/N)',
         avg_annual_daily_traffic = AADT,
         int_roughness_index = 'Avg IRI',
         rut_depth = 'Avg Rut',
         surface_distress_index = SDI)

nj_pavement_2011 <- read_xls("../Data/New Jersey/2011 NJDOT Pavement Data.xls", skip = 12) %>%
  select(-SD) %>%
  filter(!is.na(Route)) %>%
  mutate(year = 2011) %>%
  select(year, route = Route, direction = Dir,
         mp_from = 'Mile Post From', mp_to = 'Mile Post To',
         standard_route_id = 'SRI (Standard Route Identifier)',
         route_name = 'Route Name',
         nhs = 'Natl Hwy Sys (Y/N)',
         avg_annual_daily_traffic = AADT,
         int_roughness_index = 'Avg IRI (in/ mi)',
         rut_depth = 'Avg Rut (in)',
         surface_distress_index = SDI)

nj_pavement_2012 <- read_xls("../Data/New Jersey/2012 NJDOT Pavement Data.xls", skip = 12) %>%
  select(-Poor) %>%
  filter(!is.na(Route)) %>%
  mutate(year = 2012) %>%
  select(year, route = Route, direction = Dir,
         mp_from = 'Mile Post From', mp_to = 'Mile Post To',
         standard_route_id = 'SRI (Standard Route Identifier)',
         route_name = 'Route Name',
         nhs = 'Natl Hwy Sys (Y/N)',
         avg_annual_daily_traffic = AADT,
         int_roughness_index = 'Avg IRI (in/ mi)',
         rut_depth = 'Avg Rut (in)',
         surface_distress_index = SDI)

nj_pavement_2013 <- read_xls("../Data/New Jersey/2013 NJDOT Pavement Data.xls", skip = 12) %>%
  select(-Poor) %>%
  filter(!is.na(Route)) %>%
  mutate(year = 2013) %>%
  select(year,route = Route, direction = Dir,
         mp_from = 'Mile Post From', mp_to = 'Mile Post To',
         standard_route_id = 'SRI (Standard Route Identifier)',
         route_name = 'Route Name',
         nhs = 'Natl Hwy Sys (Y/N)',
         avg_annual_daily_traffic = AADT,
         int_roughness_index = 'Avg IRI (in/ mi)',
         rut_depth = 'Avg Rut (in)',
         surface_distress_index = SDI)

nj_pavement_2014 <- read_xlsx("../Data/New Jersey/2014 DVRPC Pavement Data.xlsx") %>%
  mutate(year = 2014) %>%
  select(year, route = Rte, direction = Dir,
         mp_from = 'MPFrom', mp_to = 'MPTo',
         standard_route_id = SRI, 
         route_name = RteName,
         nhs = NHS,
         avg_annual_daily_traffic = AADT, 
         int_roughness_index = AIRI,
         rut_depth = AvgRut, 
         surface_distress_index = SDI)

nj_pavement_2015 <- read_xlsx("../Data/New Jersey/2015 TotDVRPC_DataDump.xlsx", skip = 11) %>%
  mutate(year = 2015) %>%
  select(year,route = Route, direction = Dir,
         mp_from = 'Mile Post From',mp_to = 'Mile Post To',
         standard_route_id = 'SRI (Standard Route Identifier)',
         route_name = 'Route Name',
         nhs = 'Natl Hwy Sys (Y/N)',
         avg_annual_daily_traffic = AADT,
         int_roughness_index = 'Avg IRI (in/ mi)',
         rut_depth = 'Avg Rut (in)',
         surface_distress_index = SDI)

nj_pavement_2016 <- read_xlsx("../Data/New Jersey/2016 DVRPC Pavement Data.xlsx", sheet="2016") %>%
  mutate(year = 2016) %>%
  select(year, route = Rte, direction = Dir,
         mp_from = 'MPFrom', mp_to = 'MPTo',
         standard_route_id = SRI, 
         route_name = RteName,
         nhs = NHS,
         avg_annual_daily_traffic = AADT, 
         int_roughness_index = AIRI,
         rut_depth = AvgRut, 
         surface_distress_index = SDI)

nj_pavement_2017 <- read_xlsx("../Data/New Jersey/2017 DVRPC Pavement Data.xlsx", sheet ="2017") %>%
  mutate(year = 2017) %>%
  select(year, route = Rte, direction = Dir, 
         mp_from = 'MPFrom', mp_to = 'MPTo', 
         standard_route_id = SRI,
         route_name = RteName,
         nhs = NHS,
         avg_annual_daily_traffic = AADT, 
         int_roughness_index = AIRI,
         rut_depth = AvgRut, 
         surface_distress_index = SDI)

#bind individual year new jersey dataframes together and remove individual frames
nj_pavement <- bind_rows(nj_pavement_2010, nj_pavement_2011, nj_pavement_2012, nj_pavement_2013, 
                         nj_pavement_2014, nj_pavement_2015, nj_pavement_2016, nj_pavement_2017)

rm(nj_pavement_2010, nj_pavement_2011, nj_pavement_2012, nj_pavement_2013, 
   nj_pavement_2014, nj_pavement_2015, nj_pavement_2016, nj_pavement_2017)
